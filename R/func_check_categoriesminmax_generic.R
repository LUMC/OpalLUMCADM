#' @title Check if values in the datafile are conform the min and max in the var dictionary
#'
#' @description In the var dictionary it is possible to supply a min and a max, to give the boundaries of the values. However, it is still possible that values are observed that are outside this boundary and it can be important to check if this is the case.
#'
#' @param datafiles list. A list containing for each element a datafile
#' @param vars list. A list containing for each element a var dictionary, that matches the datafile from datafiles with the same element
#' @param cats an optional list. A list containing for each element a cat dictionary, that matches the datafile from datafiles with the same element. If NULL, than only the min/max can be checked and not the categories.
#' @param key string. The key of the datafile that will be read into opal.
#' @param report_path string. Folder where the report needs to be saved. If \code{NULL} the report is not saved but only returned.
#' @param silent boolean. Whether or not to silence the printing of messages while running the function (FALSE is default).
#' @param conditional an optional list. In each list element for a specific variable an extra filter statement can be included. Should be constructed as: `variable_name ~ statement`, where the variable name must match a variable in the dataset and the statement should be an evaluable statement by the dplyr::filter function.
#' @param includeNA boolean. Whether or not to include the NAs from the min_max report
#'
#' @return a list with an element for each supplied datafile. Per list element there can be two separate list elements containing matrices with the findings. Only present if there are non-zero findings:
#' * min_max A matrix with 5 columns and rows equal to the number of findings. Columns are `{key}`, `variable name`, `value`, `min` and `max`. Value is the observed value that is outside the min/max range
#' * outside_cat A matrix with 4 column and rows equal to the number of findings. Columns are `{key}`, `variable name`, `value` and `categories`. Value is the observed value, categories the current known labels
#'
#' @note
#' Cannot be used reliably when the variables have labels, so when a table is retrieved from opal
#'
#' @import dplyr
#' @importFrom openxlsx write.xlsx
#' @importFrom rlang parse_expr
#'
#' @author Lars van der Burg & Thekla Jansen
check_categoriesminmax_generic <- function(datafiles, vars, cats = NULL, key = "id", report_path = NULL, report_name = "Report", silent = FALSE, conditional = NULL, includeNA = FALSE){


# Checks ------------------------------------------------------------------
  ## Check whether all essential arguments are specified when the function is called
  if(missing(datafiles)){stop("Make sure that argument datafiles is specified, it's essential")}
  if(missing(vars)){stop("Make sure that argument vars is specified, it's essential")}


  ## Check whether everything can be compared
  if(is.null(names(datafiles))){
    stop("You should name each list element with the name of the datafile")
  } else {names_datafiles = names(datafiles)}
  if(is.null(names(vars))){
    stop("You should name each list element with the name of the var dictionary")
  } else {names_vars = names(vars)}
  if(!is.null(cats) && is.null(names(cats))){
    stop("You should name each list element with the name of the cat dictionary")
  } else {names_cats = names(cats)}


  if(!identical(names_datafiles, names_vars)){
    stop("The names of datafile and var dictionary should be identical")
  }
  if(!is.null(cats)){
    if(FALSE %in% (names_cats %in% names_datafiles)){
      stop("All cat dictionary names should be present in the names of datafile")
    }
  }


## Check if everything is a tibble
  for(i in 1:length(names_datafiles)){
    if(isFALSE(is_tibble(datafiles[[i]]))){stop("Not all datafiles are a tibble, provide datafiles, vars and cats as a list of tibble.")}
    if(isFALSE(is_tibble(vars[[i]]))){stop("Not all var dictionaries are a tibble, provide datafiles, vars and cats as a list of tibble.")}
    if(!is.null(cats)){
      if(isFALSE(is_tibble(cats[[i]]))){stop("Not all cat dictionaries are a tibble, provide datafiles, vars and cats as a list of tibble.")}
    }
  }


## Conditional
  if(!is.null(conditional)){
    if(length(datafiles) > 1){stop("Conditional is currently only implemented for single datasets, not multiple")}

    conditional_l = lapply(conditional, function(x){str_trim(unlist(str_split(x, "~")), side = "both")})
    condit = tibble("var" = map_vec(conditional_l, 1), "con" = map_vec(conditional_l, 2))

    if(FALSE %in% (condit$var %in% vars[[1]]$name)){stop("The variable in the conditional statement is not present in the var dictionary, please rewrite")}
    if(isTRUE(TRUE %in% duplicated(condit$var))){stop("There are duplicates in the conditional variables, please rewrite into a single statement per variable")}

    if(FALSE %in% sapply(1:nrow(condit), function(x){tryCatch({datafiles[[1]] |> filter(!!rlang::parse_expr(condit[x, 2] |> pull()))
                                                     TRUE}, error = function(e){FALSE})})){
      stop("Not all conditions are evaluable by filter. Please rewrite them correctly.")
    }
  } else {
    condit = NULL
  }


# Running check -----------------------------------------------------------
  checkreport <- list()
  for(table_name in names(datafiles)){

    ## fetch type, categories (values), min, max, missing values
    for(variable_name in names(datafiles[[table_name]])){

      if(variable_name == key | !(variable_name %in% (vars[[table_name]] |> pull(name)))){
        next
      }
      if(isFALSE(silent)){cat(table_name, "with", variable_name, "\n")}


      varinfo <- list()
      xtype <- vars[[table_name]] %>% filter(name == variable_name) %>% select(valueType) %>% unlist()

      if("min" %in% colnames(vars[[table_name]])){
        varinfo[["xmin"]] <- vars[[table_name]] %>% filter(name == variable_name) %>% select(min) %>% unlist()
      } else {varinfo[["xmin"]] <- NA}
      if("max" %in% colnames(vars[[table_name]])){
        varinfo[["xmax"]] <- vars[[table_name]] %>% filter(name == variable_name) %>% select(max) %>% unlist()
      } else {varinfo[["xmax"]] <- NA}

      if(!is.null(cats[[table_name]])){
        varinfo[["xcat"]] <- cats[[table_name]] %>% filter(variable == variable_name) %>% select(name) %>% unlist()
        varinfo[["xmis"]] <- cats[[table_name]] %>% filter(variable == variable_name) %>% mutate(missing = as.numeric(missing)) %>%
          filter(missing == 1) %>% select(name) %>% unlist()
      } else {varinfo[["xcat"]] <- varinfo[["xmis"]] <- NA}

      ## change all empty strings/null/NULL values into NA
      varinfo[varinfo == "" | varinfo == "null" | varinfo == "NULL" | (lengths(varinfo) == 0)] <- NA

      ## convert values/dates to correct type
      if(xtype == "decimal" | xtype == "integer"){varinfo <- lapply(varinfo, as.numeric)
      } else if(xtype == "date")                 {varinfo <- lapply(varinfo, as.Date)
      } else if(xtype == "datetime")             {varinfo <- lapply(varinfo, parse_date_time, c("Ymd HMS", "Ymd HM", "Ymd"))}

      ## only check non-text variables and text variables with categories
      if(xtype != "text" | (xtype == "text" & length(varinfo[["xcat"]]) > 0)){

        ## current variable data
        tempdata = datafiles[[table_name]]
        if(!is.null(condit) && variable_name %in% condit$var){
          tempdata = tempdata |>
            filter(!!rlang::parse_expr(condit |> filter(var == variable_name) |> pull(con)))
        }
        tempdata <- tempdata %>%
          select(all_of(c(key, variable_name))) |> rename(current_var = all_of(variable_name))

        ## if datetime: apply as.POSIXct to data: to remove ending zeros
        if(xtype == "datetime"){
          tempdata <- tempdata %>% mutate(current_var = parse_date_time(current_var, c("Ymd HMS", "Ymd HM","Ymd")))
        }

        ## check for values outside of min/max in data for current variable
        if(isFALSE(includeNA)){
          tempdataout <- tempdata %>%
            filter(current_var < varinfo[["xmin"]] | current_var > varinfo[["xmax"]]) %>%
            filter(!(current_var %in% varinfo[["xmis"]]))  ## but do not include missings
        } else if(isTRUE(includeNA)){
          tempdataout <- tempdata %>%
            filter(current_var < varinfo[["xmin"]] | current_var > varinfo[["xmax"]] | is.na(current_var))
        }

        ## add to report if applicable
        if(nrow(tempdataout) > 0){
          tempdataout <- tempdataout %>%
            mutate(`variable name` = variable_name) %>%
            relocate(current_var, .after = last_col()) %>%
            mutate(min = varinfo[["xmin"]], max = varinfo[["xmax"]]) %>%
            rename(`value` = current_var) %>%
            mutate_all(as.character)

          checkreport[[table_name]][["min_max"]] <- checkreport[[table_name]][["min_max"]] %>% bind_rows(tempdataout)
        }

        ## check for values outside of categories: only if categories exist other than missing (NB: this might still be a valid case sometimes)
        if(!identical(varinfo[["xcat"]], varinfo[["xmis"]])){
          tempdatacat <- tempdata %>%
            filter(!(current_var %in% varinfo[["xcat"]]) & !is.na(current_var))  ## check for non-empty values that are not in categories

          ## add to report if applicable
          if(nrow(tempdatacat) > 0){
            tempdatacat <- tempdatacat %>%
              mutate(`variable name` = variable_name) %>%
              relocate(current_var, .after = last_col()) %>%
              mutate (categories = paste(varinfo[["xcat"]], collapse = ";")) %>%
              rename(`value` = current_var) %>%
              mutate_all(as.character)

            checkreport[[table_name]][["outside_cat"]] <- checkreport[[table_name]][["outside_cat"]] %>% bind_rows(tempdatacat)
          }
        }
      }
    }

    if(isFALSE(silent)){cat("\n")}
  }


  # Export report -----------------------------------------------------------
  if(!is.null(report_path)){
    for(Datafile in names(checkreport)){
      for(i in names(checkreport[[Datafile]])){

        if(nrow(checkreport[[Datafile]][[i]]) > 0) {
          cat(Datafile, i, "- Number of rows:", nrow(checkreport[[Datafile]][[i]]), "\n")

          path <- paste0(report_path, report_name, "_", Datafile, ".xlsx")
          openxlsx::write.xlsx(checkreport[[Datafile]], file = path)
        }
      }
    }
  }

  return(checkreport)
}
