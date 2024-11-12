#' @title Function to check whether all datafile variable classes are conform the valueType in the dictionary, if not, datafile class is changed
#'
#' @description We check in the var dictionary whether the specified valueType matches the actual class of the variable in the datafile. If this is not the case, OPAL will guess this automatically possibly without giving you a warning. Some combinations are correct (e.g., R: integer -> Opal: integer), others can be incorrect (e.g., R: character -> Opal: integer) due to various reasons. For certain combinations the class of the datafile is changed, for others only a message is given indicating that there might be a problem that needs to be checked out. When the class is changed, it is possible that some data is lost. A message is given when this is the case, denoting for which variable and for which conversion data loss takes place.
#'
#' @param datafile data.frame. Containing the dataset with for each column the variables described by var. The datafile that will be supplied to opalr::dictionary.apply().
#' @param var data.frame.Containing per row the description of each variable. The dictionary that will be supplied to opalr::dictionary.apply().
#' @param date_time_format character. Specifying the date and/or datetime format used in the datafile. Multiple options are possible, it will then look for the option that best fits the data. Here we can ignore the \-, \/, \% sign and other signs in the notation. Default = c("Ymd", "Ymd HMS")
#'
#' @return A list is returned with two elements. Element 1 is the altered datafile of the same dimensions as the supplied datafile. Element 2 is a matrix containing the remaining problems, i.e., the variables for which a class mismatch is found but no solution is currently supplied or for which there is data loss due to the class conversion. This matrix has a row for each issue and 3 columns:
#' * name: containing the corresponding variables,
#' * issue: the issue with the variable and
#' * info: with information about the issue.
#'
#' @import opalr
#' @import dplyr
#'
#' @author Lars van der Burg
#'
#' @export
datafile_conform_var_change = function(datafile, var, date_time_format = c("Ymd", "Ymd HMS")){


# Checks ------------------------------------------------------------------
  ## Check whether all essential arguments are specified when the function is called
  if(missing(datafile)){stop("Make sure that argument datafile is specified, it's essential")}
  if(missing(var)){stop("Make sure that argument var is specified, it's essential")}


  ## Check if everything is a tibble
  if(isFALSE(is_tibble(datafile))){stop("The datafile is not a tibble, provide datafile, var and cat as tibble.")}
  if(isFALSE(is_tibble(var))){stop("The var dictionary is not a tibble, provide datafile, var and cat as tibble.")}


  dnames = colnames(datafile)
  vnames = var$name

# Initializations ---------------------------------------------------------
  problems = tribble(~check, ~issue, ~info)
  for(i in 1:nrow(var)){
    var_i = var[i, ]
    var_i_name = var_i$name; var_i_vT = var_i$valueType


    if(!(var_i_name %in% dnames)){
      # problems = problems |>
      #   bind_rows(bind_cols(check = var_i_name, issue = "Var absent in datafile", info = NULL))

      next
    }
    datafile_i_class = datafile |> pull(all_of(var_i_name)) |> class()


    if(length(datafile_i_class) > 1){
      problems = problems |>
        bind_rows(bind_cols(check = var_i_name, issue = "Multiple classes", info = paste(datafile_i_class, collapse = "; ")))

      opal_comp_class = c("integer", "numeric", "character", "logical", "Date", "POSIXct")
      if(TRUE %in% (opal_comp_class %in% datafile_i_class)){
        datafile_i_class = datafile_i_class[datafile_i_class %in% opal_comp_class][1]
      } else {
        datafile_i_class = datafile_i_class[1]
      }
      datafile_i_class

      cat("For variable", var_i |> pull(name), "there are multiple classes, continue with the first (opal compatible) class\n")
    }



# Match check -------------------------------------------------------------
    ## In principle datafile_i_class == "character" & valueType == "date" | "datetime" is correct.
    ## But still want to have some check in the code (see below), but no conversion will be applied.
    if((datafile_i_class == "integer" & var_i_vT == "integer") |
       (datafile_i_class == "character" & var_i_vT %in% c("text")) |  # , "datetime", "date"
       (datafile_i_class == "numeric" & var_i_vT == "decimal")){

      next
    }



# Adjustment required ------------------------------------------------------
    ## For each class and valueType combination there will be a separate conversion performed.
    ## To ensure that there is no loss of data, the number of non-NAs before and after conversion is compared

    conversion = paste0(datafile_i_class, "->", var_i_vT)

    tab_before = datafile |> summarize(across(all_of(var_i_name), ~ sum(!is.na(.)))) |> pull()
    tab_after = NULL



## To text (date(time)s) --------------------------------------------------
    ## Do not want to convert these because opal wants to get these as character/text. But check if conversion is okay
    if(datafile_i_class == "character" & var_i_vT %in% c("date", "datetime")){

      datafile_i = datafile |> pull(all_of(var_i_name))

      tab_before = sum(!is.na(datafile_i) & (datafile_i != ""))
      datafile_i_converted = parse_date_time2(datafile_i, date_time_format)
      tab_after = sum(!is.na(datafile_i_converted))



## To text ----------------------------------------------------------------
    ## valueTypes date, datetime and text must to be a character when supplied to opal
    } else if((datafile_i_class == "Date"    & var_i_vT == "date") |
              (datafile_i_class == "POSIXct" & var_i_vT %in% c("date", "datetime")) |
              (datafile_i_class == "logical" & var_i_vT %in% c("text", "date", "datetime")) |
              (datafile_i_class == "numeric" & var_i_vT == "text")){

      datafile = datafile |> mutate_at(vars(var_i_name), as.character)



## To integer --------------------------------------------------------------
    ## valueType integer must be an integer when supplied to opal
    } else if((datafile_i_class == "logical"   & var_i_vT == "integer") |
              (datafile_i_class == "numeric"   & var_i_vT == "integer") |
              (datafile_i_class == "character" & var_i_vT == "integer")){

      datafile = datafile |> mutate_at(vars(var_i_name), as.integer)



# To decimal --------------------------------------------------------------
    ## valueType decimal must be a numeric when supplied to opal
    } else if((datafile_i_class == "logical"   & var_i_vT == "decimal") |
              (datafile_i_class == "character" & var_i_vT == "decimal")){

      datafile = datafile |> mutate_at(vars(var_i_name), as.numeric)



## Factoren ----------------------------------------------------------------
    ## Nothing implemented, what to do?
    } else if(datafile_i_class == "factor"){
      cat("For index", i, "with variable", var_i_name, "the class is a factor, check if this is correct? Nothing implemented for factors\n")

      problems = problems |>
        bind_rows(bind_cols(check = var_i_name, issue = "Factor", info = "Nothing implemented for factors"))

      next



## Unfixed problem -------------------------------------------------------------
    ## To catch other issues, not included above
    } else {
      cat("For index", i, "with variable", var_i_name, "there is nothing implemented yet, CHECK THIS!, conversion is", conversion, "\n")

      problems = problems |>
        bind_rows(bind_cols(check = var_i_name, issue = "Conversion not implemented", info = conversion))

      next
    }



# Documenting issues ------------------------------------------------------
    if(is.null(tab_after)){
      tab_after = datafile |> summarize(across(all_of(var_i_name), ~ sum(!is.na(.)))) |> pull()

    }

    if(!identical(tab_before, tab_after)){
      cat("For variable", var_i_name, "with index", i, "there is loss of data upon conversion", conversion, "\n")

      problems = problems |>
        bind_rows(bind_cols(check = var_i_name, issue = "Data loss", info = conversion))
    }
  }


  if(FALSE %in% (dnames %in% vnames)){
    cat("Be aware, there are variables in the datafile that are not described in the var dictionary\n")
  }
  if(FALSE %in% (vnames %in% dnames)){
    cat("Be aware, there are variables in the var dictionary that are not described in the datafile\n")
  }



  if(nrow(problems) == 0){
    cat("There was no loss of data and no problems found with the data conversion\n")
  }

  return(list(datafile = datafile, problems = problems))
}
