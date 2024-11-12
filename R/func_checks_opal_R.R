# LUMC ADM standard script to check datafile and dictionary
#
# Date: 2024-03-19
# Author: Lars van der Burg
# Status: in development
#
# Last modified: 2024-09-05
# Last modifications: fix issues with categoriesminmax (to not get it globally) & update repeatable check (TRUE or 1 is not good) & only run cat dict checks when !is.null(cat)
#
#' @title Function that performs some general checks
#'
#' @description Function that performs various checks on the datafile and dictionary, essential if data has to be put into opal. Not all detected issues are an actual problem, that is up to the data manager to sort out. But it is a handy start.
#'
#' @param datafile data.frame. Containing the datafile with for each column the variables described by \code{var}. The datafile that will be supplied to \code{opalr::dictionary.apply()}.
#' @param var data.frame. Containing the variable dictionary for datafile. The dictionary that will be supplied to \code{opalr::dictionary.apply()}.
#' @param cat data.frame. Containing the category dictionary for datafile. The dictionary that will be supplied to \code{opalr::dictionary.apply()}.
#' @param key string. The key of the datafile that will be read into opal.
#' @param min_max boolean. Is there a min/max in the datafile that needs to be checked (FALSE is default).
#' @param silent boolean. Whether or not to silence the printing of a short conclusion for each check (FALSE is default).
#'
#' @details
#' The following checks are executed:
#' 1. if there are duplicated columns.
#' 2. If there are duplicated keys.
#' 3. If there are variables in the var dictionary that are not in the datafile.
#' 4. If there are variables in the cat dictionary that are not in the datafile.
#' 5. If there are variables that are not described in the var dictionary.
#' 6. If there is an entityType in the var dictionary it must be unique, absent is allowed.
#' 7. If all valueTypes are opal-compatible.
#' 8. If all repeatable values are valid
#' 9. If there is a label or description in the var dictionary.
#' 10. If all essential cat columns are present
#' 10.5 If all missing values are valid
#' 11. If there are values with more than decimals, currently not possible with opal
#' 12. If all observed values adhere to the min/max in the var dictionary (optional).
#' 13. If all date and datetimes have a format that is accepted by opal.
#'
#' The last check (13. if the date and datetimes are opal-compatible) is sub-optimal. It is very difficult to get a simple but complete check for all date(times) formats in opal. So interpret this with caution. Issues might be not a problem and no detected issues doesn't mean that everything is perfect.
#'
#' @return A list with a data.frame with the found issues and optionally (if min_max = TRUE) with the issues found with the check_categoriesmin_max function. Additionally, for every check a text is printed indicating whether or not an issue is found.
#'
#' @import opalr dplyr tibble stringr
#' @importFrom lubridate parse_date_time
#'
#' @author Lars van der Burg
#'
#' @export
checks_opal_R <- function(datafile, var, cat = NULL, key = "id", min_max = FALSE, silent = FALSE){


# Initializations ---------------------------------------------------------
  problems = tribble(~check, ~issue, ~info)

  cnames = colnames(datafile)
  cnames_var = colnames(var)
  cnames_cat = colnames(cat)



# Checks ------------------------------------------------------------------
  ## Check whether all essential arguments are specified when the function is called
  if(missing(datafile)){stop("Make sure that argument datafile is specified, it's essential")}
  if(missing(var)){stop("Make sure that argument var is specified, it's essential")}


  ## `name` has to be in var dictionary
  if(!("name" %in% cnames_var)){
    stop("There is no name in the var dictionary, can't run checks without it")
  }


  ## Check if everything is a tibble
  if(isFALSE(is_tibble(datafile))){stop("The datafile is not a tibble, provide datafile, var and cat as tibble")}
  if(isFALSE(is_tibble(var))){stop("The var dictionary is not a tibble, provide datafile, var and cat as tibble")}
  if(!is.null(cat)){
    if(isFALSE(is_tibble(cat))){stop("The cat dictionary is not a tibble, provide datafile, var and cat as tibble")}
  }




# Duplicate columns -------------------------------------------------------
  ## With tidyverse it is not possible to have the same column name. They are then renamed with a ...
  cnames_splt = cnames |> strsplit("\\.\\.\\.") |> lapply(function(x){x[1]}) |> unlist()
  check_dup_cols1 = cnames_splt |> duplicated()

  check_dup_cols2 = str_ends(cnames_splt, "\\.x") | str_ends(cnames_splt, "\\.y")

  check_double = FALSE
  if(TRUE %in% check_dup_cols1){
    problems = problems |>
      bind_rows(bind_cols(check = "Columns", issue = "duplicated", info = cnames[which(check_dup_cols1)]))

    if(isFALSE(silent)){cat("There are some duplicated columns, watch out!\n")}; check_double = TRUE
  }
  if(TRUE %in% check_dup_cols2){
    problems = problems |>
      bind_rows(bind_cols(check = "Columns", issue = "duplicated", info = cnames[which(check_dup_cols2)]))

    if(isFALSE(check_double)){
      if(isFALSE(silent)){cat("There are some duplicated columns, watch out!\n")}  # Simply here for not a double cat statement in output
    }
  } else {
    if(isFALSE(silent)){cat("No duplicated columns\n")}
  }



# Duplicate keys ----------------------------------------------------------
  check_dup_keys = datafile |> select(all_of(key)) |> duplicated()
  # Hieronder een andere optie, maar denk dat bovenstaande net zo goed werkt
  # check_keys <- datafile %>% group_by(id) %>% summarize(double = n()) %>% filter(double > 1)
  if(TRUE %in% check_dup_keys){
    problems = problems |>
      bind_rows(bind_cols(check = "keys", issue = "duplicated", info = paste(check_dup_keys |> sum(), "time(s)")))

    if(isFALSE(silent)){cat(paste0("There are some duplicated keys (keys are: ", paste(key, collapse = ", "), "), watch out!\n"))}
  } else {
    if(isFALSE(silent)){cat("No duplicated keys\n")}
  }


  # Compatible data-dict ----------------------------------------------------
  ## check if var dictionary contains variables that do not occur in the data
  check_var = var$name %in% cnames
  if(FALSE %in% check_var){
    problems = problems |>
      bind_rows(bind_cols(check = "Compatible data-dict", issue = "extra var", info = var$name[!check_var]))

    if(isFALSE(silent)){cat("There are some variables that are described in the var dictionary but are not present in the datafile, watch out!\n")}
  } else {
    if(isFALSE(silent)){cat("No missing datafile columns (var dict)\n")}
  }


  ## check if cat dictionary contains variables that do not occur in the data
  if(!is.null(cat)){
    check_cat = cat$variable %in% cnames
    if(FALSE %in% check_cat){
      problems = problems |>
        bind_rows(bind_cols(check = "Compatible data-dict", issue = "extra cat", info = cat$variable[!check_cat]))

      if(isFALSE(silent)){cat("There are some variables that are described in the cat dictionary but are not present in the datafile, watch out!\n")}
    } else {
      if(isFALSE(silent)){cat("No missing datafile columns (cat dict)\n")}
    }
  } else {
    if(isFALSE(silent)){cat("No cat dictionary supplied, no check\n")}
  }


  ## check if data contains variables that do not occur in the variable dictionary
  check_data = cnames[!(cnames %in% key)] %in% (var |> pull(name))
  if(FALSE %in% check_data){
    problems = problems |>
      bind_rows(bind_cols(check = "Compatible data-dict", issue = "extra data", info = cnames[!(cnames %in% key)][!check_data]))

    if(isFALSE(silent)){cat("There are some variables in the datafile that are not described in the var dictionary, watch out!\n")}
  } else {
    if(isFALSE(silent)){cat("No undescribed datafile columns\n")}
  }



# var dictionary ----------------------------------------------------------
## entityType unique ------------------------------------------------------
  if("entityType" %in% cnames_var){
    check_ent = unique(var[["entityType"]])

    if(length(check_ent) > 1){
      problems = problems |>
        bind_rows(bind_cols(check = "entityType", issue = "Multiple", info = paste(check_ent, collapse = "; ")))

      if(isFALSE(silent)){cat("There are multiple entityTypes in the var dictionary, watch out!\n")}
    } else {
      if(isFALSE(silent)){cat("No duplicated entityTypes\n")}
    }
  } else {
    if(isFALSE(silent)){cat("There is no entityType in the var dictionary, be aware\n")}
  }



## check valueTypes ------------------------------------------------------
  ## valueType checks are performed by datafile_conform_var_check, they can be adjusted (not in this script) by datafile_conform_var_change.R
  if("valueType" %in% cnames_var){
    check_valueType = datafile_conform_var_check(datafile, var, silent = silent)

    if(!is.null(check_valueType)){
      problems = problems |>
        bind_rows(check_valueType)

      if(isFALSE(silent)){cat("There are valueType discrepancies found, watch out!\n")}
    }
  } else {
    problems = problems |>
      bind_rows(bind_cols(check = "valueType", issue = "Missing", info = NA))
  }



## Repeatable -------------------------------------------------------------
  if("repeatable" %in% cnames_var){
    var_repeatable = unique(var$repeatable)

    if(class(var_repeatable) == "logical"){
      if(TRUE %in% var_repeatable){
        problems = problems |>
          bind_rows(bind_cols(check = "repeatable", issue = "present", info = paste(sum(var$repeatable), "variables are repeated")))

          if(isFALSE(silent)){cat("There are variable(s) that are repeatable in the var dictionary, watch out!\n")}
      }
      # if(isFALSE(FALSE %in% (var_repeatable %in% c(FALSE, TRUE)))){
      #   if(isFALSE(silent)){cat("No repeatable issues\n")}
      #
      # } else {
      #   problems = problems |>
      #     bind_rows(bind_cols(check = "repeatable", issue = "invalid values", info = paste(var_repeatable[!(var_repeatable %in% c(FALSE, TRUE))], collapse = ", ")))
      #
      #   if(isFALSE(silent)){cat("There are invalid values for the variable repeatable in the var dictionary, watch out!\n")}
      # }

    } else if(class(var_repeatable) %in% c("integer", "numeric")){
      if(1 %in% var_repeatable){
        problems = problems |>
          bind_rows(bind_cols(check = "repeatable", issue = "present", info = paste(sum(var$repeatable), "variables are repeated")))

        if(isFALSE(silent)){cat("There are variable(s) that are repeatable in the var dictionary, watch out!\n")}
      }
      if(isTRUE(FALSE %in% (var_repeatable %in% c(0, 1)))){
        problems = problems |>
          bind_rows(bind_cols(check = "repeatable", issue = "invalid values", info = paste(var_repeatable[!(var_repeatable %in% c(0, 1))], collapse = ", ")))

        if(isFALSE(silent)){cat("There are invalid values for the variable repeatable in the var dictionary, watch out!\n")}
      }
      # if(isFALSE(FALSE %in% (var_repeatable %in% c(0, 1)))){
      #   if(isFALSE(silent)){cat("No repeatable issues\n")}
      #
      # } else {
      #   problems = problems |>
      #     bind_rows(bind_cols(check = "repeatable", issue = "invalid values", info = paste(var_repeatable[!(var_repeatable %in% c(0, 1))], collapse = ", ")))
      #
      #   if(isFALSE(silent)){cat("There are invalid values for the variable repeatable in the var dictionary, watch out!\n")}
      # }
    } else {
      problems = problems |>
        bind_rows(bind_cols(check = "repeatable", issue = "invalid class", info = ""))

      if(isFALSE(silent)){cat("There is an invalid class for the variable repeatable in the var dictionary, watch out!\n")}
    }

  } else {
    if(isFALSE(silent)){cat("No repeatable in var dict, no check\n")}
  }



## label/description ------------------------------------------------------
  ## Preferably want a label or a description in var dictionary
  if(isFALSE(TRUE %in% (str_detect(cnames_var, "label") | str_detect(cnames_var, "description")))){
    problems = problems |>
      bind_rows(bind_cols(check = "label/description", issue = "missing", info = NA))

    if(isFALSE(silent)){cat("There is no label or description in your var dictionary, watch out!\n")}
  } else {
    if(isFALSE(silent)){cat("label or description detected\n")}
  }



## Encryption -------------------------------------------------------------
  if(isFALSE(TRUE %in% str_detect(cnames_var, "encryption"))){
    problems = problems |>
      bind_rows(bind_cols(check = "encryption", issue = "missing", info = NA))

    if(isFALSE(silent)){cat("There is no encryption variable in your var dictionary, watch out!\n")}
  } else {
    if(isFALSE(silent)){cat("Encryption information detected\n")}
  }



# cat dictionary ----------------------------------------------------------
  if(!is.null(cat)){

## Columns ----------------------------------------------------------------
    cat_cols = c("variable", "name", "missing", "label")
    cat_cols_check1 = c(cat_cols[1:3] %in% cnames_cat, TRUE %in% str_detect(cnames_cat, cat_cols[4]))
    cat_cols_check2 = cnames_cat[!str_detect(cnames_cat, cat_cols[4])] %in% cat_cols[1:3]
    if(FALSE %in% c(cat_cols_check1, cat_cols_check2)){
      if(FALSE %in% cat_cols_check1){
        problems = problems |>
          bind_rows(bind_cols(check = "cat", issue = "absent columns", info = cat_cols[cat_cols_check1 == FALSE]))

        if(isFALSE(silent)){cat("There is an essential column in the cat dictionary missing, watch out!\n")}

      }

      if(FALSE %in% cat_cols_check2){
        problems = problems |>
          bind_rows(bind_cols(check = "cat", issue = "redundant columns", info = cnames_cat[!str_detect(cnames_cat, cat_cols[4])][cat_cols_check2 == FALSE]))

        if(isFALSE(silent)){cat("There is a redundant column in the cat dictionary missing, watch out!\n")}

      }

    } else {
      if(isFALSE(silent)){cat("No cat column issues\n")}
    }



# Missings cat dict ------------------------------------------------------
    if("missing" %in% cnames_cat){
      cat_missing = unique(cat$missing)

      if(class(cat_missing) == "logical"){
        if(isFALSE(FALSE %in% (cat_missing %in% c(FALSE, TRUE)))){
          if(isFALSE(silent)){cat("No missings issues\n")}

        } else {
          problems = problems |>
            bind_rows(bind_cols(check = "missings", issue = "invalid values", info = paste(cat_missing[!(cat_missing %in% c(FALSE, TRUE))], collapse = ", ")))

          if(isFALSE(silent)){cat("There are invalid values for the variable missing in the cat dictionary, watch out!\n")}
        }

      } else if(class(cat_missing) %in% c("integer", "numeric")){
        if(isFALSE(FALSE %in% (cat_missing %in% c(0, 1)))){
          if(isFALSE(silent)){cat("No missings issues\n")}

        } else {
          problems = problems |>
            bind_rows(bind_cols(check = "missings", issue = "invalid values", info = paste(cat_missing[!(cat_missing %in% c(0, 1))], collapse = ", ")))

          if(isFALSE(silent)){cat("There are invalid values for the variable missing in the cat dictionary, watch out!\n")}
        }

      } else {
        problems = problems |>
          bind_rows(bind_cols(check = "missings", issue = "invalid class", info = ""))

        if(isFALSE(silent)){cat("There is an invalid class for the variable missing in the cat dictionary, watch out!\n")}
      }
    } else {
      if(isFALSE(silent)){cat("There is no missing column in the cat dictionary, no check\n")}
    }
  } else {
    if(isFALSE(silent)){cat("No cat dictionary supplied, no checks\n")}
  }






# Four decimals -----------------------------------------------------------
## Temporary check if there are more than four decimals in the data
  decimalplaces <- function(x) {
    x <- sub("0+$", "", x)
    x <- sub("^.+[.]", "", x)
    nchar(x)

    # unlist(lapply(str_split(x, "\\."), function(y){y[2]}))
  }

  check_decimals = FALSE
  for(i in 1:nrow(var)){
    if(var$valueType[i] == "decimal"){
      varname <- var$name[i]
      decm <- decimalplaces(x = datafile[[varname]])

      if(TRUE %in% (decm > 4)){
        problems = problems |>
          bind_rows(bind_cols(check = "decimals", issue = "More than four decimals", info = paste0(varname, ": ", sum(decm > 4, na.rm = TRUE), " observations")))

        check_decimals = TRUE
      }
    }
  }
  if(isTRUE(check_decimals)){
    if(isFALSE(silent)){cat("There are values with more than four decimals, watch out!\n")}
  } else {
    if(isFALSE(silent)){cat("There are no issues with the number of decimals\n")}
  }




# min/max check -----------------------------------------------------------
  ## First check whether the min/max are named as they should
  if(isTRUE(min_max)){

    check_min = which(cnames_var == "min")
    if(length(check_min) == 0){
      problems = problems |>
        bind_rows(bind_cols(check = "min", issue = "absent", info = NA))

      if(isFALSE(silent)){cat("You wanted a minmax check, but there is no column with the name min, watch out!\n")}; #min_max = FALSE
    } else if(length(check_min) > 1){  # Should not be possible
      problems = problems |>
        bind_rows(bind_cols(check = "min", issue = "multiple", info = cnames_var[check_min]))

      if(isFALSE(silent)){cat("You wanted a minmax check, but there are multiple columns with the name min, watch out!\n")}; #min_max = FALSE
    }


    check_max = which(cnames_var == "max")
    if(length(check_min) == 0){
      problems = problems |>
        bind_rows(bind_cols(check = "max", issue = "absent", info = NA))

      if(isFALSE(silent)){cat("You wanted a minmax check, but there is no column with the name max, watch out!\n")}; #min_max = FALSE
    } else if(length(check_min) > 1){  # Should not be possible
      problems = problems |>
        bind_rows(bind_cols(check = "max", issue = "multiple", info = cnames_var[check_min]))

      if(isFALSE(silent)){cat("You wanted a minmax check, but there are multiple columns with the name max, watch out!\n")}; #min_max = FALSE
    }


  ## Second check whether the values are within boundaries
    if(isTRUE(min_max)){
      datafiles = list(data_check = datafile)
      vars = list(data_check = var)
      if(!is.null(cat)){
        cats = list(data_check = cat)
      } else {
        cats = NULL
      }

      report_min_max = check_categoriesminmax_generic(datafiles = datafiles, vars = vars, cats = cats, report_path = NULL, silent = TRUE)
      if(length(report_min_max) == 0){
        if(isFALSE(silent)){cat("No min/max issues\n")}
      } else {
        problems = problems |>
          bind_rows(bind_cols(check = "min-max", issue = "violated", info = "see other report"))

        if(isFALSE(silent)){cat("There are min/max issues found, see separate report for problems encountered, watch out!\n")}
      }
    } else {
      cat("You wanted a minmax check, but I didn't see either a min or max (or both) in your var dictionary, so not done\n")
    }
  }



  # Date/Datetime format ----------------------------------------------------
  ## lubridate::parse_date_times doesn't work, it also parses date(times) with other symbols (e.g., %Y_%m_%d)
  check_date_time_formats = function(vect, vT_vect){
    ## Perform 2 checks, because I can't figure out how to do it with just 1...
    ## A third check is added because difficulties with opal

    if(!is.character(vect)){
      vect = as.character(vect)
    }

    if(vT_vect == "date"){
      formats = c("%Y-%m-%d", "%Y/%m/%d", "%Y.%m.%d", "%Y %m %d", "%d-%m-%Y", "%d/%m/%Y", "%d.%m.%Y", "%d %m %Y")

    } else if(vT_vect == "datetime"){
      formats = c("%Y-%m-%d %H:%M:%OS%z", "%Y-%m-%d %H:%M:%S%z", "%Y-%m-%d %H:%M%z", "%Y-%m-%d %H:%M:%OS%z",
                  "%Y-%m-%d %H:%M:%S", "%Y/%m/%d %H:%M:%S", "%Y.%m.%d %H:%M:%S", "%Y %m %d %H:%M:%S", "%Y-%m-%d %H:%M", "%Y/%m/%d %H:%M", "%Y.%m.%d %H:%M", "%Y %m %d %H:%M")
    }

    NA_before = sum(is.na(vect))

    ## 1. whether correct separation symbols are used
    if(vT_vect == "date"){
      NA_after1 = sum(!(lengths(strsplit(vect, "[-/ .]+")) == 3))
    } else if(vT_vect == "datetime"){
      NA_after1 = sum(!(lengths(strsplit(vect, "[-/ .]+")) >= 4))
    }

    ## 2. whether conversion gives an appropriate date(time)
    NA_after2 = sum(is.na(suppressWarnings({lubridate::parse_date_time(vect, orders = formats)})))

    ## 3. whether a `T` is present (opal says it can handle it, but can't)
    NA_after3 = sum(is.na(vect) | str_detect(vect, "T"))

    return(max(c(NA_after1 - NA_before, NA_after2 - NA_before, NA_after3 - NA_before)))
  }

  NA_diffs = FALSE
  for(i in 1:length(cnames)){
    vT_vect = var |> filter(name == cnames[i]) |> pull(valueType)

    if(length(vT_vect) != 0 && vT_vect %in% c("date", "datetime")){
      NA_diff = check_date_time_formats(vect = datafile[[cnames[i]]], vT_vect = vT_vect)

      if(NA_diff != 0){
        problems = problems |>
          bind_rows(bind_cols(check = "date(time) conversion", issue = "NAs created", info = cnames[i]))

        NA_diffs = TRUE
      }
    }
  }
  if(isTRUE(NA_diffs)){
    if(isFALSE(silent)){cat("There are date or datetime formats detected which might not be accepted by opal, watch out!\n")}
  } else {
    if(isFALSE(silent)){cat("No incompatbile date(time) formats, but be aware, check is sub-optimal...\n")}
  }

  # vector_dates = c(NA, "2024-01-01", "2024/01/01", "2024.01.01", "2024 01 01", "2024_01_01",
  #                      "01-01-2024", "01/01/2024", "01.01.2024", "01 01 2024", "01*01*2024")
  #
  # vector_datetimes = c(NA, "2024-01-01T12:34:56.43 UTC+5", "2024-01-01T12:34:56.789Z",
  #                          "2024-01-01T12:34:56 UTC+5", "2024-01-01T12:34:56Z",
  #                          "2024-01-01T12:34 UTC+5", "2024-01-01T12:34Z",
  #                          "2024-01-01T12:34:56.000000",
  #
  #                          "2024-01-01 12:34:56", "2024/01/01 12:34:56", "2024.01.01 12:34:56", "2024 01 01 12:34:56",
  #                          "2024-01-01 12:34", "2024/01/01 12:34", "2024.01.01 12:34", "2024 01 01 12:34")
  #
  # check_date_time_formats(vect = vector_dates, vT_vect = "date")
  # check_date_time_formats(vect = vector_datetimes, vT_vect = "datetime")
  # check_date_time_formats(vect = c(vector_dates, vector_datetimes), vT_vect = "datetime")



  out = list(problems = problems)
  if(min_max){
    out$report_min_max = report_min_max$data_check
  }

  return(out)
}
