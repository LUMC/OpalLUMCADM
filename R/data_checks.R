
#' Function to check run all checks
#'
#' @param datafile data input
#' @param variables variables dataframe
#'
#' @import opalr rlang
#' 
#' @export

adm.run_all_checks <- function(datafile, variables) {
  ## Check columns
  adm.check_columns(
    datafile = datafile,
    variables = variables
  )
  
  ## Check valuetypes
  adm.check_valuetype(
    datafile = datafile,
    variables = variables
  )
}


#' Function to check if all columns listed in variables are also in the dataset and vice-versa
#'
#' @param datafile data input
#' @param variables variables dataframe
#'
#' @import opalr rlang
#' 
#' @export

adm.check_columns <- function(datafile, variables) {
  ## Get columns
  columns_data <- colnames(datafile)
  columns_vars <- variables$name
  
  ## Get differences
  column_diff <- setdiff(columns_data, columns_variables)
  
  ## Show differences
  if (is_empty(column_diff)) {
    message("Columns match with variables")
  } else {
    for (column in column_diff) {
      warning(
        "Column '", column,
        "' in data: ", column %in% columns_data,
        " in variables: ", column %in% columns_vars
      )
    }
  }
}


#' Function to check valuetypes of data vs variables
#'
#' @param datafile data input
#' @param variables variables dataframe
#'
#' @import opalr
#' 
#' @export

## Retrieved from datafile_conform_var_change()
adm.check_valuetype <- function(datafile, variables) {
  ## Type translation
  translation_opal <- c(
    integer = "integer",
    decimal = "numeric",
    text = "character",
    boolean = "logical",
    datetime = "POSIXct",
    date = "POSIXct"
  )
  
  ## Get valuetypes
  valuetypes_data <- sapply(datafile, function(x) tail(class(x), n = 1))
  valuetypes_vars <- setNames(translation_opal[variables$valueType], variables$name)
  
  ## Compare valuetypes
  for (column in names(valuetypes_vars)) {
    compare <- identical(valuetypes_vars[column], valuetypes_data[column])
    if (!compare) {
      warning(
        "ValueType of ", column, " doesn't match: ",
        valuetypes_vars[column], " vs ", valuetypes_data[column]
      )
    }
  }
  message("All valueTypes checked")
}


#' Function to find differences in data
#'
#' @param dataframe1 dataframe
#' @param dataframe2 dataframe
#' @param path path to output dir
#' 
#' @import diffdf 
#' 
#' @export

## Retrieved from check_diffdf_opal_generic()
adm.check_diffdf <- function(dataframe1, dataframe2, path = NA, ...) {
  ## Get differences
  difference <- diffdf(
    base = dataframe1,
    compare = dataframe2
  )
  
  ## Merge all vardiffs
  difference[["VarDiff"]] <- do.call(rbind, difference[grep("VarDiff_", names(difference))])
  difference[grep("VarDiff_", names(difference))] <- NULL
  
  ## Return difference if no path is set
  if (is.na(path)) {
    return(difference)
  }
  
  ## Write to Excel
  adm.write_to_excel(
    df_list = difference,
    path = path,
    ...
  )
}


#' Function to write diffdf to excel
#'
#' @param df_list list of dataframes from diffdf
#' @param path path to output dir
#' 
#' @import openxlsx
#' 
#' @export

adm.write_to_excel <- function(df_list, path, ...) {
  ## Create a new workbook
  wb <- createWorkbook()
  
  ## Add each dataframe as a sheet
  for (sheet_name in names(df_list)) {
    addWorksheet(wb, sheet_name)
    writeData(wb, sheet = sheet_name, df_list[[sheet_name]])
  }
  
  ## Save workbook
  saveWorkbook(
    wb = wb,
    file = paste0(path, format(Sys.Date(), "%Y%m%d"), ".xlsx"),
    ...
  )
}
