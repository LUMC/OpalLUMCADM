
#' Function to check run all checks
#'
#' @param datafile a working opalr::opal_login
#' @param variables Origin opal project name
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
#' @param datafile a working opalr::opal_login
#' @param variables Origin opal project name
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
#' @param datafile a working opalr::opal_login
#' @param variables Origin opal project name
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
