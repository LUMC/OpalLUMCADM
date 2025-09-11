
#' Function to check if all columns listed in variables are also in the dataset and vice-versa
#'
#' @param datafile a working opalr::opal_login
#' @param variables Origin opal project name
#'
#' @import opalr rlang
#' 
#' @export

adm.check_columns <- function(datafile, variables, ...) {
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
