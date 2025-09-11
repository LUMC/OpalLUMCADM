
#' Function to save a table from R to Opal
#'
#' @param opal a working opalr::opal_login
#' @param project Origin opal project name
#' @param table Origin opal table name
#'
#' @import opalr dplyr
#' 
#' @export

## Retrieved from write_table_R2opal()
adm.table_save <- function(opal, project, table, datafile, variables, categories = NULL, ...) {
  ## Apply dictionary if variables are present
  if (!missing(variables)) {
    datafile <- dictionary.apply(
      tibble = datafile,
      variables = variables,
      categories = categories
    )
  }

  ## Save table to Opal
  opal.table_save(
    opal = opal,
    project = project,
    table = table,
    tibble = datafile,
    ...
  )
}
