
#' Function to save a table from R to Opal
#'
#' @param opal a working opalr::opal_login
#' @param projname Origin opal project name
#' @param tablename Origin opal table name
#'
#' @import opalr dplyr
#' 
#' @export

## Retrieved from write_table_R2opal()
adm.table_save <- function(opal, projname, tablename, datafile, vars, cats = NULL, ...) {
  ## Apply dictionary if variables are present
  if(!missing(vars)) {
    datafile <- dictionary.apply(
      tibble = datafile,
      variables = vars,
      categories = cats
    )
  }
  
  ## Save table to Opal
  opal.table_save(
    opal = opal,
    project = projname,
    table = tablename,
    tibble = datafile,
    ...
  )
}
