
#' Function to read a table from opal to R
#'
#' @param opal a working opalr::opal_login
#' @param project Origin opal project name
#' @param tablename Origin opal table name
#'
#' @import opalr dplyr
#' 
#' @return 
#'
#' @export

## Retrieved from import_table_opal2R()
adm.table_get <- function(opal, project, table, ...) {
  ## Get table from Opal
  df <- opal.table_get(
    opal = opal,
    project = project,
    table = table,
    ...
  )

  ## Get dictionary from Opal
  dict <- opal.table_dictionary_get(
    opal = opal,
    project = project,
    table = table
  )

  ## Combine output
  datalist <- list(
    datafile1 = df,
    datafile2 = df,
    dictionary1 = dict,
    dictionary2 = dict
  )

  return(datalist)
}
