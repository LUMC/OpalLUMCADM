
#' Function to copy a table within Opal
#'
#' @param opal a working opalr::opal_login
#' @param projname Origin opal project name
#' @param tablename Origin opal table name
#'
#' @import opalr dplyr
#' 
#' @export

## Retrieved from import_copy_table_opal()
adm.table_copy <- function(opal, projname, tablenames, ...) {
  ## Loop through each tablename
  for (name in tablenames) {
    ## Get table from Opal
    df <- adm.table_get(
      opal = opal,
      projname = projname,
      tablename = name
    )

    ## Save a copy of table in Opal
    adm.table_save(
      opal = opal,
      projname = projname,
      tablename = paste0(name, "_COPY"),
      vars = df$dictionary1$variables,
      cats = df$dictionary1$categories,
      ...
    )
  }
}
