
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
