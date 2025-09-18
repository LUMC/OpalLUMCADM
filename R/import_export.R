
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


#' Function to set force & overwrite
#'
#' @param method String: write, update or overwrite
#'
#' @import opalr dplyr
#' 
#' @return 
#'
#' @export

## Retrieved from import_table_opal2R()
adm.set_method <- function(method) {
  ## Set method
  if(method == "write"){
    method = c(force = FALSE, overwrite = FALSE)
  } else if(method == "update"){
    method = c(force = TRUE, overwrite = FALSE)
  } else if(method == "overwrite"){
    method = c(force = TRUE, overwrite = TRUE)
  } else {
    stop("The method argument can only have three options: write, update or overwrite.")
  }
  
  return(method)
}


#' Function to save a table from R to Opal
#'
#' @param opal a working opalr::opal_login
#' @param project Origin opal project name
#' @param table Origin opal table name
#' @param datafile data dataframe
#' @param variables variables dataframe
#' @param categories categories dataframe
#' @param method String: write, update or overwrite
#'
#' @import opalr dplyr
#' 
#' @export

## Retrieved from write_table_R2opal()
adm.table_save <- function(opal, project, table, datafile, variables, categories = NULL, method,...) {
  ## Set method
  method <- adm.set_method(method = method)
  
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
    force = method["force"],
    overwrite = method["overwrite"],
    ...
  )
  
  ## Remove user own permissions (if you can make a table, you have to have project rights)
  opal.table_perm_delete(
    opal = opal,
    project = projname,
    table = tablename,
    subject = opal$username
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
