
#' Function to create a view in Opal
#'
#' @param opal a working opalr::opal_login
#' @param projname_view Origin opal project name
#' @param tablename_view Origin opal table name
#' @param source Vector with project and table names: project.table: CNSIM.CNSIM1
#' @param variables variables dataframe
#' @param categories categories dataframe
#' 
#' @import opalr
#' 
#' @export

adm.view_create <- function(opal, projname_view, tablename_view, source, variables, categories = NULL, ...) {
  ## Check if view already exists
  view_exists <- opal.table_exists(
    opal = opal,
    project = projname_view,
    table = tablename_view
  )
  
  if (!view_exists) {
    ## Create view if it doesn't exist
    opal.table_view_create(
      opal = opal,
      project = projname_view, 
      table = tablename_view,
      tables = source,
      ...
    )
    
    ## Update view with data & dictionary
    adm.view_update(
      opal = opal,
      projname_view = projname_view,
      tablename_view = tablename_view,
      source = source,
      variables = variables,
      categories = categories,
      ...
    )
    
    ## Remove own user permissions
    opal.table_perm_delete(
      opal = opal,
      project = projname_view,
      table = tablename_view,
      subject = opal$username
    )
  }
}


#' Function to update a view in Opal
#'
#' @param opal a working opalr::opal_login
#' @param projname_view Origin opal project name
#' @param tablename_view Origin opal table name
#' @param source Vector with project and table names: project.table: CNSIM.CNSIM1
#' @param variables variables dataframe
#' @param categories categories dataframe
#' 
#' @import opalr
#' 
#' @export

adm.view_update <- function(opal, projname_view, tablename_view, source, variables, categories = NULL, ...) {
  ## Set script column as required for views
  variables$script <- paste0("$('", variables$name, "')")
  
  ## Update existing view
  opal.table_view_update(
    opal = opal,
    project = projname_view, 
    table = tablename_view,
    tables = source,
    ...
  )
  
  ## Update dictionary
  opal.table_dictionary_update(
    opal = opal,
    project = projname_view, 
    table = tablename_view,
    variables = variables,
    categories = categories
  )
}
