
#' Function to create a view in Opal
#'
#' @param opal a working opalr::opal_login
#' @param view_projname Origin opal project name
#' @param view_tablename Origin opal table name
#' @param source Vector with project and table names: project.table: CNSIM.CNSIM1
#' @param variables variables dataframe
#' @param categories categories dataframe
#' 
#' @import opalr
#' 
#' @export

adm.view_create <- function(opal, view_projname, view_tablename, source, variables, categories = NULL, ...) {
  ## Check if view already exists
  view_exists <- opal.table_exists(
    opal = opal,
    project = view_projname,
    table = view_tablename
  )
  
  if (!view_exists) {
    ## Create view if it doesn't exist
    opal.table_view_create(
      opal = opal,
      project = view_projname, 
      table = view_tablename,
      tables = source,
      ...
    )
    
    ## Update view with data & dictionary
    adm.view_update(
      opal = opal,
      view_projname = view_projname,
      view_tablename = view_tablename,
      source = source,
      variables = variables,
      categories = categories,
      ...
    )
    
    ## Remove own user permissions
    opal.table_perm_delete(
      opal = opal,
      project = view_projname,
      table = view_tablename,
      subject = opal$username
    )
  }
}


#' Function to update a view in Opal
#'
#' @param opal a working opalr::opal_login
#' @param view_projname Origin opal project name
#' @param view_tablename Origin opal table name
#' @param source Vector with project and table names: project.table: CNSIM.CNSIM1
#' @param variables variables dataframe
#' @param categories categories dataframe
#' 
#' @import opalr
#' 
#' @export

adm.view_update <- function(opal, view_projname, view_tablename, source, variables, categories = NULL, ...) {
  ## Set script column as required for views
  variables$script <- paste0("$('", variables$name, "')")
  
  ## Update existing view
  opal.table_view_update(
    opal = opal,
    project = view_projname, 
    table = view_tablename,
    tables = source,
    ...
  )
  
  ## Update dictionary
  opal.table_dictionary_update(
    opal = opal,
    project = view_projname, 
    table = view_tablename,
    variables = variables,
    categories = categories
  )
}
