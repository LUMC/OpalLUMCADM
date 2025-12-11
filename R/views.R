
#' Function to create a view in Opal
#'
#' @param opal a working opalr::opal_login
#' @param project Origin opal project name for view
#' @param table Origin opal table name for view
#' @param source Vector with project and table names: project.table: CNSIM.CNSIM1
#' @param variables variables dataframe
#' @param categories categories dataframe
#' 
#' @import opalr
#' 
#' @export

adm.view_create <- function(opal, project, table, source, variables, categories = NULL, ...) {
  ## Check if view already exists
  view_exists <- opal.table_exists(
    opal = opal,
    project = project,
    table = table
  )
  
  if (!view_exists) {
    ## Create view if it doesn't exist
    opal.table_view_create(
      opal = opal,
      project = project, 
      table = table,
      tables = source,
      ...
    )
  }
  
  ## Set script column as required for views
  variables$script <- paste0("$('", variables$name, "')")
  
  ## Update dictionary (in loop, because of WAF)
  for (x in 1:nrow(variables)) {
    opal.table_dictionary_update(
      opal = opal,
      project = project, 
      table = table,
      variables = variables[x, ],
      categories = categories
    )
  }
    
  ## Remove own user permissions
  opal.table_perm_delete(
    opal = opal,
    project = project,
    table = table,
    subject = opal$username
  )
}
