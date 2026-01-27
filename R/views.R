
#' Create or Update a View in OPAL with Dictionary and Permissions
#'
#' This function creates or updates a view in an OPAL project based on a source table.
#' It checks if the view exists, creates or updates it, sets the script column,
#' updates the dictionary entries, and removes user permissions.
#'
#' @param opal A connection object to the OPAL server.
#' @param project The name of the project where the view is to be created or updated.
#' @param table The name of the table (view) to be created or updated.
#' @param source The source table(s) used to define the view.
#' @param variables A data frame containing variable definitions (name, type, etc.).
#' @param categories Optional data frame of category mappings for variables.
#' 
#' @return A logical value indicating whether the view was created or updated.
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
    message("Creating new view...")
    ## Create view if it doesn't exist
    opal.table_view_create(
      opal = opal,
      project = project, 
      table = table,
      tables = source,
      ...
    )
  } else {
    message("Updating existing view...")
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
