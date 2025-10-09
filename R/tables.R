
#' Function to read a table from opal to R
#'
#' @param opal a working opalr::opal_login
#' @param projname Origin opal project name
#' @param tablename Origin opal table name
#'
#' @import opalr dplyr
#' 
#' @return datalist, list with datafiles & dictionary
#'
#' @export

adm.table_get <- function(opal, projname, tablename, ...) {
  ## Get table from Opal
  df <- opal.table_get(
    opal = opal,
    project = projname,
    table = tablename,
    ...
  )
  
  ## Get dictionary from Opal
  dict <- opal.table_dictionary_get(
    opal = opal,
    project = projname,
    table = tablename
  )
  
  ## Combine output
  datalist <- list(
    datafile = df,
    variables = dict$variables,
    categories = dict$categories
  )
  
  return(datalist)
}


#' Function to set method for force & overwrite
#'
#' @param method String: write, update or overwrite
#'
#' @import opalr dplyr
#' 
#' @return method, vector with settings for force & overwrite
#'
#' @export

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
#' @param projname Origin opal project name
#' @param tablename Origin opal table name
#' @param datafile data dataframe
#' @param variables variables dataframe
#' @param categories categories dataframe
#' @param method String: write, update or overwrite
#' @param max_retries Integer: The number of times R will try to read a datafile from, or write a datafile to, opal.
#'
#' @import opalr dplyr
#' 
#' @export

adm.table_save <- function(opal, projname, tablename, datafile, variables, categories = NULL, method = "write", max_retries = 3, ...) {
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
  
  ## Save table to Opal with x number of max_retries
  attempt <- 1
  while (attempt <= max_retries) {
    tryCatch({
      opal.table_save(
        opal = opal,
        project = projname,
        table = tablename,
        tibble = datafile,
        force = method["force"],
        overwrite = method["overwrite"],
        ...
      )
      break
    }, error = function(e) {
      warning(paste("Attempt", attempt, "failed:", e$message))
      attempt <<- attempt + 1
    })
  }
  
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
