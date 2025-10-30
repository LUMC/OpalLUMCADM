
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
    categories = if (nrow(dict$categories)) dict$categories else NULL
  )
  
  return(datalist)
}


#' Function to set method for force & overwrite
#'
#' @param method String: write, update or overwrite
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
#' @param diffdf Boolean: Should a table_get() be run so a diffdf can be performed on uploaded table
#' @param path String: Path of folder
#' @param max_retries Integer: The number of times R will try to read a datafile from, or write a datafile to, opal.
#'
#' @import opalr dplyr
#' 
#' @export

adm.table_save <- function(opal, projname, tablename, datafile, variables, categories = NULL, method = "write", diffdf = FALSE, path = NULL, max_retries = 3, ...) {
  ## Set method
  method <- adm.set_method(method = method)
  
  ## Set default entitytype
  type <- "Participant"
  
  ## Apply dictionary if variables are present
  if (!missing(variables)) {
    datafile <- dictionary.apply(
      tibble = datafile,
      variables = variables,
      categories = categories
    )
    
    if ("entityType" %in% colnames(variables)) {
      type <- unique(variables$entityType)[1]
    }
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
        type = type,
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
  
  ## Remove user own permissions
  opal.table_perm_delete(
    opal = opal,
    project = projname,
    table = tablename,
    subject = opal$username
  )
  
  ## Run diffdf
  if (diffdf) {
    findings <- .table_save_diffdf(
      opal = opal,
      projname = projname,
      tablename = tablename,
      datafile = datafile, 
      variables = variables,
      categories = categories,
      ...
    )
    
    ## Write to file or return object
    if (!is.null(path)) {
      today <- format(Sys.Date(), format = "%Y%m%d")
      .write_to_excel(
        findings = findings,
        path = paste0(path, "/", tablename, "_", today, ".xlsx")
      )
    } else {
      return(findings)
    }
  }
}


#' Function to copy a table within Opal
#'
#' @param opal_src A working opalr::opal_login as source
#' @param opal_dst A working opalr::opal_login as destination
#' @param project_src Origin opal project name from source Opal
#' @param project_dst Origin opal project name for destination Opal
#' @param tables_src Origin opal table name from source Opal
#' @param tables_dst Origin opal table name for destination Opal
#' @param diffdf Boolean: Should a table_get() be run so a diffdf can be performed on uploaded table
#' @param path String: Path of folder
#'
#' @import opalr dplyr
#' 
#' @export

adm.table_copy <- function(opal_src, opal_dst, project_src, project_dst, tables_src, tables_dst, diffdf = FALSE, path = NULL, ...) {
  if (length(tables_dst) != length(tables_src)) {
    stop("Number of tablenames from source not equal to destination!")
  }
  
  ## Loop through each tablename from source
  for (item in 1:length(tables_src)) {
    ## Get table from Opal
    df <- adm.table_get(
      opal = opal_src,
      projname = project_src,
      tablename = tables_src[item]
    )

    ## Save a copy of table in Opal
    adm.table_save(
      opal = opal_dst,
      projname = project_dst,
      tablename = tables_dst[item],
      datafile = df$datafile,
      variables = df$variables,
      categories = df$categories,
      diffdf = diffdf,
      path = path,
      ...
    )
  }
}
