
#' Function to read a table from opal to R
#'
#' @param opal a working opalr::opal_login
#' @param project Origin opal project name
#' @param table Origin opal table name
#' @param max_retries Integer: The number of times R will try to read a datafile from, or write a datafile to, opal.
#'
#' @import opalr dplyr
#' 
#' @return datalist, list with datafiles & dictionary
#'
#' @export

adm.table_get <- function(opal, project, table, max_retries = 3,...) {
  attempt <- 1
  dict <- NULL
  
  ## Get table from Opal with x number of max_retries
  while (attempt <= max_retries) {
    tryCatch({
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
      
      ## Set categories as NULL if empty
      dict$categories <- if (nrow(dict$categories)) dict$categories else NULL
      
      break
    }, error = function(e) {
      warning(paste("Attempt", attempt, "failed:", e$message))
      attempt <<- attempt + 1
    })
  }
  
  ## Combine output
  datalist <- list(
    datafile = df,
    variables = dict$variables,
    categories = dict$categories
  )
  
  return(datalist)
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
#' @param max_retries Integer: The number of times R will try to read a datafile from, or write a datafile to, opal.
#'
#' @import opalr dplyr
#' 
#' @export

adm.table_save <- function(opal, project, table, datafile, variables = NULL, categories = NULL, method = "write", max_retries = 3, ...) {
  ## Set method
  method <- .set_method(method = method)
  
  ## Set default entitytype
  type <- "Participant"
  
  ## Apply dictionary if variables are present
  if (!is.null(variables)) {
    ## Fix dates & datetimes to character to avoid haven problems
    columns <- sapply(datafile, .map_dtype)
    date_columns <- names(columns)[columns %in% c("date", "datetime")]
    datafile[date_columns] <- lapply(datafile[date_columns], as.character)
    
    ## Apply dictionary
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
        project = project,
        table = table,
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
    project = project,
    table = table,
    subject = opal$username
  )
}


#' Function to save a table from R to Opal with a diffdf check
#'
#' @param opal a working opalr::opal_login
#' @param project Origin opal project name
#' @param table Origin opal table name
#' @param datafile data dataframe
#' @param variables variables dataframe
#' @param categories categories dataframe
#' @param method String: write, update or overwrite
#' @param path String: Path of folder
#' @param max_retries Integer: The number of times R will try to read a datafile from, or write a datafile to, opal.
#'
#' @import opalr dplyr
#' 
#' @export

adm.table_save_diffdf <- function(opal, project, table, datafile, variables = NULL, categories = NULL, method = "write", path = NULL, max_retries = 3, ...) {
  ## Get data from Opal before upload
  datalist1 <- adm.table_get(
    opal = opal,
    project = project,
    table = table,
    ...
  )
  
  ## Save table
  adm.table_save(
    opal = opal,
    project = project,
    table = table,
    datafile = datafile,
    variables = variables,
    categories = categories,
    method = method,
    max_retries = max_retries,
    ...
  )
  
  ## Get data from Opal after upload
  datalist2 <- adm.table_get(
    opal = opal,
    project = project,
    table = table,
    ...
  )
  
  ## Run diffdf
  findings <- adm.complete_diffdf(
    datalist1 = datalist1,
    datalist2 = datalist2
  )
  
  ## Save or return output of diffdf
  if (!is.null(path)) {
    today <- format(Sys.time(), format = "%Y%m%d_%H%m%S")
    .write_to_excel(
      findings = findings,
      path = paste0(path, "/", table, "_", today, ".xlsx")
    )
  } else {
    return(findings)
  }
}


#' Function to copy a table within Opal
#'
#' @param opal_src A working opalr::opal_login as source
#' @param opal_dst A working opalr::opal_login as destination
#' @param project_src Origin opal project name from source Opal
#' @param project_dst Origin opal project name for destination Opal
#' @param table_src Origin opal table name from source Opal
#' @param table_dst Origin opal table name for destination Opal
#' @param method String: write, update or overwrite
#' @param diffdf Boolean: Should a table_get() be run so a diffdf can be performed on uploaded table
#' @param path String: Path of folder
#'
#' @import opalr dplyr
#' 
#' @export

adm.table_copy <- function(opal_src, opal_dst, project_src, project_dst, table_src, table_dst, method = "write", diffdf = FALSE, path = NULL, ...) {
  if (length(table_dst) != length(table_src)) {
    stop("Number of tablenames from source not equal to destination!")
  }
  
  ## Loop through each tablename from source
  for (item in 1:length(table_src)) {
    ## Get table from Opal
    df <- adm.table_get(
      opal = opal_src,
      project = project_src,
      table = table_src[item],
      ...
    )
    
    ## Run save with diffdf or not
    if (isTRUE(diffdf)) {
      adm.table_save_diffdf(
        opal = opal_dst,
        project = project_dst,
        table = table_dst[item],
        datafile = df$datafile,
        variables = df$variables,
        categories = df$categories,
        method = method,
        path = path,
        ...
      )
    } else {
      adm.table_save(
        opal = opal_dst,
        project = project_dst,
        table = table_dst[item],
        datafile = df$datafile,
        variables = df$variables,
        categories = df$categories,
        method = method,
        ...
      )
    }
  }
}
