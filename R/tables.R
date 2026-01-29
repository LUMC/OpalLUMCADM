
#' Get table data and metadata from Opal
#'
#' Retrieves a table from an Opal server along with its variable and category dictionary.
#' Supports retry logic in case of connection or network failures.
#'
#' @param opal A connection object to the Opal server.
#' @param project The name of the project containing the table.
#' @param table The name of the table to retrieve.
#' @param max_retries Integer specifying the maximum number of retry attempts (default: 3).
#'
#' @return A list containing:
#'   \itemize{
#'     \item \code{datafile}: The data as a tibble.
#'     \item \code{variables}: The table's variable dictionary.
#'     \item \code{categories}: The table's category dictionary (NULL if empty).
#'   }
#'
#' @import opalr dplyr
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
      
      ## Remove na_values (issue when applying new categories with missing)
      dict$variables$na_values <- NULL
      
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


#' Save a table to Opal with optional dictionary application and retry logic
#'
#' Saves a tibble to an Opal table, applying variable and category dictionaries if provided.
#' Includes retry mechanism for failed saves and removes user permissions afterward.
#'
#' @param opal A connection object to the Opal server.
#' @param project The name of the project containing the table.
#' @param table The name of the table to save.
#' @param datafile A tibble containing the data to save.
#' @param variables A tibble of variable definitions (optional).
#' @param categories A tibble of category definitions (optional).
#' @param method Character specifying save method ("write", "overwrite", etc.) (default: "write").
#' @param max_retries Integer specifying the maximum number of retry attempts (default: 3).
#'
#' @return A list containing the saved data and metadata (not directly returned; operation is side-effect).
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


#' Save a table with diff comparison before and after upload
#'
#' Retrieves data from Opal before and after upload, compares the two datasets using `adm.complete_diffdf`,
#' and optionally saves the differences to an Excel file.
#'
#' @param opal A connection object to the Opal server.
#' @param project The name of the project containing the table.
#' @param table The name of the table to save.
#' @param datafile A tibble containing the data to save.
#' @param variables A tibble of variable definitions (optional).
#' @param categories A tibble of category definitions (optional).
#' @param method Character specifying save method ("write", "overwrite", etc.) (default: "write").
#' @param path Character path to save diff findings as Excel file (optional).
#' @param max_retries Integer specifying the maximum number of retry attempts (default: 3).
#'
#' @return If \code{path} is provided, returns silently; otherwise, returns a list of differences.
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


#' Copy a table from one Opal instance to another
#'
#' Copies tables from a source Opal project to a destination project.
#' Optionally performs a diff check after saving.
#'
#' @param opal_src A connection object to the source Opal server.
#' @param opal_dst A connection object to the destination Opal server.
#' @param project_src The name of the source project.
#' @param project_dst The name of the destination project.
#' @param table_src A character vector of source table names.
#' @param table_dst A character vector of destination table names.
#' @param method Character specifying save method ("write", "overwrite", etc.) (default: "write").
#' @param diffdf Logical indicating whether to run a diff check after save (default: FALSE).
#' @param path Character path to save diff findings as Excel file (optional).
#'
#' @return A list of copied tables (no direct return; operation is side-effect).
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
