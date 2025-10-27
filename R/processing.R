
#' Function to capture all warnings & errors when running all check functions
#'
#' @param function_name Function to run & catch logs from
#' 
#' @return logs Return list with warnings & errors
#' 
#' @export

.capture_logs <- function(function_name) {
  func_name <- deparse(substitute(function_name))
  
  function(...) {
    logs <- list()
    
    ## Add warnings & errors to logs list
    add_log <- function(type, message) {
      logs[[length(logs) + 1]] <<- list(
        function_name = func_name,
        type = type,
        message = message
      )
    }
    
    ## Run function and catch errors & warnings
    withCallingHandlers(
      tryCatch(
        function_name(...),
        error = function(e) {
          add_log("ERROR", conditionMessage(e))
        }
      ),
      warning = function(w) {
        add_log("WARNING", conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    
    ## Set ok if logs are empty
    if (!length(logs)) (
      add_log("OK", "No issues found")
    )
    return(logs)
  }
}


#' Function to set class type based on data in column
#'
#' @param column Data column
#' 
#' @export

.map_dtype <- function(column) {
  ## Type translation to Opal
  map_valuetype <- c(
    "integer"   = "integer",
    "numeric"   = "decimal",
    "double"    = "decimal",
    "character" = "text",
    "logical"   = "boolean",
    "POSIXct"   = "datetime",
    "POSIXt"    = "datetime",
    "Date"      = "date"
  )
  
  ## Set class & translate to Opal class type
  column_class <- tail(class(column), n = 1)
  unname(map_valuetype[column_class])
}


#' Function to set the correct valuetype of data vs variables
#'
#' @param datafile data input
#' @param variables variables dataframe
#' 
#' @return variables Return variables with corrected valuetypes
#' 
#' @export

adm.fix_valuetype <- function(datafile, variables) {
  ## Get valuetypes
  valuetypes_data <- sapply(datafile, .map_dtype)
  valuetypes_vars <- setNames(variables$valueType, variables$name)
  
  ## Compare valuetypes
  for (column in names(valuetypes_vars)) {
    compare <- identical(valuetypes_vars[column], valuetypes_data[column])
    if (!compare) {
      message("Changed valuetype of `", column, "` from ", valuetypes_vars[column], " to ", valuetypes_data[column])
      variables$valueType[variables$name == column] <- valuetypes_data[[column]]
    }
  }
  
  return(variables)
}


#' Function to save a table from R to Opal + a table_get() + diffdf
#'
#' @param opal a working opalr::opal_login
#' @param projname Origin opal project name
#' @param tablename Origin opal table name
#' @param datafile data dataframe
#' @param variables variables dataframe
#' @param categories categories dataframe
#' 
#' @import opalr
#' 
#' @export

.table_save_diffdf <- function(opal, projname, tablename, datafile, variables, categories = NULL, ...) {
  ## Get table
  df <- adm.table_get(
    opal = opal,
    projname = projname,
    tablename = tablename,
    ...
  )
  
  ## Create diffdf output list
  findings <- list()
  
  ## Run diffdf for datafile
  findings$datafile <- adm.check_diffdf(
    datafile1 = datafile,
    datafile2 = df$datafile,
    keys = colnames(df$datafile)[1],
    ...
  )
  
  ## Run diffdf for variables
  findings$variables <- adm.check_diffdf(
    datafile1 = variables,
    datafile2 = df$variables,
    keys = "name",
    ...
  )
  
  ## Run diffdf for categories
  if (!is.null(categories)) {
    findings$categories <- adm.check_diffdf(
      datafile1 = categories,
      datafile2 = df$categories,
      keys = c("variable", "name"),
      ...
    )
  }
  
  return(findings)
}
