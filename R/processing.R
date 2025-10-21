
#' Function to capture all warnings & errors when running all check functions
#'
#' @param run_function Function to run
#' 
#' @return logs Return list with warnings & errors
#' 
#' @export

.capture_logs <- function(run_function) {
  func_name <- deparse(substitute(run_function))
  
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
        run_function(...),
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
