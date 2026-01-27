
#' Capture function execution logs including warnings and errors
#'
#' This function wraps any given function and captures all warnings and errors
#' during its execution. It logs the type and message of each warning or error,
#' and returns a list of logs. If no issues are found, it logs "OK".
#'
#' @param function_name A function to be wrapped and monitored
#'
#' @return A list containing log entries with details about errors, warnings, or success
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


#' Map R data types to Opal data types
#'
#' Translates R data types (e.g., integer, character) to corresponding Opal data types
#' (e.g., integer, text, decimal, boolean, datetime, date).
#'
#' @param column A single R column object (e.g., vector)
#'
#' @return A string representing the Opal data type corresponding to the input column class
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


#' Fix data variable value types to match Opal expectations
#'
#' Compares the expected value types from a variables list with the actual types
#' in the data file. If mismatches are found, updates the value type in the variables
#' list and prints a message.
#'
#' @param datafile A data frame or list containing the data to be checked
#' @param variables A data frame or list with 'name' and 'valueType' columns
#'
#' @return A modified version of the variables list with corrected value types
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


#' Set method parameters for write operations
#'
#' Translates a method string ("write", "update", "overwrite") into a list of
#' force and overwrite flags. Returns a consistent method parameter list.
#'
#' @param method A character string specifying the operation method: "write", "update", or "overwrite"
#'
#' @return A list with 'force' and 'overwrite' flags based on the input method
#'
#' @export

.set_method <- function(method) {
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


#' Write findings to an Excel workbook
#'
#' Creates an Excel workbook and writes each findings object (datafile, variables, categories)
#' into separate worksheets. If no issues are found, a placeholder message is written.
#'
#' @param findings A list of findings, with keys corresponding to object types (e.g., "datafile", "variables")
#' @param path The file path where the Excel workbook will be saved
#'
#' @return NULL (workbook is saved to path)
#'
#' @export

.write_to_excel <- function(findings, path, ...) {
  ## Create a new workbook
  wb <- createWorkbook()
  
  ## Loop through each findings object (datafile, variables, categories)
  for (object in names(findings)) {
    ## Show if no issues were found
    if (length(findings[[object]]) == 0) {
      addWorksheet(wb = wb, sheetName = object)
      writeData(wb = wb, sheet = object, x = "No issues were found!")
    }
    
    ## Loop through each diffdf item in object
    for (sheetName in names(findings[[object]])) {
      addWorksheet(wb = wb, sheetName = paste(object, sheetName, sep = "_"))
      writeData(wb = wb, sheet = paste(object, sheetName, sep = "_"), x = findings[[object]][[sheetName]])
    }
  }
  
  ## Save workbook (if there are any differences)
  if (!is_empty(names(wb))) {
    saveWorkbook(
      wb = wb,
      file = path,
      ...
    )
  }
}


#' Map TRES response codes to human-readable messages
#'
#' Extracts and interprets error codes from a TRES response string, mapping them
#' to descriptive messages. Prints warnings for each code and its frequency.
#'
#' @param response A string containing TRES response with error codes (e.g., "Error (E301)...")
#'
#' @return NULL (prints warnings about error code frequencies)
#'
#' @export

.tres_response_codes <- function(response) {
  ## Available codes by TRES
  codes <- c(
    "100" = "User not found",
    "200" = "Logon error",
    "300" = "Encrypt error",
    "301" = "Decrypt error",
    "303" = "Decrypt not allowed",
    "304" = "Search imaging not allowed",
    "305" = "Text contains illegal characters",
    "306" = "Encrypted value contains invalid user GUID",
    "307" = "Encrypted value is not base64",
    "308" = "Search image is not base64",
    "309" = "Decrypt string has invalid format",
    "310" = "Plain text not specified (can't encrypt NA)",
    "311" = "Decrypt string not specified",
    "312" = "Encrypt/decrypt mode not specified",
    "313" = "No permission to use api",
    "314" = "User is not logged in",
    "315" = "Encrypt on behalf not allowed",
    "316" = "Encrypted value is invalid or tampered with",
    "999" = "Unknown error occurred"
  )
  
  ## Get unique error codes
  clean_response <- c(sub(".*\\(E([0-9]+)\\).*", "\\1", response), "316")
  count_response <- table(clean_response)
  
  ## Print warnings
  for (x in names(count_response)) {
    warning(paste0(count_response[[x]], "x\tE", x, ": ", codes[x]))
  }
}


#' Clean data for missing values
#'
#' Replaces empty strings, "NA", and "null" with NA values in a diffdf.
#'
#' @param datafile A vector or data frame containing data to clean
#'
#' @return A cleaned version of the input data with missing values standardized
#'
#' @export

.clean_NA <- function(datafile) {
  ## Clean up ""
  datafile[datafile == ""] <- NA
  
  ## Clean up "NA"
  datafile[datafile == "NA"] <- NA
  
  ## Clean up "null"
  datafile[datafile == "null"] <- NA
  
  return(datafile)
}
