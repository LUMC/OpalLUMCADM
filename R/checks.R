
#' Function to check run all checks
#'
#' @param datafile data input
#' @param variables variables dataframe
#' @param categories categories dataframe
#' 
#' @export

check.run_all <- function(datafile, variables, categories = NULL) {
  message("Running all checks...")
  
  ## Function containing all checks
  start_checks <- function(...) {
    logs <- list()
    logs[01] <- .capture_logs(check.columns_var)(...)
    logs[02] <- .capture_logs(check.columns_cat)(...)
    logs[03] <- .capture_logs(check.valuetype)(...)
    logs[04] <- .capture_logs(check.minmax)(...)
    logs[05] <- .capture_logs(check.entitytype)(...)
    logs[06] <- .capture_logs(check.required_columns)(...)
    logs[07] <- .capture_logs(check.encrypted_values)(...)
    logs[08] <- .capture_logs(check.infinite)(...)
    logs[09] <- .capture_logs(check.date)(...)
    logs[10] <- .capture_logs(check.datetime)(...)
    logs[11] <- .capture_logs(check.ids)(...)
    
    ## Create dataframe from all logs
    logs <- do.call(rbind.data.frame, logs)
    return(logs)
  }
  
  ## Start all checks
  df_logs <- start_checks(
    datafile = datafile,
    variables = variables,
    categories = categories
  )
  
  ## Done
  message("All checks done!")
  return(df_logs)
}


#' Function to check if all columns listed in variables are also in the dataset and vice-versa
#'
#' @param datafile data input
#' @param variables variables dataframe
#' 
#' @import purrr
#' 
#' @export

check.columns_var <- function(datafile, variables, ...) {
  ## Get columns
  columns_data <- colnames(datafile)
  columns_vars <- variables$name
  
  ## Get differences
  column_diff <- setdiff(columns_data, columns_vars)
  
  ## Show differences
  if (!is_empty(column_diff)) {
    for (column in column_diff) {
      warning(
        "Column '", column, "' in data: ", column %in% columns_data,
        " & column '", column, "' in variables: ", column %in% columns_vars
      )
    }
  }
  
  ## Done
  message(" Checked columns between datafile & variables")
}


#' Function to check if all columns listed in categories are also in the dataset
#'
#' @param datafile data input
#' @param categories categories dataframe
#' 
#' @import purrr
#' 
#' @export

check.columns_cat <- function(datafile, categories = NULL, ...) {
  if (is.null(categories)) {
    warning("There is no categorie object")
    return()
  }
  
  ## Get columns
  columns_data <- colnames(datafile)
  columns_cats <- categories$variable
  
  ## Get differences
  column_diff <- setdiff(columns_cats, columns_data)
  
  ## Show differences
  if (!is_empty(column_diff)) {
    for (column in column_diff) {
      warning(
        "Column '", column, "' in data: ", column %in% columns_data,
        " & column '", column, "' in categories: ", column %in% columns_cats
      )
    }
  }
  
  ## Done
  message(" Checked columns between datafile & categories")
}


#' Function to check valuetypes of data vs variables
#'
#' @param datafile data input
#' @param variables variables dataframe
#' 
#' @export

check.valuetype <- function(datafile, variables, ...) {
  ## Get valuetypes
  valuetypes_data <- sapply(datafile, .map_dtype)
  valuetypes_vars <- setNames(variables$valueType, variables$name)
  
  ## Compare valuetypes
  for (column in names(valuetypes_vars)) {
    compare <- identical(valuetypes_vars[column], valuetypes_data[column])
    if (!compare) {
      warning(
        "ValueType of '", column, "' doesn't match: ",
        valuetypes_vars[column], " vs ", valuetypes_data[column]
      )
    }
  }
  
  ## Done
  message(" Checked valuetypes")
}


#' Function to check data on minimum & maximum values if defined in variables
#'
#' @param datafile data input
#' @param variables variables dataframe
#' @param categories categories dataframe
#' 
#' @export

check.minmax <- function(datafile, variables, categories = NULL) {
  ## Check for min/max columns
  if (!all(c("min", "max") %in% colnames(variables))) {
    warning("There is no min/max column in variables object")
    return()
  }
  
  ## Get min/max values
  min_values <- setNames(variables$min, variables$name)
  max_values <- setNames(variables$max, variables$name)
  
  ## Set null to NA
  min_values[min_values == "null"] <- NA
  max_values[max_values == "null"] <- NA
  
  ## Get numeric or integer columns (exclude columns with all NA)
  datafile <- datafile[, colSums(is.na(datafile)) < nrow(datafile)]
  valuetypes_data <- sapply(datafile, function(x) tail(class(x), n = 1))
  valuetypes_data <- valuetypes_data[valuetypes_data %in% c("numeric", "integer", "double", "POSIXct", "POSIXt", "Date")]
  
  ## Check min/max for each numeric column
  for (column in names(valuetypes_data)) {
    ## Ignore missing values in min/max check
    if (!is.null(categories)) {
      missing <- categories$name[categories$variable == column & categories$missing == TRUE]
      datafile[[column]][datafile[[column]] %in% missing] <- NA
    }
    
    ## Get min/max values from datafile & variables objects
    data_min <- type.convert(as.character(min(datafile[[column]], na.rm = TRUE)), as.is = TRUE)
    data_max <- type.convert(as.character(max(datafile[[column]], na.rm = TRUE)), as.is = TRUE)
    var_min <- type.convert(as.character(min_values[[column]]), as.is = TRUE)
    var_max <- type.convert(as.character(max_values[[column]]), as.is = TRUE)
    
    ## Compare min & max
    tryCatch({
      if (data_min < var_min) {
        warning("'", column, "' minimum value to low: ", data_min, " < ", var_min)
      }
    }, error = function(e) {})
    tryCatch({
      if (data_max > var_max) {
        warning("'", column, "' maximum value to high: ", data_max, " > ", var_max)
      }
    }, error = function(e) {})
  }
  
  ## Done
  message(" Checked min/max values")
}


#' Function to check entity type
#' 
#' @param variables variables dataframe
#' 
#' @export

check.entitytype <- function(variables, ...) {
  ## Get entity types
  entity <- unique(variables$entityType)
  
  if (length(entity) > 1) {
    stop("More then one entity type in use")
  }
  
  ## Done
  message(" Checked entity type")
}


#' Function to check presence of required columns labels, descriptions & encrypted in variables
#' 
#' @param variables variables dataframe
#' 
#' @import stringr
#' 
#' @export

check.required_columns <- function(variables, ...) {
  ## Search for specific columns
  col_labels <- str_detect(colnames(variables), "label")
  col_entitytype <- str_detect(colnames(variables), "entityType")
  col_encrypted <- str_detect(colnames(variables), "encrypted")
  
  ## Show warning if something is missing
  if (!TRUE %in% col_labels) {
    warning("There is no 'label' column in variables object")
  }
  if (!TRUE %in% col_entitytype) {
    warning("There is no 'entityType' column in variables object")
  }
  if (!TRUE %in% col_encrypted) {
    warning("There is no 'encrypted' column in variables object")
  }
  
  ## Done
  message(" Checked required columns")
}


#' Function to check encryption of values
#' 
#' @param datafile data input
#' @param variables variables dataframe
#' 
#' @import dplyr
#' 
#' @export

check.encrypted_values <- function(datafile, variables, ...) {
  ## Check for encrypted column
  col_encrypted <- str_detect(colnames(variables), "encrypted")
  if (!TRUE %in% col_encrypted) {
    stop("There is no 'encrypted' column in variables object")
  }
  
  ## Tres options
  tres_values <- list(
    c("yes", "^3::"),
    c("SI", "^1:")
  )
  
  ## Check for each tres option
  for (value in tres_values) {
    ## Check if all data is encrypted
    encrypted <- apply(
      datafile %>%
        select(variables %>% filter(encrypted == value[1]) %>% pull(name)), 2,
      function(x) {
        all(grepl(value[2], na.omit(x)))
      }
    )
    
    ## Check content
    if (FALSE %in% encrypted) {
      stop(
        paste0("Some columns are not encrypted (encrypted =", value[1], "): "),
        paste(names(encrypted)[encrypted == FALSE], collapse = ", ")
      )
    }
  }
  
  ## Done
  message(" Checked encrypted values")
}


#' Function to check on Inf values
#' 
#' @param datafile data input
#' 
#' @export

check.infinite <- function(datafile, ...) {
  ## Search for infinite values
  get_inf <- sapply(datafile, function(x) {
    any(is.infinite(x))
  })
  
  ## Check content
  if (TRUE %in% get_inf) {
    warning(
      "Some columns have Infinite values: ", 
      paste(names(get_inf)[get_inf == TRUE], collapse = ", ")
    )
  }
  
  ## Done
  message(" Checked infinite values")
}


#' Function to check date
#' 
#' @param datafile data input
#' @param variables variables dataframe
#' 
#' @import lubridate
#' 
#' @export

check.date <- function(datafile, variables, ...) {
  ## Get variables with date object
  get_date <- datafile %>%
    select(variables %>% filter(valueType == "date") %>% pull(name))
  
  ## Check format
  if (ncol(get_date) > 0) {
    is_date <- apply(
      get_date, 2,
      function(x) {
        all(!is.na(as.Date(na.omit(x), format = "%Y-%m-%d")))
      })
    
    ## Check content
    if (FALSE %in% is_date) {
      warning(
        "Some date columns don't have format: `yyyy-mm-dd`: ",
        paste(names(is_date)[is_date == FALSE], collapse = ", ")
      )
    }
  }
  
  ## Done
  message(" Checked date format")
}


#' Function to check datetime
#' 
#' @param datafile data input
#' @param variables variables dataframe
#' 
#' @import lubridate
#' 
#' @export

check.datetime <- function(datafile, variables, ...) {
  ## Get variables with datetime object
  get_datetime <- datafile %>%
    select(variables %>% filter(valueType == "datetime") %>% pull(name))
  
  ## Check format
  if (ncol(get_datetime) > 0) {
    is_datetime <- apply(
      get_datetime, 2,
      function(x) {
        all(!is.na(as.Date(na.omit(x), format = "%Y-%m-%d")))
      })
    
    ## Check content
    if (FALSE %in% is_datetime) {
      warning(
        "Some datetime columns don't have POSIXct format: `yyyy-mm-dd hh:mm:ss`: ", 
        paste(names(is_datetime)[is_datetime == FALSE], collapse = ", ")
      )
    }
  }
  
  ## Done
  message(" Checked datetime format")
}


#' Function to check if datafile has duplicated ids
#'
#' @param datafile data input
#' @param id.name The name of the column representing the entity identifiers. Default is 'id'.
#' 
#' @export

check.ids <- function(datafile, id.name = "id", ...) {
  check_duplicated <- datafile |> select(all_of(id.name)) |> duplicated()
  
  if(TRUE %in% check_duplicated) {
    warning("There are some duplicated IDs in column `", id.name, "`")
  }
  
  ## Done
  message(" Checked for duplicated IDs")
}
