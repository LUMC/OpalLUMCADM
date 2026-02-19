
#' Run All Data Validation Checks
#'
#' Executes a comprehensive set of data validation checks across datafile, variables, and categories.
#'
#' @param datafile A data frame containing the actual data to be validated.
#' @param variables A data frame or list containing variable definitions (e.g., name, valueType, min, max, entityType, encrypted).
#' @param categories A data frame or list containing category definitions (e.g., variable, missing, labels).
#' 
#' @return A data frame containing logs of all validation checks performed.
#' 
#' @import dplyr
#' 
#' @export

check.run_all <- function(datafile, variables, categories = NULL) {
  message("Running all checks...")
  
  ## Function containing all checks
  start_checks <- function(...) {
    logs <- list()
    logs <- append(logs, .capture_logs(check.columns_var)(...))
    logs <- append(logs, .capture_logs(check.columns_cat)(...))
    logs <- append(logs, .capture_logs(check.valuetype)(...))
    logs <- append(logs, .capture_logs(check.minmax)(...))
    logs <- append(logs, .capture_logs(check.entitytype)(...))
    logs <- append(logs, .capture_logs(check.required_columns)(...))
    logs <- append(logs, .capture_logs(check.encrypted_values)(...))
    logs <- append(logs, .capture_logs(check.infinite)(...))
    logs <- append(logs, .capture_logs(check.date)(...))
    logs <- append(logs, .capture_logs(check.datetime)(...))
    logs <- append(logs, .capture_logs(check.duplicated_ids)(...))
    logs <- append(logs, .capture_logs(check.duplicated_rows)(...))
    logs <- append(logs, .capture_logs(check.character_ids)(...))
    logs <- append(logs, .capture_logs(check.var_column_datatype)(...))
    logs <- append(logs, .capture_logs(check.cat_labels)(...))
    
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


#' Check Column Mismatch Between Datafile and Variables
#'
#' Validates that all columns in the variables object are present in the datafile.
#'
#' @param datafile A data frame containing the actual data.
#' @param variables A data frame or list containing variable definitions.
#' @param ... Additional arguments passed to underlying functions.
#' 
#' @return A list of warnings about missing or extra columns.
#' 
#' @import dplyr purrr
#' 
#' @export

check.columns_var <- function(datafile, variables, ...) {
  ## Get columns
  columns_data <- colnames(datafile)
  columns_vars <- variables$name
  
  ## Ignore id column from data
  columns_data <- columns_data[columns_data != "id"]
  
  ## Get differences
  column_diff <- setdiff(columns_data, columns_vars)
  column_diff <- c(column_diff, setdiff(columns_vars, columns_data))
  
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


#' Check Column Mismatch Between Datafile and Categories
#'
#' Validates that all columns in the categories object are present in the datafile.
#'
#' @param datafile A data frame containing the actual data.
#' @param categories A data frame or list containing category definitions.
#' @param ... Additional arguments passed to underlying functions.
#' 
#' @return A list of warnings about missing columns in categories.
#' 
#' @import dplyr purrr
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


#' Check Value Type Consistency Between Data and Variables
#'
#' Compares the data type of each column in the datafile with the specified value types in variables.
#'
#' @param datafile A data frame containing the actual data.
#' @param variables A data frame or list containing variable definitions.
#' @param ... Additional arguments passed to underlying functions.
#' 
#' @return A list of warnings about mismatched value types.
#' 
#' @import dplyr
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


#' Check Minimum and Maximum Value Bounds
#'
#' Validates that numeric columns fall within the specified min and max bounds defined in variables.
#'
#' @param datafile A data frame containing the actual data.
#' @param variables A data frame or list containing variable definitions with min and max fields.
#' @param categories A data frame or list containing category definitions (used to handle missing values).
#' 
#' @return A list of warnings about values outside defined min/max ranges.
#' 
#' @import dplyr
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


#' Check Entity Type Consistency
#'
#' Ensures that only one entity type is defined across all variables.
#'
#' @param variables A data frame or list containing variable definitions.
#' @param ... Additional arguments passed to underlying functions.
#' 
#' @return A warning if more than one entity type is present.
#' 
#' @import dplyr
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


#' Check for Required Columns in Variables
#'
#' Validates the presence of required columns: 'label', 'entityType', and 'encrypted'.
#'
#' @param variables A data frame or list containing variable definitions.
#' @param ... Additional arguments passed to underlying functions.
#' 
#' @return A list of warnings if required columns are missing.
#' 
#' @import dplyr stringr
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


#' Check Encrypted Values Compliance
#'
#' Validates that encrypted columns contain only valid values ('no', 'yes', 'SI') and are fully encrypted.
#'
#' @param datafile A data frame containing the actual data.
#' @param variables A data frame or list containing variable definitions.
#' @param ... Additional arguments passed to underlying functions.
#' 
#' @return A warning or error if encrypted values are invalid or not fully encrypted.
#' 
#' @import dplyr
#' 
#' @export

check.encrypted_values <- function(datafile, variables, ...) {
  ## Check for encrypted column
  if (!("encrypted" %in% colnames(variables))) {
    stop("There is no 'encrypted' column in variables object")
  }
  
  if (!all(variables$encrypted %in% c("no", "yes", "SI"))) {
    warning("There are values in encrypted columns that don't match: no, yes or SI")
  }
  
  ## Tres options
  tres_values <- list(
    c("yes", "^3::", ">= 100"),
    c("SI", "^1:", "== 66")
  )
  
  ## Check for each tres option
  for (value in tres_values) {
    ## Check if all data is encrypted
    encrypted <- apply(
      datafile %>%
        select(variables %>% filter(encrypted == value[1]) %>% pull(name)), 2,
      function(x) {
        all(grepl(value[2], na.omit(x)) & eval(parse(text = paste0(nchar(na.omit(x)), value[3]))))
      }
    )
    
    ## Check content
    if (FALSE %in% encrypted) {
      stop(
        paste0("Some columns are not encrypted correctly (encrypted = ", value[1], "): "),
        paste(names(encrypted)[encrypted == FALSE], collapse = ", ")
      )
    }
  }
  
  ## Done
  message(" Checked encrypted values")
}


#' Check for Infinite Values in Data
#'
#' Identifies columns containing infinite values (Inf or -Inf).
#'
#' @param datafile A data frame containing the actual data.
#' @param ... Additional arguments passed to underlying functions.
#' 
#' @return A warning if any column contains infinite values.
#' 
#' @import dplyr
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


#' Check Date Format Consistency
#'
#' Validates that date columns are properly formatted according to the specified format.
#'
#' @param datafile A data frame containing the actual data.
#' @param variables A data frame or list containing variable definitions.
#' @param format A string specifying the date format (default: "\%Y-\%m-\%d").
#' @param ... Additional arguments passed to underlying functions.
#' 
#' @return A warning if any date column fails format validation.
#' 
#' @import dplyr lubridate
#' 
#' @export

check.date <- function(datafile, variables, format = "%Y-%m-%d", ...) {
  ## Get variables with date object
  get_date <- datafile %>%
    select(variables %>% filter(valueType == "date") %>% pull(name))
  
  ## Check format
  if (ncol(get_date) > 0) {
    is_date <- apply(
      get_date, 2,
      function(x) {
        all(!is.na(as.Date(na.omit(x), format = format)))
      })
    
    ## Check content
    if (FALSE %in% is_date) {
      warning(
        "Some date columns don't have Date format: `", format, "`: ",
        paste(names(is_date)[is_date == FALSE], collapse = ", ")
      )
    }
  }
  
  ## Done
  message(" Checked date format")
}


#' Check Datetime Format Consistency
#'
#' Validates that datetime columns are properly formatted according to the specified format.
#'
#' @param datafile A data frame containing the actual data.
#' @param variables A data frame or list containing variable definitions.
#' @param format A string specifying the datetime format (default: "\%Y-\%m-\%d \%H:\%M:\%OS").
#' @param ... Additional arguments passed to underlying functions.
#' 
#' @return A warning if any datetime column fails format validation.
#' 
#' @import dplyr lubridate
#' 
#' @export

check.datetime <- function(datafile, variables, format = "%Y-%m-%d %H:%M:%OS", ...) {
  ## Get variables with datetime object
  get_datetime <- datafile %>%
    select(variables %>% filter(valueType == "datetime") %>% pull(name))
  
  ## Check format
  if (ncol(get_datetime) > 0) {
    is_datetime <- apply(
      get_datetime, 2,
      function(x) {
        all(!is.na(as.POSIXct(na.omit(x), format = format)))
      })
    
    ## Check content
    if (FALSE %in% is_datetime) {
      warning(
        "Some datetime columns don't have POSIXct format: `", format, "`: ", 
        paste(names(is_datetime)[is_datetime == FALSE], collapse = ", ")
      )
    }
  }
  
  ## Done
  message(" Checked datetime format")
}


#' Check for Duplicated IDs in Data
#'
#' Ensures that the ID column contains no duplicated values.
#'
#' @param datafile A data frame containing the actual data.
#' @param id.name A character string specifying the ID column name (default: "id").
#' @param ... Additional arguments passed to underlying functions.
#' 
#' @return A warning if duplicated IDs are found.
#' 
#' @import dplyr
#' 
#' @export

check.duplicated_ids <- function(datafile, id.name = "id", ...) {
  check_duplicated <- datafile |> select(all_of(id.name)) |> duplicated()
  
  if(TRUE %in% check_duplicated) {
    warning("There are some duplicated IDs in column `", id.name, "`")
  }
  
  ## Done
  message(" Checked for duplicated IDs")
}


#' Check for Duplicated Rows in Variables and Categories
#'
#' Validates that both variables and categories objects do not contain duplicated rows.
#'
#' @param variables A data frame or list containing variable definitions.
#' @param categories A data frame or list containing category definitions.
#' @param ... Additional arguments passed to underlying functions.
#' 
#' @return A warning if duplicated rows are found in variables or categories.
#' 
#' @import dplyr
#' 
#' @export

check.duplicated_rows <- function(variables, categories = NULL, ...) {
  ## Check for duplicated rows in variables
  if (TRUE %in% duplicated(variables)) {
    warning("There are duplicated rows in variables object")
  }
  
  ## Check for duplicated rows in categories
  if (TRUE %in% duplicated(categories)) {
    warning("There are duplicated rows in categories object")
  }
  
  message(" Checked for duplicated row in variable/categorie objects")
}


#' Check if ID Column is Character Type
#'
#' Ensures that the ID column is of character type.
#'
#' @param datafile A data frame containing the actual data.
#' @param id.name A character string specifying the ID column name (default: "id").
#' @param ... Additional arguments passed to underlying functions.
#' 
#' @return A warning if the ID column is not of character type.
#' 
#' @import dplyr
#' 
#' @export

check.character_ids <- function(datafile, id.name = "id", ...) {
  ## Check if ID column is character
  dtype <- typeof(datafile[[id.name]])
  if (dtype != "character") {
    warning("ID values are not listed as character")
  }
  
  message(" Checked for ID as character")
}


#' Check Categorical Labels Completeness
#'
#' Ensures that all values in categorical columns are present in the category definitions.
#'
#' @param datafile A data frame containing the actual data.
#' @param categories A data frame or list containing category definitions.
#' @param ... Additional arguments passed to underlying functions.
#' 
#' @return A warning if any value in a categorical column is missing from the category list.
#' 
#' @import dplyr
#' 
#' @export

check.cat_labels <- function(datafile, categories, ...) {
  ## Get all character categories
  columns <- which(sapply(datafile, .map_dtype) %in% c("text", "integer"))
  categories <- categories[categories$variable %in% colnames(datafile)[columns], ]
  
  for (x in unique(categories$variable)) {
    cat_value <- categories$name[categories$variable == x]
    data_value <- na.omit(datafile[[x]][!(datafile[[x]] %in% cat_value)])
    if (!is_empty(data_value)) {
      warning(paste0("Categorie object is possibly missing a value for column: `", x, "`"))
    }
  }
  
  message(" Checked categorie labels")
}


#' Check Variable Object Datatypes
#'
#' Ensures that all columns in the variables object are character type.
#'
#' @param variables A data frame or list containing variable definitions.
#' @param ... Additional arguments passed to underlying functions.
#' 
#' @return A warning if any column is not of character type.
#' 
#' @import dplyr
#' 
#' @export

check.var_column_datatype <- function(variables, ...) {
  ## Remove columns with NA only
  variables <- variables[, colSums(is.na(variables)) < nrow(variables)]
  
  ## Ignore these columns
  variables[, c("min", "max", "repeatable", "index")] <- NULL
  
  ## Check if there are columns that are not character
  if (!all(sapply(na.omit(variables), class) == "character")) {
    warning("Not all columns in variable object are character (could cause ParseException)")
  }
  
  message(" Checked variable object datatypes")
}
