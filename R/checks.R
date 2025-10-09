
#' Function to check run all checks
#'
#' @param datafile data input
#' @param variables variables dataframe
#'
#' @import opalr rlang
#' 
#' @export

adm.run_all_checks <- function(datafile, variables) {
  message("Running all checks...")
  
  ## Function containing all checks
  start_checks <- function(...) {
    adm.check_columns(...)
    adm.check_valuetype(...)
    adm.check_minmax(...)
    adm.check_entitytype(...)
    adm.check_required_columns(...)
    adm.check_encrypted_values(...)
    adm.check_infinite(...)
    adm.check_date(...)
    adm.check_datetime(...)
  }
  
  ## Start all checks
  start_checks(
    datafile = datafile,
    variables = variables
  )
  
  ## Done
  message("All checks done!")
}


#' Function to check if all columns listed in variables are also in the dataset and vice-versa
#'
#' @param datafile data input
#' @param variables variables dataframe
#'
#' @import opalr rlang
#' 
#' @export

## TODO categories compare with the datafile
adm.check_columns <- function(datafile, variables, categories = NULL) {
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


#' Function to check valuetypes of data vs variables
#'
#' @param datafile data input
#' @param variables variables dataframe
#'
#' @import opalr
#' 
#' @export

adm.check_valuetype <- function(datafile, variables) {
  ## Function to set class type
  map_dtype <- function(column) {
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
  
  ## Get valuetypes
  valuetypes_data <- sapply(datafile, map_dtype)
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
#'
#' @import opalr
#' 
#' @export

adm.check_minmax <- function(datafile, variables) {
  ## Check for min/max columns
  if (!all(c("min", "max") %in% colnames(variables))) {
    warning("There are no min/max columns in variables object")
    return()
  }
  
  ## Get min/max values
  min_values <- suppressWarnings(setNames(as.numeric(variables$min), variables$name))
  max_values <- suppressWarnings(setNames(as.numeric(variables$max), variables$name))
  
  ## Get numeric or integer columns (exclude columns with all NA)
  datafile <- datafile[, colSums(is.na(datafile)) < nrow(datafile)]
  valuetypes_data <- sapply(datafile, function(x) tail(class(x), n = 1))
  valuetypes_data <- valuetypes_data[valuetypes_data %in% c("numeric", "integer", "double")]
  
  ## Check min/max for each column
  for (column in names(valuetypes_data)) {
    get_min <- as.numeric(min(datafile[[column]], na.rm=TRUE))
    get_max <- as.numeric(max(datafile[[column]], na.rm=TRUE))
    
    ## Compare min & max
    if (!is.na(min_values[[column]]) & get_min < min_values[[column]]) {
      warning("'", column, "' minimum value to low: ", get_min, " < ", min_values[[column]])
    }
    if (!is.na(max_values[[column]]) & get_max > max_values[[column]]) {
      warning("'", column, "' maximum value to high: ", get_max, " > ", max_values[[column]])
    }
  }
  
  ## Done
  message(" Checked min/max values")
}


#' Function to check entity type
#' 
#' @param variables variables dataframe
#' 
#' @export

adm.check_entitytype <- function(variables, ...) {
  ## Get entity types
  entity <- unique(variables$entityType)
  
  if (length(entity) > 1) {
    stop("More then one entity type in use!")
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

adm.check_required_columns <- function(variables, ...) {
  ## Search for column labels & descriptions
  col_labels <- str_detect(colnames(variables), "label")
  col_description <- str_detect(colnames(variables), "description")
  col_encrypted <- str_detect(colnames(variables), "encrypted")
  
  ## Show warning if something is missing
  if (!TRUE %in% col_labels) {
    warning("There is no 'label' column in your variables object!")
  }
  if (!TRUE %in% col_description) {
    warning("There is no 'description' column in your variables object!")
  }
  if (!TRUE %in% col_encrypted) {
    warning("There is no 'encrypted' column in your variables object!")
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

adm.check_encrypted_values <- function(datafile, variables) {
  ## Check for encrypted column
  col_encrypted <- str_detect(colnames(variables), "encrypted")
  if (!TRUE %in% col_encrypted) {
    stop("There is no 'encrypted' column in your variables object!")
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

adm.check_infinite <- function(datafile, ...) {
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

adm.check_date <- function(datafile, variables) {
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

adm.check_datetime <- function(datafile, variables) {
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
#' @param id The name of the column representing the entity identifiers. Default is 'id'.
#' 
#' @export

adm.check_ids <- function(datafile, id.name = "id") {
  check_duplicated <- datafile |> select(all_of(id.name)) |> duplicated()
  
  if(TRUE %in% check_duplicated) {
    warning("There are some duplicated IDs in column `", id.name, "`")
  }
  
  ## Done
  message(" Checked for duplicated IDs")
}

