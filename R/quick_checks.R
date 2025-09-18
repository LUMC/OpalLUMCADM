
#' Function to check entity type
#' 
#' @param variables variables dataframe
#' 
#' @export

## Retrieved from checks_opal_R()
adm.check_entitytype <- function(variables) {
  entity <- unique(variables$entityType)
  
  if (length(entity) > 1) {
    stop("More then one entity type in use!")
  }
}


#' Function to check presence of labels & descriptions in variables
#' 
#' @param variables variables dataframe
#' 
#' @import stringr
#' 
#' @export

## Retrieved from checks_opal_R()
adm.check_labels <- function(variables) {
  col_labels <- str_detect(colnames(variables), "label")
  col_description <- str_detect(colnames(variables), "description")
  
  if (!TRUE %in% col_labels) {
    stop("There is no 'label' column in your variables object")
  }
  if (!TRUE %in% col_description) {
    stop("There is no 'description' column in your variables object")
  }
}


#' Function to check encryption of columns
#' 
#' @param datafile data input
#' @param variables variables dataframe
#' 
#' @export

## Retrieved from checks_opal_R()
adm.check_encrypted_columns <- function(datafile, variables) {
  ## Get all columns that should be encrypted
  encrypted_columns <- str_detect(colnames(variables), "encrypted")
  if (!TRUE %in% encrypted_columns) {
    stop("There is no 'encrypted' column in your variables object")
  }
  
  adm.check_encrypted_values(...)
}


#' Function to check encryption of values
#' 
#' @param datafile data input
#' @param variables variables dataframe
#' 
#' @export

## Retrieved from checks_opal_R()
adm.check_encrypted_values <- function(datafile, variables) {
  ## Check if all data is encrypted yes
  encrypted_yes <- apply(
    datafile %>%
      select(variables %>% filter(encrypted == "yes") %>% pull(name)), 2,
    function(x) {
      all(grepl("^3::", na.omit(x)))
    }
  )
  
  ## Check if all data is encrypted SI
  encrypted_si <- apply(
    datafile %>%
      select(variables %>% filter(encrypted == "SI") %>% pull(name)), 2,
    function(x) {
      all(grepl("^1:", na.omit(x)))
    }
  )
  
  ## Check content
  if (FALSE %in% encrypted_yes) {
    warning(
      "Some columns are not encrypted (encrypted = yes): ",
      paste(names(encrypted_yes)[encrypted_yes == FALSE], collapse = ", ")
    )
  }
  if (FALSE %in% encrypted_si) {
    warning(
      "Some columns are not encrypted (encrypted = SI): ",
      paste(names(encrypted_si)[encrypted_si == FALSE], collapse = ", ")
    )
  }
}


#' Function to check on Inf values
#' 
#' @param datafile data input
#' 
#' @export

## Retrieved from checks_opal_R()
adm.check_infinite <- function(datafile) {
  get_inf <- apply(
    datafile, 2,
    function(x) {
      any(is.infinite(x))
    }
  )
  
  ## Check content
  if (TRUE %in% get_inf) {
    warning(
      "Some columns have Infinite values: ", 
      paste(names(get_inf)[get_inf == TRUE], collapse = ", ")
    )
  }
}


#' Function to check date
#' 
#' @param datafile data input
#' @param variables variables dataframe
#' 
#' @import lubridate
#' 
#' @export

## Retrieved from checks_opal_R()
adm.check_date <- function(datafile, variables) {
  ## Get variables with date object
  is_date <- apply(
    datafile %>%
      select(variables %>% filter(valueType == "date") %>% pull(name)), 2,
    function(x) {
      all(!is.na(as.Date(na.omit(x), format = "%Y-%m-%d")))
    }
  )
  
  ## Check content
  if (FALSE %in% is_date) {
    warning(
      "Some date columns don't have format `yyyy-mm-dd`: ", 
      paste(names(is_date)[is_date == FALSE], collapse = ", ")
    )
  }
}


#' Function to check datetime
#' 
#' @param datafile data input
#' @param variables variables dataframe
#' 
#' @import lubridate
#' 
#' @export

## Retrieved from checks_opal_R()
adm.check_datetime <- function(datafile, variables) {
  ## Get variables with date object
  is_datetime <- apply(
    datafile %>%
      select(variables %>% filter(valueType == "datetime") %>% pull(name)), 2,
    function(x) {
      all(!is.na(as.POSIXct(na.omit(x), format = "%Y-%m-%d %H:%M:%S")))
    }
  )
  
  ## Check content
  if (FALSE %in% is_datetime) {
    warning(
      "Some datetime columns don't have POSIXct format: `yyyy-mm-dd hh:mm:ss`: ", 
      paste(names(is_datetime)[is_datetime == FALSE], collapse = ", ")
    )
  }
}























