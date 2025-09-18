
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
    stop("Some columns are not encrypted (encrypted = yes): ", encrypted_yes[encrypted_yes == FALSE])
  }
  if (FALSE %in% encrypted_si) {
    stop("Some columns are not encrypted (encrypted = SI): ", encrypted_si[encrypted_si == FALSE])
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
    stop("Some columns have Infinite values: ", get_inf[get_inf == TRUE])
  }
}
