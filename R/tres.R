

#' Function to check tres connection
#'
#' @param connection Connection with RTRES
#'
#' @import rtres
#'
#' @export

adm.tres_connection <- function(connection, ...) {
  ## Create a test connection
  test_connection <- tryCatch({
    tres_encrypt(
      values = "Test",
      connection = connection,
      ...
    )
  }, error = function(e) {
    stop(
      "Cannot start the encryption because there is a problem with the tres_connect. Here is the error message:\n",
      e$message
    )
  })
  
  ## Check result
  if(!str_starts(test_connection, "3::")){
    stop(test_connection, call. = FALSE)
  } else {
    message("TRES connection successful!")
  }
}


#' Function to encrypt with tres
#'
#' @param connection Connection with RTRES
#' @param datafile data input
#' @param columns selected columns for encryption
#' @param boolean. Whether or not the search_image needs to be included when encrypting
#'
#' @import rtres
#' 
#' @return datafile, encrypted datafile
#'
#' @export

adm.tres_encryption <- function(connection, datafile, columns = NULL, search_image = FALSE, ...) {
  ## Select all columns if no columns are selected
  if (is.null(columns)) {
    columns <- colnames(datafile)
  }
  
  ## Set warning variable
  warn <- character()
  
  ## Encrypt for each column
  for (column in columns) {
    withCallingHandlers(
      datafile[[column]] <- tres_encrypt(
        values = datafile[[column]],
        connection = connection,
        search_image = search_image,
        ...
      ),
      warning = function(w) {
        warn <<- c(warn, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    
    if(search_image) {
      datafile[[column]] <- vec_extract_search_image(
        values = datafile[[column]]
      )
    }
  }
  
  ## Show warnings
  .tres_response_codes(response = warn)
  
  return(datafile)
}


#' Function to decrypt with tres
#'
#' @param connection Connection with RTRES
#' @param datafile data input
#' @param columns selected columns for decryption
#'
#' @import rtres dplyr
#' 
#' @return datafile, decrypted datafile
#'
#' @export

adm.tres_decryption <- function(connection, datafile, columns = NULL, ...) {
  ## Select all columns if no columns are selected
  if (is.null(columns)) {
    columns <- datafile %>% 
      select(where(~ any(grepl("^3::", .)))) %>%
      colnames()
  }
  
  ## Set warning variable
  warn <- character()
  
  ## Encrypt for each column
  for (column in columns) {
    withCallingHandlers(
      datafile[[column]] <- tres_decrypt(
        values = datafile[[column]],
        connection = connection,
        ...
      ),
      warning = function(w) {
        warn <<- c(warn, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
  }
  
  ## Show warnings
  .tres_response_codes(response = warn)
  
  return(datafile)
}
