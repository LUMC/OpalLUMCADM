
#' Test TRES Connection
#'
#' Creates a test connection to verify that the TRES encryption service is accessible.
#'
#' @param connection A connection object used to communicate with the TRES service.
#' 
#' @return A character string starting with "3::" indicating successful connection.
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


#' Encrypt Data Columns Using TRES
#'
#' Encrypts specified columns in a data frame using the TRES encryption service.
#'
#' @param connection A connection object used to communicate with the TRES service.
#' @param datafile A data frame containing the data to be encrypted.
#' @param columns A character vector specifying which columns to encrypt. If NULL, all columns are encrypted.
#' @param search_image A logical value indicating whether to search for image data after encryption.
#' 
#' @return A data frame with encrypted values in the specified columns.
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


#' Decrypt Data Columns Using TRES
#'
#' Decrypts specified columns in a data frame using the TRES decryption service.
#'
#' @param connection A connection object used to communicate with the TRES service.
#' @param datafile A data frame containing the encrypted data to be decrypted.
#' @param columns A character vector specifying which columns to decrypt. If NULL, columns starting with "3::" are selected.
#' 
#' @return A data frame with decrypted values in the specified columns.
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
