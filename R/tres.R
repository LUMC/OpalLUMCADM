
#' Test TRES Connection
#'
#' Creates a test connection to verify that the TRES encryption service is accessible.
#'
#' @param connection A connection object used to communicate with the TRES service.
#' @param ... Additional arguments passed to underlying functions.
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


#' Encrypt Data Using TRES
#'
#' Encrypts data using the TRES encryption service.
#'
#' @param connection A connection object used to communicate with the TRES service.
#' @param values Column values to encrypt.
#' @param search_image A logical value indicating whether to search for image data after encryption.
#' @param ... Additional arguments passed to underlying functions.
#' 
#' @return Result as encrypted values
#' 
#' @export

adm.tres_encryption <- function(connection, values, search_image = FALSE, ...) {
  ## Check if search_image & search_image in connection string are not both TRUE (this results in empty strings)
  if (connection$search_image == FALSE & search_image == TRUE) {
    stop("Search Image in connection is FALSE whilst TRUE in function parameter")
  }
  
  ## Set warning variable
  warn <- character()
  
  ## Encrypt values
  result <- withCallingHandlers(
    tres_encrypt(
      values = values,
      connection = connection,
      ...
    ),
    warning = function(w) {
      warn <<- c(warn, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  
  if(search_image) {
    result <- vec_extract_search_image(
      values = values
    )
  }
  
  ## Show warnings
  .tres_response_codes(response = warn)
  
  return(result)
}


#' Decrypt Data Using TRES
#'
#' Decrypts data using the TRES decryption service.
#'
#' @param connection A connection object used to communicate with the TRES service.
#' @param values Column values to encrypt.
#' @param ... Additional arguments passed to underlying functions.
#' 
#' @return Result as decrypted values
#' 
#' @export

adm.tres_decryption <- function(connection, values, ...) {
  ## Set warning variable
  warn <- character()
  
  ## Decrypt values
  result <- withCallingHandlers(
    tres_decrypt(
      values = values,
      connection = connection,
      ...
    ),
    warning = function(w) {
      warn <<- c(warn, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  
  ## Show warnings
  .tres_response_codes(response = warn)
  
  return(result)
}
