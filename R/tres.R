
#' Function to get tres response codes
#'
#' @export

adm.tres_response_codes <- function() {
  message("Here are all TRES response codes:")
  
  message(
    "- 100: User not found\n",
    "- 200: Logon error\n",
    "- 300: Encrypt error\n",
    "- 301: Decrypt error\n",
    "- 303: Decrypt not allowed\n",
    "- 304: Search imaging not allowed\n",
    "- 305: Text contains illegal characters\n",
    "- 306: Encrypted value contains invalid user GUID\n",
    "- 307: Encrypted value is not base64\n",
    "- 308: Search image is not base64\n",
    "- 309: Decrypt string has invalid format\n",
    "- 310: Plain text not specified (can't encrypt NA)\n",
    "- 311: Decrypt string not specified\n",
    "- 312: Encrypt/decrypt mode not specified\n",
    "- 313: No permission to use api\n",
    "- 314: User is not logged in\n",
    "- 315: Encrypt on behalf not allowed\n",
    "- 316: Encrypted value is invalid or tampered with (can't encrypt NA)\n",
    "- 999: Unkown error occured"
  )
}


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
  
  ## Encrypt for each column
  for (column in columns) {
    datafile[[column]] <- tres_encrypt(
      values = datafile[[column]],
      connection = connection,
      search_image = search_image,
      ...
    )
    
    if(search_image) {
      datafile[[column]] <- vec_extract_search_image(
        values = datafile[[column]]
      )
    }
  }
  
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
  
  ## Encrypt for each column
  for (column in columns) {
    datafile[[column]] <- tres_decrypt(
      values = datafile[[column]],
      connection = connection,
      ...
    )
  }
  
  return(datafile)
}


