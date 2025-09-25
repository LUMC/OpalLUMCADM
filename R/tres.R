
#' Function to check tres connection
#'
#' @param connection Connection with RTRES
#'
#' @import rtres
#' 
#' @return 
#'
#' @export

## Retrieved from encrypt_data()
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
    message("TRES connection succesfull!")
  }
}


#' Function to encrypt with tres
#'
#' @param connection Connection with RTRES
#' @param datafile data input
#' @param columns selected columns for encryption
#'
#' @import rtres
#' 
#' @return 
#'
#' @export

## Retrieved from encrypt_data()
adm.tres_encryption <- function(connection, datafile, columns = NA, ...) {
  ## Select all columns if no columns are selected
  if (is.na(columns)) {
    columns <- colnames(datafile)
  }
  
  ## Encrypt for each column
  for (column in columns) {
    datafile[[column]] <- tres_encrypt(
      values = datafile[[column]],
      connection = connection,
      ...
    )
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
#' @return 
#'
#' @export

## Retrieved from encrypt_data()
adm.tres_decryption <- function(connection, datafile, columns = NA, ...) {
  ## Select all columns if no columns are selected
  if (is.na(columns)) {
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
