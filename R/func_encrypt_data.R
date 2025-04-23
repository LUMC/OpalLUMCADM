# Written by: Lars van der Burg
# Written on: 2024-12-05
#
# Punten om te bespreken:
## Nu encrypteren van een vector of een tibble/data.frame. Willen we ook een list?
## Bij encrypteren van een tibble kan je encryptie toevoegen als een extra kolom (, .names="{.col}_enc")) of kolom vervangen. Lijkt me dat vervangen is wat we willen?
#
#' @title Encrypt a vector or a tibble/data.frame
#'
#' @description wrapper around the zorgTTP rtres package to encrypt data. Added a check for connection, summary of the warnings and allows for encryption of a vector or a tibble/data.frame
#'
#' @param con string. Tres connection as obtained via \code{tres_connect}
#' @param data vector or tibble/data.frame. The data that needs to be encrypted
#' @param vars_to_encrypt optional vector. Indicates which variables need to be encrypted if \code{data} is a tibble or a data.frame. If NULL all variables will be encrypted
#' @param search_image boolean. Whether or not the search_image needs to be included when encrypting (default = TRUE). If TRUE, the search_image needs to be extracted via vec_extract_search_image. Cannot be TRUE together with \code{salted_encryption}.
#' @param salted_encryption boolean. Whether or not ... (default = FALSE). Cannot be TRUE together with \code{search_image}.
#'
#' @return encrypted vector or a tibble with the vars_to_encrypt as encrypted. Also print a summary of the warnings that the \code{tres_encrypt} function returned.
#'
#' @import rtres
#'
#' @author Lars van der Burg
#'
#' @note When encrypt_data is used inside a dplyr::mutate call the summary of the warnings will be cumulative. Unclear how to reset dplyr::last_dplyr_warnings
#'
#' @export
encrypt_data = function(con, data, vars_to_encrypt = NULL, search_image = FALSE, salted_encryption = FALSE){

  test_connection = tryCatch({
    tres_encrypt(values = "Test", connection = con, search_image = search_image, salted_encryption = salted_encryption)
  }, error = function(e){paste("cannot start the encryption because there is a problem with the tres_connect. Here is the error message:\n", e$message)})
  if(!str_starts(test_connection, "3::")){
    stop(test_connection, call. = FALSE)
  }


  ## Unknown how to clear dplyr::last_dplyr_warnings()...
  ## So run a manual warning that ensures that later works correctly...
  foo <- function(){warning("foo")}; df <- tibble(x = 1); suppressWarnings(df <- mutate(df, x = foo()))


  if(is.vector(data)){
    # data_encr = suppressWarnings(tres_encrypt(data, con, search_image = search_image, salted_encryption = salted_encryption))
    data_encr = suppressWarnings(tibble(x = data) |> mutate(y = tres_encrypt(x, con, search_image = search_image, salted_encryption = salted_encryption)) |> pull(y))
    warning_list <<- dplyr::last_dplyr_warnings(n = Inf)

    if(search_image){
      data_encr = vec_extract_search_image(data_encr)
    }

  } else if(tibble::is_tibble(data) | is.data.frame(data)){

    if(is.null(vars_to_encrypt)){
      cat("You have not indicated which variables need to be encrypted, will encrypt all\n")
      vars_to_encrypt = colnames(data)

    } else if(FALSE %in% (vars_to_encrypt %in% colnames(data))){
      stop(paste0("The variable(s) ", paste(vars_to_encrypt[!vars_to_encrypt %in% colnames(data)], collapse = ", "), " that you want to encrypt is/are not in the data\n"))

    }

    data_encr = suppressWarnings(data |> mutate(across(all_of(vars_to_encrypt), ~tres_encrypt(.x, con, search_image = search_image, salted_encryption = salted_encryption))))
    warning_list <- dplyr::last_dplyr_warnings(n = Inf)


    if(search_image){
      data_encr = data_encr |> mutate(across(all_of(vars_to_encrypt), ~vec_extract_search_image(.x), .names = "{.col}"))
    }
  }


  warning_list_foos = unlist(lapply(warning_list, \(x){paste0(x$parent) == "simpleWarning in foo(): foo\n"}))
  if(max(which(warning_list_foos)) == length(warning_list_foos)){
    warning_list = NULL
  } else {
    warning_list = warning_list[(which(warning_list_foos)[sum(warning_list_foos)] + 1):(length(warning_list))]
  }

  if(length(warning_list) != 0){
    all_warnings = matrix(unlist(lapply(warning_list, \(x){str_match(x$parent$message, "\\[\\d+\\]\\s*(.+?)\\.\\s*\\((E\\d+)\\)")[1, c(2, 3)]})),
                          ncol = 2, byrow = TRUE, dimnames = list(NULL, c("Message", "Code"))) |> as_tibble()
    cat("Here are the following warnings that occur:\n")
    all_warnings_code = unique(all_warnings$Code); all_warnings_message = unique(all_warnings$Message)
    for(i in 1:length(all_warnings_code)){
      cat(paste0(all_warnings_code[i], ": ", as.integer(table(all_warnings$Code)[all_warnings_code[i]]), " times. ", all_warnings_message[i], "\n"))
    }
  } else {
    cat("Encryption succeeded without any warnings!\n")
  }

  return(data_encr)
}

