# Written by: Lars van der Burg
# Written on: 2024-10-10
#
# Punten om te bespreken:
#
#' @title Decrypt a vector or a tibble/data.frame
#'
#' @description wrapper around the zorgTTP rtres package to decrypt data. Added a check for connection, summary of the warnings and allows for decryption of a vector or a tibble/data.frame
#'
#' @param con string. Tres connection as obtained via \code{tres_connect}
#' @param data vector or tibble/data.frame. The data that needs to be decrypted
#' @param vars_to_decrypt optional vector. Indicates which variables need to be decrypted if \code{data} is a tibble or a data.frame. If NULL all variables will be decrypted
#'
#' @return decrypted vector or a tibble with the vars_to_decrypt as decrypted. Also print a summary of the warnings that the \code{tres_decrypt} function returned.
#'
#' @import rtres
#'
#' @author Lars van der Burg
#'
#' @note When decrypt_data is used inside a dplyr::mutate call the summary of the warnings will be cumulative. Unclear how to reset dplyr::last_dplyr_warnings
#'
#' @export
decrypt_data = function(con, data, vars_to_decrypt = NULL){

  test_connection = tryCatch({
    tres_encrypt(values = "Test", connection = con)
  }, error = function(e){paste("cannot start the decryption because there is a problem with the tres_connect. Here is the error message:\n", e$message)})
  if(!str_starts(test_connection, "3::")){
    stop(test_connection, call. = FALSE)
  }


  ## Unknown how to clear dplyr::last_dplyr_warnings()...
  ## So run a manual warning that ensures that later works correctly...
  foo <- function(){warning("foo")}; df <- tibble(x = 1); suppressWarnings(df <- mutate(df, x = foo()))


  if(is.vector(data)){

    if(!(FALSE %in% str_starts(data, "3::"))){
      # data_decr = suppressWarnings(tres_decrypt(data, con))
      data_decr = suppressWarnings(tibble(x = data) |> mutate(y = tres_decrypt(x, con)) |> pull(y))
      warning_list = dplyr::last_dplyr_warnings(n = Inf)
    } else {
      stop("Data is not (completely/non-SI) decrypted because haven't detected 3:: encryption\n")
    }

  } else if(tibble::is_tibble(data) | is.data.frame(data)){

    if(is.null(vars_to_decrypt)){
      vars_to_decrypt = names(which(!apply(data, 2, \(x){FALSE %in% str_starts(x, "3::")})))

      if(length(vars_to_decrypt) == 0){
        stop("No variables are decrypted, because haven't detected any encryption\n")
      } else {
        cat("You have not indicated which variables need to be decrypted, so will decrypt:", vars_to_decrypt, "\n")
      }

    } else if(FALSE %in% (vars_to_decrypt %in% colnames(data))){
      stop(paste0("The variable(s) ", paste(vars_to_decrypt[!vars_to_decrypt %in% colnames(data)], collapse = ", "), " that you want to decrypt are not in the data\n"))

    } else if(TRUE %in% apply(data[, vars_to_decrypt], 2, \(x){FALSE %in% str_starts(x, "3::")})){
      stop(paste0("The variable(s) ", paste(names(which(apply(data[, vars_to_decrypt], 2, \(x){FALSE %in% str_starts(x, "3::")}))), collapse = ", "), " that you want to decrypt have values that do not start with '3::'\n"))
    }


    data_decr = suppressWarnings(data |> mutate(across(all_of(vars_to_decrypt), ~tres_decrypt(.x, con))))
    warning_list = dplyr::last_dplyr_warnings(n = Inf)
  }


  warning_list = warning_list[unlist(lapply(warning_list, \(x){paste0(x$parent) != "simpleWarning in foo(): foo\n"}))]

  if(length(warning_list) != 0){
    all_warnings = matrix(unlist(lapply(warning_list, \(x){str_match(x$parent$message, "\\[\\d+\\]\\s*(.+?)\\.\\s*\\((E\\d+)\\)")[1, c(2, 3)]})),
                          ncol = 2, byrow = TRUE, dimnames = list(NULL, c("Message", "Code"))) |> as_tibble()
    cat("Here are the following warnings that occur:\n")
    all_warnings_code = unique(all_warnings$Code); all_warnings_message = unique(all_warnings$Message)
    for(i in 1:length(all_warnings_code)){
      cat(paste0(all_warnings_code[i], ": ", as.integer(table(all_warnings$Code)[all_warnings_code[i]]), " times. ", all_warnings_message[i], "\n"))
    }
  } else {
    cat("Decryption succeeded without any warnings!\n")
  }

  return(data_decr)
}
