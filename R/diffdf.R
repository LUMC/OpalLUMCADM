
#' Function to find differences in data
#'
#' @param dataframe1 dataframe
#' @param dataframe2 dataframe
#' @param path path to output dir
#' @param keys vector of variables (as strings) that defines a unique row in the base and compare dataframes
#' 
#' @import diffdf 
#' 
#' @return difference, dataframe with differences (if there are any)
#' 
#' @export

adm.diffdf <- function(datafile1, datafile2, keys = NULL, ...) {
  ## Remove attributes
  datafile1 <- datafile1 |> dplyr::mutate(across(everything(), as.character))
  datafile2 <- datafile2 |> dplyr::mutate(across(everything(), as.character))
  
  ## Get differences
  difference <- diffdf(
    base = datafile1,
    compare = datafile2,
    keys = keys,
    ...
  )
  
  ## Merge all vardiffs
  difference[["VarDiff"]] <- do.call(rbind, difference[grep("VarDiff_", names(difference))])
  difference[grep("VarDiff_", names(difference))] <- NULL
  
  ## Return difference
  return(difference)
}
