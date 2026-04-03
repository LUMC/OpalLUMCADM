
#' Compare Two Data Frames for Differences
#'
#' This function compares two data frames to identify differences in variables, values, and structure.
#'
#' @param datafile1 A data frame representing the base data.
#' @param datafile2 A data frame representing the data to compare against.
#' @param keys A character vector specifying the key columns used to match rows between data frames. If NULL, all columns are used as keys.
#' @param ... Additional arguments passed to underlying functions.
#'
#' @return A list containing the differences found between the two data frames, including variable-level and value-level differences.
#'
#' @import dplyr
#' @importFrom diffdf diffdf
#' 
#' @export

adm.diffdf <- function(datafile1, datafile2, keys = NULL, ...) {
  ## Remove attributes
  datafile1 <- datafile1 |> dplyr::mutate(across(everything(), as.character))
  datafile2 <- datafile2 |> dplyr::mutate(across(everything(), as.character))
  
  ## Ignore if NA==NA or NULL=NA
  datafile1 <- .clean_NA(datafile = datafile1)
  datafile2 <- .clean_NA(datafile = datafile2)
  
  ## Get differences
  findings <- diffdf(
    base = datafile1,
    compare = datafile2,
    keys = keys,
    ...
  )
  
  ## Merge all vardiffs
  findings[["VarDiff"]] <- do.call(rbind, findings[grep("VarDiff_", names(findings))])
  findings[grep("VarDiff_", names(findings))] <- NULL
  
  ## Return findings
  return(findings)
}


#' Compare Multiple Data Frame Lists for Comprehensive Differences
#'
#' This function performs a comprehensive comparison between two lists of data frames, including datafile, variables, and categories.
#'
#' @param datalist1 A list containing at least 'datafile' and optionally 'variables' and 'categories'.
#' @param datalist2 A list containing at least 'datafile' and optionally 'variables' and 'categories'.
#' @param ... Additional arguments passed to underlying functions.
#'
#' @return A list containing the differences found across datafile, variables, and categories.
#'
#' @import dplyr
#' @importFrom diffdf diffdf
#' 
#' @export

adm.complete_diffdf <- function(datalist1, datalist2, ...) {
  ## Create diffdf output list
  findings <- list()
  
  ## Run diffdf for datafile
  if (!is.null(datalist1$datafile) & !is.null(datalist2$datafile)) {
    findings[["datafile"]] <- adm.diffdf(
      datafile1 = datalist1$datafile,
      datafile2 = datalist2$datafile,
      keys = colnames(datalist1$datafile)[1],
      ...
    )
  }
  
  ## Run diffdf for variables
  if (!is.null(datalist1$variables) & !is.null(datalist2$variables)) {
    findings[["variables"]] <- adm.diffdf(
      datafile1 = datalist1$variables,
      datafile2 = datalist2$variables,
      keys = "name",
      ...
    )
  }
  
  ## Run diffdf for categories
  if (!is.null(datalist1$categories) & !is.null(datalist2$categories)) {
    findings[["categories"]] <- adm.diffdf(
      datafile1 = datalist1$categories,
      datafile2 = datalist2$categories,
      keys = c("variable", "name"),
      ...
    )
  }
  
  return(findings)
}
