
#' Function to find differences in data
#'
#' @param dataframe1 dataframe
#' @param dataframe2 dataframe
#' @param path path to output dir
#' @param keys vector of variables (as strings) that defines a unique row in the base and compare dataframes
#' 
#' @import diffdf 
#' 
#' @return findings, dataframe with differences (if there are any)
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


#' Function to check datafile, variables & categories in one go
#'
#' @param datalist1 List object: list(datafile = datafile, categories = categories, variables = variables)
#' @param datalist2 List object: list(datafile = datafile, categories = categories, variables = variables)
#' 
#' @return findings, dataframe with differences (if there are any)
#' 
#' @export

adm.complete_diffdf <- function(datalist1, datalist2, ...) {
  ## Create diffdf output list
  findings <- list()
  
  ## Run diffdf for datafile
  findings[["datafile"]] <- adm.diffdf(
    datafile1 = datalist1$datafile,
    datafile2 = datalist2$datafile,
    keys = colnames(datalist1$datafile)[1],
    ...
  )
  
  ## Run diffdf for variables
  findings[["variables"]] <- adm.diffdf(
    datafile1 = datalist1$variables,
    datafile2 = datalist2$variables,
    keys = "name",
    ...
  )
  
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
