
#' Function to find differences in data
#'
#' @param dataframe1 dataframe
#' @param dataframe2 dataframe
#' @param path path to output dir
#' 
#' @import diffdf 
#' 
#' @export

adm.check_diffdf <- function(datafile1, datafile2, path = NA, ...) {
  ## Get differences
  difference <- diffdf(
    base = datafile1,
    compare = datafile2
  )
  
  ## Merge all vardiffs
  difference[["VarDiff"]] <- do.call(rbind, difference[grep("VarDiff_", names(difference))])
  difference[grep("VarDiff_", names(difference))] <- NULL
  
  ## Return difference if no path is set
  if (is.na(path)) {
    return(difference)
  }
  
  ## Write to Excel
  adm.write_to_excel(
    df_list = difference,
    path = path,
    ...
  )
}


#' Function to write diffdf to excel
#'
#' @param df_list list of dataframes from diffdf
#' @param path path to output dir
#' 
#' @import openxlsx
#' 
#' @export

adm.write_to_excel <- function(df_list, path, ...) {
  ## Create a new workbook
  wb <- createWorkbook()
  
  ## Add each dataframe as a sheet
  for (sheet_name in names(df_list)) {
    addWorksheet(wb, sheet_name)
    writeData(wb, sheet = sheet_name, df_list[[sheet_name]])
  }
  
  ## Save workbook
  saveWorkbook(
    wb = wb,
    file = path,
    ...
  )
}
