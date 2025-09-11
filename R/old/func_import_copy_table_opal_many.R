#' @title Copy many opal tables to opal
#'
#' @description Wrapper for \code{import_copy_table_opal}. If not all tables can be moved (for example, because there is already a table in the new location), that table will be skipped
#'
#' @param opal string. A working opalr::opal_login
#' @param projnames vector of string. Origin opal project names for each table. Can be either 1 string (which will then be repeated for each table) or have a string per table
#' @param tablenames vector of string. Origin opal table names for each table.
#' @param opal2 optional string. A working opalr::opal_login. If absent, will use 'opal'.
#' @param projnames2 vector of string. Destination opal project names for each table. Can be either 1 string (which will then be repeated for each table) or have a string per table
#' @param tablenames2 vector of string. Destination opal table names for each table.
#' @param ent string. EntityType used in the datafile. If absent, will use entityType from the imported var dictionary.
#' @param id.name string. The name of the column representing the entity identifiers. Default is 'id'
#' @param max_tries integer. The number of times R will try to read a datafile from, or write a datafile to, opal.
#' @param report_path string. Indicating where to save the diffdf report. If NULL (default) report will only be returned.
#' @param report_name string. The name of the report for saving. A tablename and sys.Date() is always added to the report_name.
#' @param comparison string. Which comparison to run, there are three options: c("base", "mod", "both"). The base comparison compares the unadjusted datafile and dictionary, the mod comparison (default) compares the modified datafile and dictionary and the both comparison performes both.
#'
#' @return An optionally diffdf report for each copied table. The diffdf report compares the original datafile/dictionary with the newly copied datafile/dictionary in opal.
#'
#' @import opalr
#'
#' @author Lars van der Burg
#'
#' @export
import_copy_table_opal_many <- function(opal, projnames, tablenames, opal2 = NULL, projnames2, tablenames2, ent = NULL,
                                        report_path = FALSE, report_name = "Report", comparison = "mod", id.name = "id", max_tries = 3, ...){


# Checks ------------------------------------------------------------------
## Check whether all essential arguments are specified when the function is called
  if(missing(opal)){stop("Make sure that argument opal is specified, it's essential")}
  if(missing(projnames)){stop("Make sure that argument projnames is specified, it's essential")}
  if(missing(tablenames)){stop("Make sure that argument tablenames is specified, it's essential")}
  if(missing(projnames2)){stop("Make sure that argument projnames2 is specified, it's essential")}
  if(missing(tablenames2)){stop("Make sure that argument tablenames2 is specified, it's essential")}


## Ensure that copies can be made unambiguously
  if(length(tablenames) != length(tablenames2)){
    stop("Make sure that tablenames and tablenames2 have an equal number of tables.")
  }
  if(length(projnames) != 1 & length(projnames) != length(tablenames)){
    stop("projnames can equal 1 or equal the number of tables, but now it is something else.")
  }
  if(length(projnames2) != 1 & length(projnames2) != length(tablenames)){
    stop("projnames2 can equal 1 or equal the number of tables, but now it is something else.")
  }
  if(is.null(opal2)){
    opal2 = opal
  }


## Ensure that diffdf can be saved
  if(!isFALSE(report_path) & !is.null(report_path)){
    if(isFALSE(is.character(report_path)) || isFALSE(dir.exists(report_path))){
      stop("The report_path where you want to save the diffdf report doesn't exist. Please supply a valid location.\n If you don't want to save the diffdf report, set report_path = NULL.")
    }
  }



# Initializations ---------------------------------------------------------
  N = length(tablenames)

  if(length(projnames) == 1){
    projnames = rep(projnames, N)}
  if(length(projnames2) == 1){
    projnames2 = rep(projnames2, N)}


  reports = NULL
  for(i in 1:N){
    if(!is.null(opal$token)){
      opal = opalr::opal.login(url = opal$url, token = opal$token)
    }

    projname = projnames[i]; projname2 = projnames2[i]
    tablename = tablenames[i]; tablename2 = tablenames2[i]

    ## tryCatch is added to ensure that if one table can't be copied (because it is already there), the rest is still copied
    reports[[i]] = tryCatch({import_copy_table_opal(opal, projname, tablename, opal2, projname2, tablename2, ent = ent, id.name = id.name, max_tries = max_tries,
                                                    report_path = report_path, report_name = paste(report_name, tablename2, sep = "_"), comparison = comparison, ...)},
                            error = function(e){
                              message(paste0("There is an error with table ", tablename, ":   ", conditionMessage(e)))
                              message("\nThis table is therefore not copied, and we continue with the rest")
                            })

  }

  if(!isFALSE(report_path) || is.null(report_path)){
    names(reports) = tablenames2

    return(reports)
  }
}
