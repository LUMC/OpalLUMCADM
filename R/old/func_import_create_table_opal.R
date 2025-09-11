# LUMC ADM standard script to create a table in opal
#
# Date: 2024-03-07
# Author: Lars van der Burg
# Status: in development
#
# Last modified: 2025-06-11
# Last modified by: Lars van der Burg
# Last modifications: added id.name variable to function
#
# Checked by: Richard Wissels
# Checked on: 2024-05-27
#
# @note Not part of this script is:
# - encrypt identifying variables if applicable, using rtres package
# - in case of encrypted variables, set opal valueType to text
#
#' @title Create a new table or update an existing table in opal
#'
#' @description Create a new table in opal and afterwards make the comparison between the supplied datafile and dictionary with the datafile and dictionary imported from opal. If a table is updated, the diffdf comparison will be between the old and new version of the opal table.
#'
#' @param opal string. A working opalr::opal_login
#' @param projname string. Origin opal project name
#' @param tablename string. Origin opal table name
#' @param datafile tibble. Containing the dataset with for each column the variables described by \code{var}. The datafile that will be supplied to \code{opalr::dictionary.apply()}.
#' @param var tibble. Containing the variable dictionary for datafile. The dictionary that will be supplied to \code{opalr::dictionary.apply()}.
#' @param cat tibble. Containing the category dictionary for datafile. The dictionary that will be supplied to \code{opalr::dictionary.apply()}.
#' @param ent string. EntityType used in the datafile.
#' @param action string. Whether or not just to save the datafile (write, is the default), update an existing table (update) or overwrite an existing table (overwrite).
#' @param id.name string. The name of the column representing the entity identifiers. Default is 'id'
#' @param max_tries integer. The number of times R will try to read a datafile from, or write a datafile to, opal.
#' @param report_path string. Indicating where to save the diffdf report. If NULL (default) report will only be returned.
#' @param report_name sring. The name of the report for saving. A sys.Date() is always added to the report_name.
#' @param comparison string. Which comparison to run, there are three options: c("base", "mod", "both"). The base comparison compares the unadjusted datafile and dictionary, the mod comparison (default) compares the modified datafile and dictionary and the both comparison performes both.
#'
#' @return An optionally diffdf report. The diffdf report is either between the supplied datafile/dictionary and the imported datafile/dictionary from opal or between the old and updated table from opal.
#'
#'
#' @author Lars van der Burg, Thekla Jansen & Kristel Schaap
#'
#' @export
import_create_table_opal <- function(opal, projname, tablename, datafile, var, cat = NULL, ent = NULL, action = "write", id.name = "id",
                                     report_path = FALSE, report_name = "Report", comparison = "mod", max_tries = 3, ...){


# Checks ------------------------------------------------------------------
## Check whether all essential arguments are specified when the function is called
  if(missing(opal)){stop("Make sure that argument opal is specified, it's essential")}
  if(missing(projname)){stop("Make sure that argument projname is specified, it's essential")}
  if(missing(tablename)){stop("Make sure that argument tablename is specified, it's essential")}
  if(missing(datafile)){stop("Make sure that argument datafile is specified, it's essential")}
  if(missing(var)){stop("Make sure that argument var is specified, it's essential")}


## Check if everything is a tibble
  if(isFALSE(is_tibble(datafile))){stop("The datafile is not a tibble, provide datafile, var and cat as tibble.")}
  if(isFALSE(is_tibble(var))){stop("The var dictionary is not a tibble, provide datafile, var and cat as tibble.")}
  if(!is.null(cat)){
    if(isFALSE(is_tibble(cat))){stop("The cat dictionary is not a tibble, provide datafile, var and cat as tibble.")}
  }


## Ensure that diffdf can be saved
  if(!isFALSE(report_path) & !is.null(report_path)){
    if(isFALSE(is.character(report_path)) || isFALSE(dir.exists(report_path))){
      stop("The report_path where you want to save the diffdf report doesn't exist. Please supply a valid location.\n If you don't want to save the diffdf report, set report_path = NULL.")
    }
  }


## Re-write the action argument into force and overwrite
  if(action == "write"){
    force = overwrite = FALSE
  } else if(action == "update"){
    force = TRUE; overwrite = FALSE
  } else if(action == "overwrite"){
    force = overwrite = TRUE
  } else {
    stop("The action argument can only have three options: write, update or overwrite.")
  }



## Additional checks will be executed by write_table_R2opal and import_table_opal2R



# Importing table ---------------------------------------------------------
## Already read in the base comparison, because when action = update/overwrite, the old datafile will be updated and thus disappear
  if(action != "write" && isTRUE(is.null(report_path) || report_path != FALSE)){  # When report_path == FALSE, the diffdf is not created

    imported_datafile <- import_table_opal2R(opal, projname, tablename, id.name = id.name, max_tries = max_tries)

    datafile_base <- imported_datafile$datafile4copy
    var_base <- imported_datafile$var4copy
    cat_base <- imported_datafile$cat4copy
  }



# Write table -------------------------------------------------------------
  write_table_R2opal(opal, projname, tablename, datafile, var, cat, ent = ent, action = action, id.name = id.name, max_tries = max_tries, ...)



# Diffdf report -----------------------------------------------------------
  if(is.null(report_path) || report_path != FALSE){  # When report_path == FALSE, the diffdf is not created

    if(action != "write"){
      datafile = datafile_base
      var = var_base
      cat = cat_base

      report_title = "This report compares the old version (BASE) of the datafile/dictionary on opal with the updated version (COMPARE) of the datafile/dictionary"
    } else {
      report_title = "This report compares the supplied version (BASE) of the datafile/dictionary via the argument with the created table (COMPARE) of the datafile/dictionary on opal"
    }


    imported_table = import_table_opal2R(opal, projname, tablename, id.name = id.name)

    datafile2 = imported_table$datafile4copy
    var2 = imported_table$var4copy
    cat2 = imported_table$cat4copy


    report = check_diffdf_opal_generic(datafile = datafile, datafile2 = datafile2, var = var, var2 = var2, cat = cat, cat2 = cat2, suppress_warnings = TRUE,
                                       report_name = report_name, report_path = report_path, report_title = report_title, comparison = comparison, ...)

    return(report)
  }
}
