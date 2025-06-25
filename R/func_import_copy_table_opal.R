# LUMC ADM standard script to copy a single tables
#
# Date: 2024-03-07
# Author: Lars van der Burg
# Status: in development
#
# Last modified: 2025-06-11
# Last modified by: Lars van der Burg
# Last modifications: discarded check if nrow(cat4copy/cat) == 0. Is retrieved from import_table_opal2R and there they are already made NULL
#
#
# Checked by: Richard Wissels
# Checked on: 2024-05-22
#
#' @title Copy an existing opal table to a new location in opal
#'
#' @description Because it is not possible to copy a table in opal, the table is imported from opal into R and than written to the new location in opal
#'
#' @param opal string. A working opalr::opal_login.
#' @param projname string. Origin opal project name.
#' @param tablename string. Origin opal table name.
#' @param opal2 optional string. A working opalr::opal_login. If absent, will use `opal`.
#' @param projname2 string. Destination opal project name. If absent, will use `projname`.
#' @param tablename2 string. Destination opal table name. If absent, will use `tablename`.
#' @param ent string. EntityType used in the datafile. If absent, will use entityType from the imported var dictionary.
#' @param id.name string. The name of the column representing the entity identifiers. Default is 'id'
#' @param max_tries integer. The number of times R will try to read a datafile from, or write a datafile to, opal.
#' @param report_path string. Indicating where to save the diffdf report. If NULL (default) report will only be returned.
#' @param report_name sring. The name of the report for saving. A sys.Date() is always added to the report_name.
#' @param comparison string. Which comparison to run, there are three options: c("base", "mod", "both"). The base comparison compares the unadjusted datafile and dictionary, the mod comparison (default) compares the modified datafile and dictionary and the both comparison performes both.
#'
#' @return An optionally diffdf report. The diffdf report compares the original datafile/dictionary with the newly copied datafile/dictionary in opal.
#'
#' @import opalr
#' @import dplyr
#'
#' @author Lars van der Burg, Thekla Jansen & Kristel Schaap
#'
#' @export
import_copy_table_opal = function(opal, projname, tablename, opal2 = NULL, projname2 = NULL, tablename2 = NULL, ent = NULL,
                                  report_path = FALSE, report_name = "Report", comparison = "mod", id.name = "id", max_tries = 3, ...){


# Checks ------------------------------------------------------------------
## Check whether all essential arguments are specified when the function is called
  if(missing(opal)){stop("Make sure that argument opal is specified, it's essential")}
  if(missing(projname)){stop("Make sure that argument projname is specified, it's essential")}
  if(missing(tablename)){stop("Make sure that argument tablename is specified, it's essential")}


## Check that the table is saved somewhere else
### It can have the same name, same project or same opal session, but all three cannot be the same
  if((is.null(opal2) || identical(opal, opal2)) && (is.null(projname2) || projname == projname2) && (is.null(tablename2) || tablename == tablename2)){
    stop("You cannot copy a table with the same tablename in the same project in the same opal session. Supply a difference")
  }


## Check if we can make an unambiguous copy
  if(is.null(tablename2)){
    tablename2 = tablename
  } else if(length(tablename) != 1 | length(tablename2) != 1){
    stop("Make sure that you have only 1 table in both tablename and tablename2")
  }
  if(is.null(projname2)){
    projname2 = projname
  } else if(length(projname) != 1 | length(projname2) != 1){
    stop("Make sure that you only have 1 project name in both projname and projname2")
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




# Import table ------------------------------------------------------------
  imported_table = import_table_opal2R(opal, projname, tablename, id.name = id.name, max_tries = max_tries)

  datafile4copy = imported_table$datafile4copy
  var4copy = imported_table$var4copy
  cat4copy = imported_table$cat4copy  # ; if(nrow(cat4copy) == 0){cat4copy = NULL}



# EntityType --------------------------------------------------------------
## Get entityType
  if(is.null(ent)){
    ent = imported_table$var |> select(entityType) |> distinct() |> pull()
  }



  # Write copy --------------------------------------------------------------
  write_table_R2opal(opal = opal2, projname = projname2, tablename = tablename2,
                     datafile = datafile4copy, var = var4copy, cat = cat4copy, ent = ent,
                     action = "write", id.name = id.name, max_tries = max_tries, ...)



  # Diffdf report -----------------------------------------------------------
  if(is.null(report_path) || report_path != FALSE){  # When report_path == FALSE, the diffdf is not created

    datafile = imported_table$datafile4copy
    var = imported_table$var4copy
    cat = imported_table$cat4copy; if(is.null(cat) || nrow(cat) == 0){cat = NULL}


    imported_table2 = import_table_opal2R(opal2, projname2, tablename2)

    datafile2 = imported_table2$datafile4copy
    var2 = imported_table2$var4copy
    cat2 = imported_table2$cat4copy; if(is.null(cat2) || nrow(cat2) == 0){cat2 = NULL}


    report_title = "This report compares the old version (BASE) of the datafile/dictionary on opal with the moved version (COMPARE) of the datafile/dictionary on opal"

    report = check_diffdf_opal_generic(datafile = datafile, datafile2 = datafile2, var = var, var2 = var2, cat = cat, cat2 = cat2, suppress_warnings = TRUE,
                                       report_path = report_path, report_name = report_name, report_title = report_title, comparison = comparison)

    return(report)
  }
}
