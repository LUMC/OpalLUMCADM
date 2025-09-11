# LUMC ADM standard script to import a table from opal to R
#
# Date: 2024-03-07
# Author: Lars van der Burg
# Status: in development
#
# Last modified: 11-06-2024
# Modified by: Lars van der Burg
# Last modifications: added variable id.name, to also make it possible to retrieve a datafile with a different id.name
#
# Checked by: Richard Wissels
# Checked on: 2024-04-23
#
#' @title Function to read a table from opal to R
#'
#' @description Function that makes an opal ready for use in R. For this also some attributes/characteristics of the datafile and dictionary need to be altered
#'
#' @param opal a working opalr::opal_login
#' @param projname Origin opal project name
#' @param tablename Origin opal table name
#' @param id.name string. The name of the column representing the entity identifiers. Default is 'id'
#' @param max_tries integer. The number of times R will try to read a datafile from, or write a datafile to, opal.
#'
#' @return a list with the datafile and dictionary as retrieved from opal and the datafile and dictionary updated for use in R
#'
#' @import opalr dplyr
#'
#' @author Lars van der Burg
#'
#' @export
import_table_opal2R = function(opal, projname, tablename, id.name = "id", max_tries = 3){


# Checks ------------------------------------------------------------------
## Check whether all essential arguments are specified when the function is called
  if(missing(opal)){stop("Make sure that argument opal is specified, it's essential")}
  if(missing(projname)){stop("Make sure that argument projname is specified, it's essential")}
  if(missing(tablename)){stop("Make sure that argument tablename is specified, it's essential")}


## Check that we can only get one table from one project
  if(length(projname) != 1 | length(tablename) != 1){
    stop("Make sure that both projname and tablename only have 1 input, this script can only import one table at the time for one project at the time")
  }


## Check if there are spaces in the table name, tables with spaces in name cannot be saved into opal.
  if(str_count(tablename, " ") > 0){
    stop("There are spaces in your tablename, that cannot be saved to opal")
  }


## Check whether the table exists
  if(isFALSE(opalr::opal.table_exists(opal, projname, tablename))){
    stop("The table you want to import does not exists (in this project)")
  }





# Import data and dictionary ----------------------------------------------
## get table from opal
  if(max_tries > 10 | max_tries <= 0){
    cat("You have selected max_tries > 10 or below/equal to 0, that is not allowed. I have set it back to 3.")
    max_tries = 3
  }

  table_imported = FALSE; tries = 1
  while(!isTRUE(table_imported) & tries <= max_tries){

    datafile = tryCatch({
      opalr::opal.table_get(opal, project = projname, table = tablename, id.name = id.name) |>
        as_tibble() |> arrange(!!!syms(id.name))
    },
    error = function(e){e$message},  # Question: want something specific with [Client error: (403) Forbidden]?
    finally = function(f){TRUE})

    if(is_tibble(datafile)){
      table_imported = TRUE

    } else {
      cat("The table was not imported on try", tries, "with the following error message:\n  ", datafile, "\n\n")

      tries = tries + 1
      Sys.sleep(3)
    }
  }

  if(isFALSE(table_imported)){
    cat("Could not import data, so abort function.\n")
    return(NULL)
  }


## get dictionary from opal
  dict = opal.table_dictionary_get(opal, project = projname, table = tablename)
  var = dict$variables |> as_tibble()
  cat = dict$categories |> as_tibble(); if(is.null(cat) || nrow(cat) == 0){cat = NULL}




# Adjust data and dictionary ----------------------------------------------
  datafile4copy = datafile
  var4copy = var
  cat4copy = cat


## Change date(time) variables into text (NB: this action removes the attributes from these date variables)
## format will be changed into character for these columns: (shows "named logical(0)" when none)
  datecolumns = sapply(datafile4copy, function(x){inherits(x, "Date") || inherits(x, "POSIXt")})
  datafile4copy[datecolumns] = map(datafile4copy[datecolumns], as.character)

## If attribute removal is executed before changed classes things go wrong (dates become values)
  datafile4copy = lapply(datafile4copy, function(x){attributes(x) = NULL; x}) |> as_tibble()


## Remove attributes calculated by Opal from var dictionary
### entityType: if not deleted you get additional dots for entity search in your value view
  var4copy$na_values = NULL
  var4copy$entityType = NULL
  var4copy$referencedEntityType = NULL
  var4copy$mimeType = NULL
  var4copy$occurrenceGroup = NULL


## Change "missing" from BOOLEAN to NUMERIC, to always work properly
  if(isFALSE(is.null(cat)) && is.logical(cat4copy$missing)){
    cat4copy$missing = as.numeric(cat4copy$missing)
  }




  return(list(datafile = datafile, var = var, cat = cat,
              datafile4copy = datafile4copy, var4copy = var4copy, cat4copy = cat4copy))
}
