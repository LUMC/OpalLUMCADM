# LUMC ADM standard script to write a table from R to opal
#
# Date: 2024-03-07
# Author: Lars van der Burg
# Status: in development
#
# Last modified: 2024-05-28
# Modified by: Lars van der Burg
# Last modifications: remove user permissions for the table after creating it. Independent of overwrite/force, table permissions are added for the user
#
# Checked by: Richard Wissels
# Checked on: 2024-04-23
#
#' @title Function to write a table from opal to R
#'
#' @description Function that writes a R table to opal. It is possible to force a table update
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
#'
#' @return list with the datafile and dictionary downloaded from opal after writing it to opal
#'
#' @note
#' Tables can only be written to a project where an user has project permissions. With the creation or updating of a table (independent of chosen options overwrite or force) there will be given table permissions rights for the user to the table. Since the user already has project permissions, these table permissions are discarded.
#'
#' @import opalr
#'
#' @author Lars van der Burg
#'
#' @export
write_table_R2opal = function(opal, projname, tablename, datafile, var, cat = NULL, ent = NULL, action = "write", id.name = "id", max_tries = 3, ...){


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


## Check that we can only get one table from one project
  if(length(projname) != 1 | length(tablename) != 1){
    stop("Make sure that both projname and tablename only have 1 input, this script can only write one table at the time for one project at the time")
  }


## Check if there are spaces in the table name, tables with spaces in name cannot be saved into opal.
  if(str_count(tablename, " ") > 0){
    stop("There are spaces in your tablename, that cannot be saved to opal")
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


## Circumvent the 'child_lock'
  child_lock = TRUE
  extra_args <- list(...)
  if("child_lock" %in% names(extra_args) && extra_args[["child_lock"]] == FALSE){
    child_lock = FALSE
    cat("You have shut the child_lock off, now you don't need to confirm the actual deletion\n")
  }


## Check that when the table already exists, you can update it and want to update it
## If the table does not yet exits, force can be TRUE or FALSE, does not matter
  if(isTRUE(opalr::opal.table_exists(opal, projname, tablename)) & isFALSE(force)){
    stop("The table already exists and you don't want to overwrite, I cannot do this")

  } else if(isTRUE(opalr::opal.table_exists(opal, projname, tablename)) & isTRUE(force)){

    if(child_lock){
      cat("You are running on the following server:\n"); print(opal)
      cat("\nBy setting force to TRUE, you want to overwrite table", tablename, "from project", projname, "\n")


      confirm = "no"
      confirm = readline("Please type 'yes' - IN THE CONSOLE - to confirm the actual deletion: ")
    } else {
      confirm = "yes"
    }

    if(confirm != "yes"){
      stop("You didn't supply yes, so abort the function.")
    }
  }


## Check if the entityType is supplied, or that the entityType can (and will be) obtained from the var dictionary
  if(is.null(ent) & isFALSE("entityType" %in% colnames(var))){
    stop("Supply an entityType in the var or with the parameter ent")
  } else if(is.null(ent) & "entityType" %in% colnames(var)){
    ent = var |> select(entityType) |> distinct() |> pull()
  }




# Apply dictionary --------------------------------------------------------
## Apply dictionary to dataframe in R. A cat dictionary is optional
  if(is.null(cat)){
    opaltable = opalr::dictionary.apply(tibble = datafile, variables = var) |> as_tibble()
  } else {
    opaltable = opalr::dictionary.apply(tibble = datafile, variables = var, categories = cat) |> as_tibble()
  }




# Save table to opal ------------------------------------------------------
  if(max_tries > 10 | max_tries <= 0){
    cat("You have selected max_tries > 10 or below/equal to 0, that is not allowed. I have set it back to 3.")
    max_tries = 3
  }

  table_saved = FALSE; tries = 1
  while(!isTRUE(table_saved) & tries <= max_tries){

    table_saved = tryCatch({
      opalr::opal.table_save(opal = opal, tibble = opaltable, project = projname, table = tablename,
                             overwrite = overwrite, force = force, identifiers = NULL, policy = "required", id.name = id.name, type = ent)
    },
    error = function(e){e$message},  # Question: want something specific with [Client error: (403) Forbidden]?
    finally = function(f){TRUE})

    if(!isTRUE(table_saved)){
      cat("The table was not imported on try", tries, "with the following error message:\n  ", table_saved, "\n\n")

      tries = tries + 1
      Sys.sleep(3)
    }
  }

## remove your own permissions for the table (if you can make a table, you have to have project rights)
  opal.table_perm_delete(opal, projname, tablename, opal$username)


}
