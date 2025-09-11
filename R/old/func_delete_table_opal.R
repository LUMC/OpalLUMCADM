#' @title Function to delete a table from opal
#'
#' @description To delete a table from opal. Because of sensitivity of operation, an extra confirmation is required.
#'
#' @param opal string. A working opalr::opal_login
#' @param projname string. Origin opal project name
#' @param tablename string. Origin opal table name
#'
#' @examples
#' It is possible to also delete multiple tables at once:
#' tables <- c("TABLE1", "TABLE2", "TABLE3")
#' for(table in tables){
#'   delete_table_opal(opal, projname, table)
#' }
#'
#' @import opalr
#'
#' @author Lars van der Burg & Thekla Jansen
#'
#' @export
delete_table_opal <- function(opal, projname, tablename, ...){


# Checks ------------------------------------------------------------------
  ## Check whether the table exists
  if(isFALSE(opalr::opal.table_exists(opal, projname, tablename))){
    stop(paste0("The table ", tablename, " you want to delete does not exists (at least in project ", projname, ")"))
  }


  ## Circumvent the 'child_lock'
  child_lock = TRUE
  extra_args <- list(...)
  if("child_lock" %in% names(extra_args) && extra_args[["child_lock"]] == FALSE){
    child_lock = FALSE
    # cat("You have shut the child_lock off, now you don't need to confirm the actual deletion\n")
  }



# Confirmation ------------------------------------------------------------
## Show server, project name and table name for additional checking of correct actions
## Type in conformation
  if(child_lock){
    cat("You are running on the following server:\n"); print(opal)
    cat("\nYou want to delete table", tablename, "from the project", projname, "\n")

    confirm = "no"
    confirm = readline("Please type 'yes' - IN THE CONSOLE - to confirm the actual deletion: ")
  } else {
    confirm = "yes"
  }



  # Delete ------------------------------------------------------------------
  if(isTRUE(confirm == "yes")){
    opalr::opal.table_delete(opal, projname, tablename, silent = FALSE)
  } else {
    cat("\n\nBecause you didn't confirm (didn't enter yes), the table is not deleted and the function is aborted\n")
  }
}
