# LUMC ADM standard script to make a view in opal
#
# Date: 2024-03-13
# Author: Lars van der Burg
# Status: in development (Do not use)
#
# Last modified: 2024-01-13
# Last modifications: added a check to warn for possible parse error when non-verified var dict variables are not of class character
#
#' @title Making and updating views in opal
#'
#' @description Important for making a view in opal is the script column that is made in the var dictionary. Otherwise the view will be empty.
#'
#' @param opal string. A working opalr::opal_login where the original table is saved
#' @param projname string. Opal project name of the original table
#' @param tablename string. Opal table name of the original table
#' @param opal_view string. A working opalr::opal_login where the view will be or is saved
#' @param projname_view string. Opal project name of the view
#' @param tablename_view string. Opal table name of the view
#' @param var tibble. Var dictionary to assign to the view
#' @param cat tibble. Cat dictionary to assign to the view
#' @param update boolean. Whether or not the function should update or make a view with the supplied var and cat dictionary
#' @param comp_key string. The name of the column representing the entity identifiers. Default is 'id'
#' @param ent string. EntityType used in the datafile.
#' @param source list. List of the table names that will be referred to by the view. If NULL, will be combination of projname and tablename
#' @param EntityFilter string. The additional filter supplied to the view. Can be build up in two ways: the required format: $this('group').eq('A') or a convenient format: c("group" = "A")
#' @param report_path boolean. Indicating where to save the diffdf report. If NULL (default) report will only be returned and not saved.
#' @param report_name string. Name of the report to save. A sys.Date() is always added to the report_name.
#' @param max_tries integer. The number of times R will try to read a datafile from, or write a datafile to, opal.
#'
#' @return Optionally returns a diffdf report comparing the table supplied with `opal`, `projname` and `tablename` with the view supplied with `opal_view`, `projname_view` and `tablename_view`.
#'
#' @note
#' Important that a column script and a column entityType is present in the var dictionary. Otherwise it will not run!
#' This function does not handle entity filters! So keep that in mind when altering something with this function
#' Currently (25/06) it is not possible to downsize a view (e.g., from 5 --> 4 variables). Therefore, independent of update, the view will be created from scratch
#' It is possible that the view breaks down when the original datafile is altered. The view can then not be imported, an error is catched and the requested diffdf is turned off.
#' For columns in the var dictionary that are not verified the class can only be character, otherwise a parse error will occur (https://git.lumc.nl/opal/public/adm_template_scripts/-/issues/1)
#'
#'
#' @import opalr
#' @import tidyr
#'
#' @author Lars van der Burg
#'
#' @export
make_opal_view <- function(opal, projname, tablename, opal_view = NULL, projname_view = NULL, tablename_view, var, cat = NULL, update = FALSE, comp_key = "id", ent = NULL, source = NULL, EntityFilter = NULL,
                           report_path = FALSE, report_name = "Report", comparison = "mod", max_tries = 3, ...){


# Checks ------------------------------------------------------------------
  ## Check whether all essential arguments are specified when the function is called
  if(missing(opal)){stop("Make sure that argument opal is specified, it's essential")}
  if(missing(projname)){stop("Make sure that argument projname is specified, it's essential")}
  if(missing(tablename)){stop("Make sure that argument tablename is specified, it's essential")}
  if(missing(tablename_view)){stop("Make sure that argument tablename_view is specified, it's essential")}


  ## If arguments are NULL, they come from other arguments
  if(is.null(opal_view)){
    opal_view = opal
  }
  if(is.null(projname_view)){
    projname_view = projname
  }


  ## Check if there is an entityType
  if(is.null(ent) & !("entityType" %in% colnames(var))){stop("Make sure that you supply an entityType via argument ent or in the var dictionary")}


  ## Check whether the table on which the view is based exists
  ### Because views cannot be downsized, they are always build from scratch
  if(isFALSE(opalr::opal.table_exists(opal = opal, project = projname, table = tablename))){
    stop("The table where you want to make a view of does not exist")
  }


  ## Check whether view does not already exist if update = FALSE, if so --> update = TRUE
  if(isFALSE(update) & opalr::opal.table_exists(opal = opal_view, project = projname_view, table = tablename_view)){
    update = TRUE

    warning("update was FALSE but the table existed, so set update to TRUE\n")
  }


  ## Check whether the diffdf comparison can be saved
  if(!isFALSE(report_path) & !is.null(report_path)){
    if(isFALSE(is.character(report_path)) || isFALSE(dir.exists(report_path))){
      stop("The report_path where you want to save the diffdf report doesn't exist. Please supply a valid location.\n If you don't want to save the diffdf report, set report_path = NULL.")
    }
  }


  ## Check whether there is no column called table, views cannot handle that
  if("table" %in% var$name){
    stop("There is a column called `table` in the dictionary. Views cannot handle that. Please rename or discard.")
  }


  ## Check if dictionary is compatible with the data
  ### Only verified attributes (columns) in the var dictionary can be non-characters, otherwiste get "ParseException, 1:179: Expected string"
  cnames = unlist(lapply(strsplit(colnames(var), ":"), \(x){x[1]}))
  cnames_verified = c("name", "valueType", "entityType", "index", "unit", "min", "max", "label", "description", "repeatable")
  check_cnames = names(which(!sapply(cnames, \(x){x %in% cnames_verified})))
  if(length(check_cnames) > 0){
    check_cnames_class = names(which(sapply(check_cnames, \(x){class(var[[x]]) != "character"})))

    if(length(check_cnames_class) > 0){
      warning(paste0("Possibly there will be an error ('ParseException, 1:xx: Expected string') when creating this view. This is possibly because column(s): ",
                     paste(check_cnames_class, collapse = "; "),
                     " of the var dictionary are not of the class character. Opal cannot deal with that. Change that class to character and proceed.\n"))
      cat("If there is no error or if the error persists after adjusting the dictionary, contact ADM and we will try to investigate further.\n")
    }
  }


  ## Check if we can work with the EntityFilter
  ### There are two options that someone can supply an EntityFilter: 1) $this('group').eq('A') or 2) c("group" = "A")
  if(!is.null(EntityFilter)){
    if(length(EntityFilter) == 1 && str_starts(EntityFilter, "\\$this\\(")){
      ET_var = str_extract(EntityFilter, "(?<=\\(').+(?='\\).eq)")
      ET_val = str_extract(EntityFilter, "(?<=.eq\\(').+(?='\\))")
    } else if(length(EntityFilter) == 1 && !is.null(names(EntityFilter))){
      ET_var = names(EntityFilter)
      ET_val = EntityFilter[[1]]

      EntityFilter = paste0("$this('", ET_var, "').eq('", ET_val, "')")
    } else {
      stop("The EntityFilter is not in correct format, adjust it or remove it to continue.\n")
    }

    if(isFALSE(ET_var %in% (datafile |> colnames()))){
      stop("The variable of the EntityFilter is not supplied to the datafile")
    }
    if(isFALSE(ET_val %in% (datafile |> pull(ET_var)))){
      stop("The value of the EntityFilter is not present in the datafile")
    }
  }



# Initializations ---------------------------------------------------------
  var = var |>
    filter(name != {{ comp_key }})  # {{ comp_key }} is embracing of the variable. This ensures that tidyverse allows changing comp_key

  if(!("script" %in% colnames(var))){
    var = var |>
      mutate(script = paste0("$('", name, "')"))
  }

  if(is.null(ent)){
    ent <- var %>% select(entityType) %>% distinct() |> pull()
  } else {
    var = var |>
      mutate(entityType = ent) |>
      relocate(entityType, .before = script)
  }



# Comparions - pre --------------------------------------------------------
  ## Already read in the base comparison, because when update = TRUE, the old view will disappear but want it for the compare
  if(is.null(report_path) || report_path != FALSE){  # When report_path == FALSE, the diffdf is not created

    if(isTRUE(update)){
      imported_datafile <- tryCatch({
        import_table_opal2R(opal_view, projname_view, tablename_view, id.name = comp_key, max_tries = max_tries)},
          error = function(e){e$message})

      if(is_list(imported_datafile)){
        datafile_base <- imported_datafile$datafile4copy
        var_base <- imported_datafile$var4copy
        cat_base <- imported_datafile$cat4copy
      } else {
        cat("The current view is probably deprecated (changed the dictionary of original table?), so no diffdf comparison is made.\n")
        report_path = FALSE
      }

    } else {
      imported_datafile <- import_table_opal2R(opal, projname, tablename, id.name = comp_key, max_tries = max_tries)

      datafile_base <- imported_datafile$datafile4copy
      var_base <- imported_datafile$var4copy
      cat_base <- imported_datafile$cat4copy
    }
  }



# View --------------------------------------------------------------------
  if(isTRUE(update)){
    all_perms = opalr::opal.table_perm(opal_view, projname_view, tablename_view)

    cat("Because some difficulties with updating the dictionary of a view, the view needs to be deleted before making a new one\n\n")
    delete_table_opal(opal_view, projname_view, tablename_view, ...)
  }

  if(is.null(source)){
    source = list(paste0(projname, ".", tablename))
  }

  ## No differences are observed between use of opal.table_create and opal.table_view_create
  opalr::opal.table_view_create(opal_view, projname_view, tablename_view, type = ent, tables = source)
  opalr::opal.table_dictionary_update(opal_view, projname_view, tablename_view, var, cat)


  ## remove your own permissions for the table (if you can make a table, you have to have project rights)
  # opalr::opal.table_perm(opal_view, projname_view, tablename_view)
  opal.table_perm_delete(opal_view, projname_view, tablename_view, subject = opal$username)


  if(isTRUE(update) && nrow(all_perms) != 0){
    for(i in 1:nrow(all_perms)){
      opalr::opal.table_perm_add(opal_view, projname_view, tablename_view, subject = all_perms$subject[i], type = all_perms$type[i], permission = all_perms$permission[i])
    }
  }


  ## Apply EntityFilter
  if(!is.null(EntityFilter)){
    opalr::opal.table_view_update(opal_view, projname_view, tablename_view, where = EntityFilter)
  }



# Comparison --------------------------------------------------------------
  if(is.null(report_path) || report_path != FALSE){  # When report_path == FALSE, the diffdf is not created

    datafile <- datafile_base
    var <- var_base
    cat <- cat_base

    # if(!is.null(EntityFilter)){
    #   datafile = datafile |> filter(!!sym(ET_var) == ET_val)
    # }


    imported_datafile2 <- import_table_opal2R(opal_view, projname_view, tablename_view, id.name = comp_key, max_tries = max_tries)

    datafile2 <- imported_datafile2$datafile4copy
    var2 <- imported_datafile2$var4copy
    cat2 <- imported_datafile2$cat4copy


    report = check_diffdf_opal_generic(datafile = datafile, datafile2 = datafile2, var = var, var2 = var2, cat = cat, cat2 = cat2, suppress_warnings = TRUE,
                                       report_path = report_path, report_name = report_name, comparison = comparison)

    return(report)
  }
}
