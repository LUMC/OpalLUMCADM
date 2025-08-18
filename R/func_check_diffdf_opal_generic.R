#' @title Create diffdf overviews and a report (optionally in excel/csv). This function uses all the code from the script \code{Check_diffdf_opal_generic.R} with some small adjustments.
#'
#' @description Create a report with the differences between two opal datasets. Usually one that will uploaded to opal and the same dataset that is retrieved from opal
#'
#' @details In total 6 diffdf comparisons are made: datafile-datafile2; var-var2; cat-cat2; datafilemod-datafile2mod; varmod-var2mod and cat-cat2mod. Compared to the supplied tibbles, some small adjustments are made to the ...mod versions. The last three diffdfs are added to check some regular problems (e.g., changed variable class). Because of these adjustments, differences will of course be found. However, because one can see the type of difference and the origen of the difference, one can say whether it is a problem. Additionally, because of these adjustments, one can check whether the differences is created (not in the supplied tibble, but only in the ...mod version) or solved (present in the supplied tibble, but not in the ...mod version). This thus gives some insights into the severity of the difference.
#'
#' @param datafile tibble. The datafile, prior to uploading to opal. In general the BASE for the diffdf comparison.
#' @param datafile2 tibble. The datafile retrieved from opal. In general the COMPARE for the diffdf comparison.
#' @param var tibble. Variables of datafile. In general the BASE for the diffdf comparison.
#' @param var2 tibble. Variables of datafile2, retrieved from opal. In general the COMPARE for the diffdf comparison.
#' @param cat tibble. Categories of datafile. In general the BASE for the diffdf comparison.
#' @param cat2 tibble. Categories of datafile2, retrieved from opal. In general the COMPARE for the diffdf comparison.
#' @param comparison string. Which comparison to run, there are three options: c("base", "mod", "both"). The base comparison compares the unadjusted datafile and dictionary, the mod comparison (default) compares the modified datafile and dictionary and the both comparison performes both.
#' @param comp_key string. The key at which the datafile and datafile2 are compared, must be present in both
#' @param date_time_format vector. To calculate a difference between two (date) time variables. Need to know the possible formats these (date) times are written down.
#' @param report_path string. Folder where the report needs to be saved. If \code{NULL} the report is not saved but only returned.
#' @param report_name string. Name of the report to save. A sys.Date() is always added to the report_name.
#' @param report_title string. The title of the comparison
#' @param suppress_warnings logical. Whether to suppress warnings in the \code{diffdf} function, is a \code{diffdf} argument (default = FALSE)
#' @param opt_repl_castor logical. Whether to replace a `;` with a `-`, and replace `\\n` and `\\r` with a "". This is what is done in the excel export of Castor. Note that ALL ; will be replaced by a -
#' @param opt_rm_VarDiff_null logical. Whether to remove all VarDiff differences with only ""/NA/"null" (default = FALSE)
#' @param aggregate_VarDiff logical. Whether or not to make a aggregated VarDiff
#' @param opt_calc_VarDiff_spaces logical. Whether to try to calculate if the difference between BASE and COMPARE is due to whitespaces at the start and/or end of a string. If with stringr::str_trim(..) there is no difference between BASE and COMPARE, difference is set to 0 for that row
#' @param opt_calc_VarDiff_diff logical. Whether to try to calculate differences between BASE and COMPARE for all VarDiff rows (default = TRUE)
#' @param opt_rm_VarDiff_diff_0 logical. Whether to remove all rows with VarDiff difference of 0 (default = FALSE)
#' ## The following three arguments are eligible for discarding, dont think they will be used anymore (report_out = "single" is then always the case)
#' @param report_out string. In what format the report needs to be saved, there are two options: c("single", "multiple"). Single exports all files in one excel, multiple saves a separate file for each component.
#' @param manual_csv logical. Whether to save the VarDiff as .csv document. Required when because of too many strange symbols the \code{read_excel} function doesnot work (default = FALSE)
#' @param opt_rm_AttribD_null logical. Whether to remove attribute differences with only ""/null/NULL/NA differences (default = FALSE)
#'
#' @return A diffdf report is made (and optionally saved) that is a list with at most 7 elements: NumDiff, VarDiff, AttribD, VarClas, VarMode, ExtRows, ExtCols. Each element contains information about differences between the datafiles, vars and cats. If a file is empty, it is omitted from the saving of the plot.
#' * VarMode: Difference modes for each variable or attribute, e.g., character vs numeric. If different, variables are not checked! But only an issue when table = diff_xmod
#' * NumDiff: All differences that there are on a variable level, e.g., 2700 value mismatches
#' * VarDiff: All mismatches that are denoted in NumDiff, e.g., for all 2700 observations the value in the BASE and the value in the COMPARE
#' * AttribD: All attribute differences per variable, e.g., different entity_type or Min/Max values. Here opal.repeatable is important! Null=0 is fine
#' * VarClas: Difference classes for each variable or attribute, e.g., haven_labelled character vs character
#' * ExtRows: Are there extra rows added?
#' * ExtCols: Are there extra columns added? e.g., label vs label:en
#'
#'
#' @import opalr
#' @import diffdf
#' @import dplyr
#' @importFrom openxlsx write.xlsx
#'
#' @author Lars van der Burg
#'
#' @export
check_diffdf_opal_generic <- function(datafile = NULL, datafile2 = NULL, var = NULL, var2 = NULL, cat = NULL, cat2 = NULL,
                                      comparison = "mod", comp_key = "id", date_time_format = c("Ymd HMS", "Ymd"),
                                      report_path = NULL, report_name = "Report", report_title = "", suppress_warnings = FALSE,
                                      opt_repl_castor = FALSE, opt_rm_VarDiff_null = FALSE, aggregate_VarDiff = FALSE,
                                      opt_calc_VarDiff_spaces = TRUE, opt_calc_VarDiff_diff = TRUE, opt_rm_VarDiff_diff_0 = FALSE,

                                      report_out = "single", manual_csv = FALSE, opt_rm_spaces_around = FALSE, opt_rm_AttribD_null = FALSE, ...){


# Checks ------------------------------------------------------------------
## Check whether all essential arguments are specified when the function is called
  if(!is.null(datafile)){
    if(missing(datafile)){stop("Make sure that argument datafile is specified, it's essential")}
    if(missing(datafile2)){stop("Make sure that argument datafile2 is specified, it's essential")}
  }


## Is an appropriate comparison possible
  if(!(comparison %in% c("base", "mod", "both"))){stop("comparison must either be `base`, `mod` or `both`.")}


## Are the keys unique?
  if(!is.null(datafile)){
    if(isFALSE(diffdf:::has_unique_rows(datafile, comp_key))){stop("With the chosen comp_key, rows in datafile are duplicated")}
    if(isFALSE(diffdf:::has_unique_rows(datafile2, comp_key))){stop("With the chosen comp_key, rows in datafile2 are duplicated")}
    if(isFALSE(nrow((datafile |> select(all_of(comp_key))) |> inner_join((datafile2 |> select(all_of(comp_key))), by = comp_key)) > 0)){
      stop("There is not a single overlap in the keys of datafile and datafile2, cannot make a comparison without an overlap")}
  }


## Ensure that diffdf can be run/saved
  if(!isFALSE(report_path) & !is.null(report_path)){

    ### Can it be saved?
    if(isFALSE(is.character(report_path)) || isFALSE(dir.exists(report_path))){
      stop("The report_path where you want to save the diffdf report doesn't exist. Please supply a valid location.\n If you don't want to save the diffdf report, set report_path = NULL.")
    }
    ### Is it an accurate format
    if(!(report_out %in% c("single", "multiple"))){stop("The report_out format should either be `single` or `multiple`.")}
    ### Is manual_csv runable
    if(report_out == "multiple" & !is.logical(manual_csv)){stop("manual_csv must either be TRUE or FALSE.")}
  }


## Are all arguments usable?
  if(!is.logical(opt_repl_castor)){stop("opt_repl_castor must either be TRUE or FALSE.")}
  if(!is.logical(opt_rm_VarDiff_null)){stop("opt_rm_VarDiff_null must either be TRUE or FALSE.")}
  if(!is.logical(aggregate_VarDiff)){stop("aggregate_VarDiff must either be TRUE or FALSE.")}
  if(!is.logical(opt_calc_VarDiff_spaces)){stop("opt_calc_VarDiff_spaces must either be TRUE or FALSE.")}
  if(!is.logical(opt_calc_VarDiff_diff)){stop("opt_calc_VarDiff_diff must either be TRUE or FALSE.")}
  if(!is.logical(opt_rm_VarDiff_diff_0)){stop("opt_rm_VarDiff_diff_0 must either be TRUE or FALSE.")}
  if(!is.logical(opt_rm_AttribD_null)){stop("opt_rm_AttribD_null must either be TRUE or FALSE.")}

  if(isTRUE(opt_rm_VarDiff_diff_0) & (isFALSE(opt_calc_VarDiff_diff) & isFALSE(opt_calc_VarDiff_spaces))){
    opt_rm_VarDiff_diff_0 = FALSE
    cat("opt_rm_VarDiff_diff_0 is set to FALSE, can't discard differences if there are no differences calculated")
  }




# Initialize everything ---------------------------------------------------
  diff_data <- diff_datamod <- diff_var <- diff_varmod <- diff_cat <- diff_catmod <- NULL

## The names as supplied to the arguments, maybe want to do something with it?
  # name_datafile = deparse(substitute(datafile))
  # name_datafile2 = deparse(substitute(datafile2))
  # name_var = deparse(substitute(var))
  # name_var2 = deparse(substitute(var2))
  # name_cat = deparse(substitute(cat))
  # name_cat2 = deparse(substitute(cat2))



# Optional adjustments I --------------------------------------------------
  if(opt_repl_castor){
    if(!is.null(datafile)){
      datafile = datafile |> dplyr::mutate(across(where(is.character), ~str_replace_all(str_replace_all(str_replace_all(., "\n", ""), "\r", ""), ";", "-")))
      datafile2 = datafile2 |> dplyr::mutate(across(where(is.character), ~str_replace_all(str_replace_all(str_replace_all(., "\n", ""), "\r", ""), ";", "-")))
    }

    if(!is.null(var)){
      var = var |> dplyr::mutate(across(where(is.character), ~str_replace_all(str_replace_all(str_replace_all(., "\n", ""), "\r", ""), ";", "-")))
      var2 = var2 |> dplyr::mutate(across(where(is.character), ~str_replace_all(str_replace_all(str_replace_all(., "\n", ""), "\r", ""), ";", "-")))
    }

    if(!is.null(cat)){
      cat = cat |> dplyr::mutate(across(where(is.character), ~str_replace_all(str_replace_all(str_replace_all(., "\n", ""), "\r", ""), ";", "-")))
      cat2 = cat2 |> dplyr::mutate(across(where(is.character), ~str_replace_all(str_replace_all(str_replace_all(., "\n", ""), "\r", ""), ";", "-")))
    }
  }


  if(opt_rm_spaces_around){
    cat("This option is deprecated, use opt_calc_VarDiff_spaces from now on")
  }



# Default comparison ------------------------------------------------------
  if(comparison %in% c("base", "both")){
    if(!is.null(datafile)){diff_data <- diffdf::diffdf(base = datafile, compare = datafile2, keys = comp_key, suppress_warnings = suppress_warnings)}
    if(!is.null(var)){diff_var <- diffdf::diffdf(base = var, compare = var2, keys = "name", suppress_warnings = suppress_warnings)}
    if(!is.null(cat)){diff_cat <- diffdf::diffdf(base = cat, compare = cat2, keys = c("variable", "name"), suppress_warnings = suppress_warnings)}
  }



# Modified comparison ------------------------------------------------
  if(comparison %in% c("mod", "both")){

    # Datafile: convert both datafile files to character
    if(!is.null(datafile)){
      datafilemod = datafile |> dplyr::mutate(across(everything(), as.character))
      datafile2mod = datafile2 |> dplyr::mutate(across(everything(), as.character))
    }

    # Variables: convert both var files to character
    if(!is.null(var)){
      varmod <- var |> dplyr::mutate(across(everything(), as.character))
      var2mod <- var2 |> dplyr::mutate(across(everything(), as.character))
    }

    # Categories: convert BOOLEAN in "missing" into numeric
    if(!is.null(cat)){
      catmod = cat; if(is.logical(catmod$missing)){catmod$missing = as.numeric(catmod$missing)}
      cat2mod = cat2; if(is.logical(cat2mod$missing)){cat2mod$missing = as.numeric(cat2mod$missing)}
    }


    if(!is.null(datafile)){diff_datamod <- diffdf::diffdf(base = datafilemod, compare = datafile2mod, keys = comp_key, suppress_warnings = suppress_warnings)}
    if(!is.null(var)){diff_varmod <- diffdf::diffdf(base = varmod, compare = var2mod, keys = "name", suppress_warnings = suppress_warnings)}
    if(!is.null(cat)){diff_catmod <- diffdf::diffdf(base = catmod, compare = cat2mod, keys = c("variable", "name"), suppress_warnings = suppress_warnings)}
  }



# Set-up differences report -----------------------------------------------
  if(comparison == "base"){
    diff <- list(diff_data = diff_data, diff_var = diff_var, diff_cat = diff_cat)

  } else if(comparison == "mod"){
    diff <- list(diff_datamod = diff_datamod, diff_varmod = diff_varmod, diff_catmod = diff_catmod)

  } else if(comparison == "both"){
    diff <- list(diff_data = diff_data, diff_var = diff_var, diff_cat = diff_cat,
                 diff_datamod = diff_datamod, diff_varmod = diff_varmod, diff_catmod = diff_catmod)
  }


  # Create empty tibble in preset format
  report          <- list()
  report$Overview <- NA
  report$NumDiff  <- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("TABLE", "VARIABLE", "No of Differences")) %>% as_tibble() %>% mutate_all(as.character)
  report$VarDiff  <- setNames(data.frame(matrix(ncol = 5, nrow = 0)), c("TABLE", "VARIABLE", "id", "BASE", "COMPARE")) %>% as_tibble() %>% mutate_all(as.character)
  report$AttribD  <- setNames(data.frame(matrix(ncol = 5, nrow = 0)), c("TABLE", "VARIABLE", "ATTR_NAME", "VALUES.BASE", "VALUES.COMP")) %>% as_tibble() %>% mutate_all(as.character)
  report$VarClas  <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), c("TABLE", "VARIABLE", "CLASS.BASE", "CLASS.COMP")) %>% as_tibble() %>% mutate_all(as.character)
  report$VarMode  <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), c("TABLE", "VARIABLE", "MODE.BASE", "MODE.COMP")) %>% as_tibble() %>% mutate_all(as.character)
  report$ExtRows  <- setNames(data.frame(matrix(ncol = 1, nrow = 0)), c("TABLE")) %>% as_tibble() %>% mutate_all(as.character)
  report$ExtCols  <- setNames(data.frame(matrix(ncol = 1, nrow = 0)), c("TABLE")) %>% as_tibble() %>% mutate_all(as.character)


# Report overview ----------------------------------------------------------
## Here an overview of the diffdf settings are saved into an excel sheet
  if(!is.null(report_path)){
    Overview = matrix(NA, nrow = 21, ncol = 4, dimnames = list(NULL, c(" ", " ", " ", " ")))
    Overview[1, 1] = report_title
    Overview[2, 1] = paste0("This is a diffdf report that is executed on: ", Sys.Date())

    Overview[4, 1] = "The dimensions of the compared data frames are:"
    Overview[5, 2] = "# rows"; Overview[5, 3] = "# Cols"

    Overview[6, 1] = "datafile"; if(!is.null(datafile)){Overview[6, 2] = nrow(datafile); Overview[6, 3] = ncol(datafile)} else {Overview[6, 4] = "datafile was set to NULL, so ignored"}
    Overview[7, 1] = "datafile2"; if(!is.null(datafile2)){Overview[7, 2] = nrow(datafile2); Overview[7, 3] = ncol(datafile2)} else {Overview[7, 4] = "datafile2 was set to NULL, so ignored"}

    Overview[8, 1] = "var"; if(!is.null(var)){Overview[8, 2] = nrow(var); Overview[8, 3] = ncol(var)} else {Overview[8, 4] = "var dictionary was set to NULL, so ignored"}
    Overview[9, 1] = "var2"; if(!is.null(var2)){Overview[9, 2] = nrow(var2); Overview[9, 3] = ncol(var2)} else {Overview[9, 4] = "var2 dictionary was set to NULL, so ignored"}

    Overview[10, 1] = "cat"; if(!is.null(cat)){Overview[10, 2] = nrow(cat); Overview[10, 3] = ncol(cat)} else {Overview[10, 4] = "cat dictionary was set to NULL, so ignored"}
    Overview[11, 1] = "cat2"; if(!is.null(cat2)){Overview[11, 2] = nrow(cat2); Overview[11, 3] = ncol(cat2)} else {Overview[11, 4] = "cat2 dictionary was set to NULL, so ignored"}

    Overview[13, 1] = "The parameter settings for this executed diffdf:"
    Overview[14, 1] = "comparison:"; Overview[14, 2] = comparison; Overview[14, 4] = paste("This means that", if(comparison == "base"){"only the unadjusted"}
                                                                                                              else if(comparison == "mod"){"only the adjusted"}
                                                                                                              else if(comparison == "both"){"both the unadjusted and adjusted"}, "dataframes are compared")
    Overview[15, 1] = "opt_repl_castor:"; Overview[15, 2] = opt_repl_castor; if(isTRUE(opt_repl_castor)){Overview[15, 4] = "This means that every ; is replaced with a - and the line breaks are discarded, as is done in the Castor excel export"}
    Overview[16, 1] = "opt_rm_VarDiff_null:"; Overview[16, 2] = opt_rm_VarDiff_null; if(isTRUE(opt_rm_VarDiff_null)){Overview[16, 4] = "This means that all VarDiff differences with only NA/NULL/space are discarded"}
    Overview[17, 1] = "opt_calc_VarDiff_spaces:"; Overview[17, 2] = opt_calc_VarDiff_spaces; if(isTRUE(opt_calc_VarDiff_spaces)){Overview[17, 4] = "This means that if the only difference between BASE and COMPARE are white spaces before and after a text, difference column is set to 0"}
    Overview[18, 1] = "date_time_format:"; Overview[18, 2] = paste(date_time_format, collapse = "; "); Overview[18, 4] = "These date(time) formats are used to make the VarDiff difference column"
    Overview[19, 1] = "opt_calc_VarDiff_diff:"; Overview[19, 2] = opt_calc_VarDiff_diff; if(isTRUE(opt_calc_VarDiff_diff)){Overview[19, 4] = "This means that we tried to calculate differences between the BASE and COMPARE for all VarDiff rows"}
    Overview[20, 1] = "opt_rm_VarDiff_diff_0:"; Overview[20, 2] = opt_rm_VarDiff_diff_0; if(isTRUE(opt_rm_VarDiff_diff_0)){Overview[20, 4] = "This means that all rows with a VarDiff difference of 0 are discarded"}
    Overview[21, 1] = "opt_rm_AttribD_null:"; Overview[21, 2] = opt_rm_AttribD_null; if(isTRUE(opt_rm_AttribD_null)){Overview[21, 4] = "This means that all attributes differences with only NA/NULL/space are discarded"}

    report$Overview = Overview
  }



# Differences report ------------------------------------------------------
## ROWBIND all differences found to CHARACTER (all names will be the first 7 characters of the name in de diffdf output; thus combining all differences over variables as well)
  for(table in names(diff)){
    for(Name in names(diff[[table]])){

      if(Name == "DataSummary"){
        next
      }

      Name7 <- substr(Name, start = 1, stop = 7)

      if(Name7 == "ExtRows"){
        temp <- diff[[table]][[Name]] %>% dplyr::mutate(TABLE = table, EXTROWS_in = substring(Name, 8, 11)) %>% select(TABLE, EXTROWS_in, everything()) %>% mutate_all(as.character)
        report[[Name7]] <- report[[Name7]] %>% bind_rows(temp)

      } else if(Name7 == "ExtCols"){
        temp <- diff[[table]][[Name]] %>% dplyr::mutate(TABLE = table, EXTCOLS_in = substring(Name, 8, 11)) %>% select(TABLE, EXTCOLS_in, everything()) %>% mutate_all(as.character)
        report[[Name7]] <- report[[Name7]] %>% bind_rows(temp)

      } else {
        temp <- diff[[table]][[Name]] %>% dplyr::mutate(TABLE = table) %>% select(TABLE, everything()) %>% mutate_all(as.character)
        report[[Name7]] <- report[[Name7]] %>% bind_rows(temp)
      }
    }
  }

  # NumDif has "Variable" instead of "VARIABLE" : change this. (in diffdf(...) they use Variable instead of VARIABLE
  report$NumDiff[["VARIABLE"]] <- report$NumDiff[["Variable"]]
  report$NumDiff[["Variable"]] <- NULL



# Optional adjustments II -------------------------------------------------
  if(opt_rm_VarDiff_null){
    # Optional: remove VarDiff differences that are only because one is NA and other "" or "null"
    report$VarDiff <- report$VarDiff |>
      filter(!((BASE == "" & is.na(COMPARE)) | (is.na(BASE) & COMPARE == "null") | (is.na(BASE) & COMPARE == "NA") | (BASE == "NA" & is.na(COMPARE))))
  }

  if(opt_calc_VarDiff_diff | opt_calc_VarDiff_spaces){
    report$VarDiff = report$VarDiff |> dplyr::mutate(difference = NA_integer_)
  }

  if(opt_calc_VarDiff_diff){
    suppressWarnings({
      report$VarDiff = report$VarDiff |>
        dplyr::mutate(date1 = lubridate::parse_date_time(BASE, date_time_format),
                      date2 = lubridate::parse_date_time(COMPARE, date_time_format),
                      difference = dplyr::case_when((!is.na(date1) & !is.na(date2)) ~ as.numeric(difftime(date1, date2, units = "secs")),
                                                    (!is.na(as.numeric(BASE)) & !is.na(as.numeric(COMPARE))) ~ abs(as.numeric(BASE) - as.numeric(COMPARE)),
                                                    .default = difference)) |>
        dplyr::select(-c(date1, date2))
    })
  }


  if(opt_calc_VarDiff_spaces){
    report$VarDiff = report$VarDiff |>
      dplyr::mutate(base1 = str_trim(BASE, side = "both"),
                    comp1 = str_trim(COMPARE, side = "both")) |>
      dplyr::rowwise() |>
      dplyr::mutate(difference = dplyr::case_when(base1 == comp1 ~ 0,
                                                  .default = difference)) |>
      dplyr::select(-c(base1, comp1))
  }


  if(opt_rm_VarDiff_diff_0){
    report$VarDiff = report$VarDiff |>
      dplyr::filter(difference != 0 | is.na(difference))
  }


  if(opt_rm_AttribD_null){
    # Optional: Remove attribute differences with only ""/null/NULL/NA difference
    report$AttribD <- report$AttribD %>%
      filter(!(VALUES.BASE %in% c("null", "NULL", "none", "", NA) & VALUES.COMP %in% c("null", "NULL", "none", "", NA)))
  }





# Repeated measurements ---------------------------------------------------
  if(!is.null(var)){
    checkrep = tibble::tribble(
      ~var,    ~repeatability,
      "BASE var dict:", "",
      "COMP var dict:", ""
    )

    if("repeatable" %in% colnames(var)){
      checkrep[1, 2] <- dplyr::case_when(all(var$repeatable == TRUE) ~ 'all vars repeated',
                                         all(var$repeatable == FALSE) ~ 'no vars repeated',
                                         TRUE ~'mixed vars repeated')
    } else {
      checkrep[1, 2] = "no repeatable information"
    }

    if("repeatable" %in% colnames(var2)){
      checkrep[2, 2] <- dplyr::case_when(all(var2$repeatable == TRUE) ~ 'all vars repeated',
                                         all(var2$repeatable == FALSE) ~ 'no vars repeated',
                                         TRUE ~'mixed vars repeated')
    } else {
      checkrep[2, 2] = "no repeatable information"
    }


    report$Repeatability = checkrep
  }



# Aggregate ---------------------------------------------------------------
  if(aggregate_VarDiff & nrow(report$VarDiff) > 0){
    NULLs = c(NA, "null", "", "none", "NA", "NULL")

    aggregated_VarDiff = report$VarDiff |>
      group_by(VARIABLE) |>
      dplyr::mutate("CONVERSION" = case_when(
        (BASE %in% NULLs) & (COMPARE %in% NULLs) ~ "NULL->NULL",
        !(BASE %in% NULLs) & (COMPARE %in% NULLs) ~ "VALUE->NULL",
        (BASE %in% NULLs) & !(COMPARE %in% NULLs) ~ "NULL->VALUE",
        !(BASE %in% NULLs) & !(COMPARE %in% NULLs) ~ "VALUE->VALUE"
      )) |>
      group_by(id, CONVERSION, .add = TRUE) |>
      dplyr::mutate("TABLES" = case_when(
        (("diff_data" %in% TABLE) & ("diff_datamod" %in% TABLE)) ~ "both_data",
        (("diff_data" %in% TABLE) & !("diff_datamod" %in% TABLE)) ~ "diff_data",
        (!("diff_data" %in% TABLE) & ("diff_datamod" %in% TABLE)) ~ "diff_datamod",

        (("diff_var" %in% TABLE) & ("diff_varmod" %in% TABLE)) ~ "both_var",
        (("diff_var" %in% TABLE) & !("diff_varmod" %in% TABLE)) ~ "diff_var",
        (!("diff_var" %in% TABLE) & ("diff_varmod" %in% TABLE)) ~ "diff_varmod",

        (("diff_cat" %in% TABLE) & ("diff_catmod" %in% TABLE)) ~ "both_cat",
        (("diff_cat" %in% TABLE) & !("diff_catmod" %in% TABLE)) ~ "diff_cat",
        (!("diff_cat" %in% TABLE) & ("diff_catmod" %in% TABLE)) ~ "diff_catmod",

        .default = "Check")) |>
      group_by(VARIABLE, CONVERSION, TABLE, TABLES) |>
      dplyr::mutate(n = n(),
             min_base = if_else(TABLE %in% c("diff_data", "diff_datamod"), min(BASE), NA),
             max_base = if_else(TABLE %in% c("diff_data", "diff_datamod"), max(BASE), NA),
             min_comp = if_else(TABLE %in% c("diff_data", "diff_datamod"), min(COMPARE), NA),
             max_comp = if_else(TABLE %in% c("diff_data", "diff_datamod"), max(COMPARE), NA)) |>
      ungroup() |>
      select(VARIABLE, CONVERSION, n, TABLES, min_base, max_base, min_comp, max_comp) |>
      unique() |>
      arrange(VARIABLE, CONVERSION)

    report$VarDiff_aggregated = aggregated_VarDiff
  }



# Export report -----------------------------------------------------------
  if(!is.null(report_path)){
    if(report_out == "multiple"){

      for(Name in names(report)){

        cat(Name, " - Number of rows: ", nrow(report[[Name]]), "\n")

        if(nrow(report[[Name]]) == 0){
          next
        }

        # REPORT: Manual to csv. Only when manual_csv == TRUE
        if(manual_csv && Name == "VarDiff"){
          cat("This VarDiff is manually saved as a .csv, because write.xlsx didn't work (problems with many strange symbols)\n")

          path <- paste0(report_path, report_name, "_", format(Sys.Date(), "%Y%m%d"), "_", Name, ".csv")
          write.csv(x = report[[Name]], file = path, quote = TRUE, row.names = FALSE)

          # REPORT: WRITE TO EXCEl
        } else if(nrow(report[[Name]] < 1E6)){
          path <- paste0(report_path, report_name, "_", format(Sys.Date(), "%Y%m%d"), "_", Name, ".xlsx")
          openxlsx::write.xlsx(report[[Name]], file = path)

          # EXPORT LOG TO CSV if number of rows>=1M
        } else if(nrow(report[[Name]] >= 1E6)){
          path <- paste0(report_path, report_name, "_", format(Sys.Date(), "%Y%m%d"), "_", Name, ".csv")
          write.csv(x = report[[Name]], file = path, quote = TRUE, row.names = FALSE)
        }
      }

      if(!is.null(var)){
        path <- paste0(report_path, report_name, "_", format(Sys.Date(), "%Y%m%d"), "_", "Repeatability.xlsx")
        openxlsx::write.xlsx(checkrep, file = path)
      }


    } else if(report_out == "single"){

      index_csv = which(unlist(lapply(report, nrow)) > 1E5); index_csv_name = names(index_csv)

      if(length(index_csv) == 0){
        openxlsx::write.xlsx(report, file = paste0(report_path, report_name, "_", format(Sys.Date(), "%Y%m%d"), ".xlsx"))

      } else {
        openxlsx::write.xlsx(report[-index_csv], file = paste0(report_path, report_name, "_", format(Sys.Date(), "%Y%m%d"), ".xlsx"))
        for(i in 1:length(index_csv)){
          path <- paste0(report_path, report_name, "_", format(Sys.Date(), "%Y%m%d"), "_", index_csv_name[i], ".csv")

          write.csv(x = report[[index_csv[i]]], file = path, quote = TRUE, row.names = FALSE)
        }
      }
    }
  }


  return(report)
}
