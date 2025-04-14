library(tidyverse)
library(diffdf)

quiet = function(x){
  sink(tempfile())
  on.exit(sink())
  invisible(suppressWarnings(force(x)))
}


load("CI_CD/OTAP Opal5/fakedata_exp_NEWPROD.RData")
fakedata_PROD = fakedata_exp

load("CI_CD/OTAP Opal5/fakedata_exp_OLDTEST.RData")
fakedata_TEST = fakedata_exp


cnames = names(fakedata_PROD)
checks = NULL
for(i in 1:length(fakedata_PROD)){
  cnames_i = cnames[i]

  cat("Now testing index", i, "which is test", cnames_i, "\n")

  if(isTRUE(identical(fakedata_PROD[[cnames_i]], fakedata_TEST[[cnames_i]]))){
    checks[i] = FALSE
  } else {

    if(i %in% c(4, 23, 33)){
    ## 33 has issues, but are also decimal/date(time) format/unit

      if(!is.null(fakedata_PROD[[cnames_i]]$datafile4copy) && i != 33){
        # diffdf_report1 = diffdf(fakedata_PROD[[cnames_i]]$datafile, fakedata_TEST[[cnames_i]]$datafile, keys = "id")
        diffdf_report2 = quiet(diffdf(fakedata_PROD[[cnames_i]]$datafile4copy, fakedata_TEST[[cnames_i]]$datafile4copy, keys = "id"))

        if(length(diffdf_report2) != 0){
          cat("datafile mismatch:", names(diffdf_report2), "\n")
        }
      }

      if(!is.null(fakedata_PROD[[cnames_i]]$var4copy)){
        # diffdf_report3 = diffdf(fakedata_PROD[[cnames_i]]$var, fakedata_TEST[[cnames_i]]$var, keys = "name")
        diffdf_report4 = quiet(diffdf(fakedata_PROD[[cnames_i]]$var4copy, fakedata_TEST[[cnames_i]]$var4copy, keys = "name"))

        if(length(diffdf_report4) != 0){
          cat("var mismatch:", names(diffdf_report4), "\n")
        }
      }

      if(!is.null(fakedata_PROD[[cnames_i]]$cat4copy)){
        # diffdf_report5 = diffdf(fakedata_PROD[[cnames_i]]$cat, fakedata_TEST[[cnames_i]]$cat, keys = c("variable", "name"))
        diffdf_report6 = quiet(diffdf(fakedata_PROD[[cnames_i]]$cat4copy, fakedata_TEST[[cnames_i]]$cat4copy, keys = c("variable", "name")))

        if(length(diffdf_report6) != 0){
          cat("cat mismatch:", names(diffdf_report6), "\n")
        }
      }


    } else if(i %in% c(5, 6, 7, 8, 10, 11, 16, 24, 29, 36, 41)){
      if(!identical(fakedata_PROD[[cnames_i]]$NumDiff, fakedata_TEST[[cnames_i]]$NumDiff)){
        Check_NumDiff = bind_rows(fakedata_PROD[[cnames_i]]$NumDiff |> filter(str_detect(TABLE, "mod")) |> anti_join(fakedata_TEST[[cnames_i]]$NumDiff |> filter(str_detect(TABLE, "mod")), by = c("TABLE", "VARIABLE")),
                                  fakedata_TEST[[cnames_i]]$NumDiff |> filter(str_detect(TABLE, "mod")) |> anti_join(fakedata_PROD[[cnames_i]]$NumDiff |> filter(str_detect(TABLE, "mod")), by = c("TABLE", "VARIABLE"))) |> select("TABLE", "VARIABLE") |> as_vector() |> as.character() |> paste(collapse = " ")
        if(length(Check_NumDiff) != 0 && Check_NumDiff != ""){
          cat("NumDiff:", Check_NumDiff, "\n")
        }
      }

      if(!identical(fakedata_PROD[[cnames_i]]$VarDiff, fakedata_TEST[[cnames_i]]$VarDiff)){
        Check_VarDiff = bind_rows(fakedata_PROD[[cnames_i]]$VarDiff |> filter(str_detect(TABLE, "mod")) |> anti_join(fakedata_TEST[[cnames_i]]$VarDiff |> filter(str_detect(TABLE, "mod")), by = c("TABLE", "VARIABLE", "id")),
                                  fakedata_TEST[[cnames_i]]$VarDiff |> filter(str_detect(TABLE, "mod")) |> anti_join(fakedata_PROD[[cnames_i]]$VarDiff |> filter(str_detect(TABLE, "mod")), by = c("TABLE", "VARIABLE", "id"))) |> select("TABLE", "VARIABLE") |> distinct() |> as_vector() |> as.character() |> unique() |> paste(collapse = " ")
        if(length(Check_VarDiff) != 0 && Check_VarDiff != ""){
          cat("VarDiff:", Check_VarDiff, "\n")
        }
      }

      if(!identical(fakedata_PROD[[cnames_i]]$VarDiff, fakedata_TEST[[cnames_i]]$VarDiff)){
        Check_AttribD = bind_rows(fakedata_PROD[[cnames_i]]$AttribD |> filter(str_detect(TABLE, "mod")) |> anti_join(fakedata_TEST[[cnames_i]]$AttribD |> filter(str_detect(TABLE, "mod")), by = c("TABLE", "VARIABLE")),
                                  fakedata_TEST[[cnames_i]]$AttribD |> filter(str_detect(TABLE, "mod")) |> anti_join(fakedata_PROD[[cnames_i]]$AttribD |> filter(str_detect(TABLE, "mod")), by = c("TABLE", "VARIABLE"))) |> select("TABLE", "VARIABLE") |> distinct() |> as_vector() |> as.character() |> unique() |> paste(collapse = " ")
        if(length(Check_AttribD) != 0 && Check_AttribD != ""){
          cat("AttribD:", Check_AttribD, "\n")
        }
      }

      if(!identical(fakedata_PROD[[cnames_i]]$VarDiff, fakedata_TEST[[cnames_i]]$VarDiff)){
        Check_VarClas = bind_rows(fakedata_PROD[[cnames_i]]$VarClas |> filter(str_detect(TABLE, "mod")) |> anti_join(fakedata_TEST[[cnames_i]]$VarClas |> filter(str_detect(TABLE, "mod")), by = c("TABLE", "VARIABLE")),
                                  fakedata_TEST[[cnames_i]]$VarClas |> filter(str_detect(TABLE, "mod")) |> anti_join(fakedata_PROD[[cnames_i]]$VarClas |> filter(str_detect(TABLE, "mod")), by = c("TABLE", "VARIABLE"))) |> select("TABLE", "VARIABLE") |> distinct() |> as_vector() |> as.character() |> unique() |> paste(collapse = " ")
        if(length(Check_VarClas) != 0 && Check_VarClas != ""){
          cat("VarClas:", Check_VarClas, "\n")
        }
      }

      if(!identical(fakedata_PROD[[cnames_i]]$VarDiff, fakedata_TEST[[cnames_i]]$VarDiff)){
        Check_VarMode = bind_rows(fakedata_PROD[[cnames_i]]$VarMode |> filter(str_detect(TABLE, "mod")) |> anti_join(fakedata_TEST[[cnames_i]]$VarMode |> filter(str_detect(TABLE, "mod")), by = c("TABLE", "VARIABLE")),
                                  fakedata_TEST[[cnames_i]]$VarMode |> filter(str_detect(TABLE, "mod")) |> anti_join(fakedata_PROD[[cnames_i]]$VarMode |> filter(str_detect(TABLE, "mod")), by = c("TABLE", "VARIABLE"))) |> select("TABLE", "VARIABLE") |> distinct() |> as_vector() |> as.character() |> unique() |> paste(collapse = " ")
        if(length(Check_VarMode) != 0 && Check_VarMode != ""){
          cat("VarMode:", Check_VarMode, "\n")
        }
      }

      if(isFALSE(identical(fakedata_PROD[[cnames_i]]$ExtRows, fakedata_TEST[[cnames_i]]$ExtRows))){
        cat("ExtRows: CHECK\n")
      }

      if(isFALSE(identical(fakedata_PROD[[cnames_i]]$ExtCols, fakedata_TEST[[cnames_i]]$ExtCols))){
        cat("ExtCols: CHECK\n")
      }

      if(isFALSE(identical(fakedata_PROD[[cnames_i]]$Repeatability, fakedata_TEST[[cnames_i]]$Repeatability))){
        cat("Repeatability: CHECK\n")
      }

    } else if(i %in% c(34)){

      if(isFALSE(str_detect(fakedata_PROD[[cnames_i]][which(fakedata_PROD[[cnames_i]] != fakedata_TEST[[cnames_i]])], "four decimals"))){
        cat("Checks opal: other than four decimals\n")
      }

    } else if(i %in% c(35)){

      if(isFALSE(identical(fakedata_PROD[[cnames_i]]$problems |> filter(issue != "More than four decimals"),
                           fakedata_TEST[[cnames_i]]$problems |> filter(issue != "More than four decimals")))){
        cat("MinMax: other problems than decimals\n")
      }

      if(bind_rows(fakedata_PROD[[cnames_i]]$report_min_max$min_max |> anti_join(fakedata_TEST[[cnames_i]]$report_min_max$min_max, by = c("id", "variable name", "min", "max")),
                   fakedata_TEST[[cnames_i]]$report_min_max$min_max |> anti_join(fakedata_PROD[[cnames_i]]$report_min_max$min_max, by = c("id", "variable name", "min", "max"))) |> nrow() != 0){
        cat("MinMax: min_max")
      }
      if(isFALSE(identical(fakedata_PROD[[cnames_i]]$report_min_max$outside_cat, fakedata_TEST[[cnames_i]]$report_min_max$outside_cat))){
        cat("MinMax: outside_cat\n")
      }
    }

    checks[i] = TRUE
  }

  cat("\n\n")
}
which(checks)


## table vs table
diffdf_report1 = diffdf(fakedata_PROD[[cnames[4]]]$datafile, fakedata_TEST[[cnames[4]]]$datafile, keys = "id")
diffdf_report2 = diffdf(fakedata_PROD[[cnames[4]]]$datafile4copy, fakedata_TEST[[cnames[4]]]$datafile4copy, keys = "id")

diffdf_report3 = diffdf(fakedata_PROD[[cnames[4]]]$var, fakedata_TEST[[cnames[4]]]$var, keys = "name")
diffdf_report4 = diffdf(fakedata_PROD[[cnames[4]]]$var4copy, fakedata_TEST[[cnames[4]]]$var4copy, keys = "name")

diffdf_report5 = diffdf(fakedata_PROD[[cnames[4]]]$cat, fakedata_TEST[[cnames[4]]]$cat, keys = c("variable", "name"))
diffdf_report6 = diffdf(fakedata_PROD[[cnames[4]]]$cat4copy, fakedata_TEST[[cnames[4]]]$cat4copy, keys = c("variable", "name"))


diffdf_report1$VarClassDiffs
diffdf_report2$DataSummary


## diffdf
fakedata_PROD[[cnames[11]]]$NumDiff
fakedata_TEST[[cnames[11]]]$NumDiff

fakedata_PROD[[cnames[11]]]$VarDiff
fakedata_TEST[[cnames[11]]]$VarDiff




lubridate::parse_date_time2("2002-11-08T21:30:00.000Z", "Ymd HMS")
format("08-11-2002T21:30:00.000Z", "%Y%m%d %H%M%S")

ymd_hms("08-11-2002T21:30:00.000Z")
dmy_hms("08-11-2002T21:30:00.000Z")
