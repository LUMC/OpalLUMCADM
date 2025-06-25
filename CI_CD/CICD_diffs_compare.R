# Start -------------------------------------------------------------------
# Script to make a start with the CI/CD comparison between expected and observed differences
#
# For the comparison, fakedata_CICD_diff.RData is the job artifact that must be downloaded from the git CI/CD
#
# Written by: Lars van der Burg
# Written on: 2025-01-14


library(tidyverse)

# Read-in results ---------------------------------------------------------
## For encryption/decryption we cannot use the CI/CD. Compare with an old expected outcome
load("CI_CD/demo_fakedata_exp_20250519.RData")
fakedata_exp_prev = fakedata_exp

load("CI_CD/fakedata_exp.RData")  # The expected outcome
load("CI_CD/fakedata_CICD_diffs.Rdata")  # The outcome of the CI/CD


# Compare CI/CD -----------------------------------------------------------
N = length(fakedata_CICD_diffs)
cnames = names(fakedata_CICD_diffs)

for(i in 1:N){
  cnames_i = cnames[i]
  cat("Now compare  ", cnames_i, paste(rep(" ", 16 - str_count(cnames_i)), collapse = ""))


  out_i = fakedata_CICD_diffs[[cnames[i]]]
  exp_i = fakedata_exp[[paste0("exp_", cnames[i])]]


  if(str_detect(cnames_i, "output_checks")){
    cat("TEKST\n")
  } else if(str_detect(cnames_i, "report_import")){

    check_datafile = diffdf::diffdf(out_i$datafile, exp_i$datafile)
    check_var = diffdf::diffdf(out_i$var, exp_i$var)
    check_cat = diffdf::diffdf(out_i$cat, exp_i$cat)
    check_datafile4copy = diffdf::diffdf(out_i$datafile4copy, exp_i$datafile4copy)
    check_var4copy = diffdf::diffdf(out_i$var4copy, exp_i$var4copy)
    check_cat4copy = diffdf::diffdf(out_i$cat4copy, exp_i$cat4copy)

    if(length(check_datafile) == 0 & length(check_var) == 0 & length(check_cat) == 0 &
       length(check_datafile4copy) == 0 & length(check_var4copy) == 0 & length(check_cat4copy) == 0){
      cat("Passed\n")
    } else {
      cat("FAILEDD\n")
    }

  } else if(str_detect(cnames_i, "report_diffdf|report_create")){

    diff = case_when(!identical(exp_i$Overview, out_i$Overview) ~ "Overview",
                     !identical(exp_i$NumDiff, out_i$NumDiff) ~ "NumDiff",
                     !identical(exp_i$VarDiff, out_i$VarDiff) ~ "VarDiff",
                     !identical(exp_i$AttribD, out_i$AttribD) ~ "AttribD",
                     !identical(exp_i$VarClas, out_i$VarClas) ~ "VarClas",
                     !identical(exp_i$VarMode, out_i$VarMode) ~ "VarMode",
                     !identical(exp_i$ExtRows, out_i$ExtRows) ~ "ExtRows",
                     !identical(exp_i$ExtCols, out_i$ExtCols) ~ "ExtCols",
                     !identical(exp_i$Repeatability, out_i$Repeatability) ~ "Repeatability",
                     .default = "Else")

    cat(diff, "\n")
  } else {
    cat("UNKNOWN\n")
  }
}

diffdf::diffdf(fakedata_CICD_diffs[[cnames[1]]]$datafile4copy, fakedata_exp[[paste0("exp_", cnames[1])]]$datafile4copy)
diffdf::diffdf(fakedata_CICD_diffs[[cnames[2]]]$NumDiff, fakedata_exp[[paste0("exp_", cnames[2])]])



# Compare encrypt/decrypt -------------------------------------------------
checks = c("exp_encryption_checks1", "exp_decryption_checks1", "exp_report_diffdf8", "exp_decryption_checks2", "exp_report_diffdf9", "exp_encryption_checks2", "exp_decryption_checks3"); nr_checks = length(checks)
for(i in 1:nr_checks){
  check_i = checks[i]

  exp_i = fakedata_exp_prev[[check_i]]
  out_i = fakedata_exp[[check_i]]

  cat(paste0(check_i, ": ", identical(exp_i, out_i)), "\n")
}

