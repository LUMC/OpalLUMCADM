# Start -------------------------------------------------------------------
# Script to make a start with the CI/CD comparison between expected and observed differences
#
# Written by: Lars van der Burg
# Written on: 2024-10-30
#
#


# Read-in results ---------------------------------------------------------
load("CI_CD/fakedata_CICD_diffs.Rdata")
load("CI_CD/fakedata_exp.RData")




N = length(fakedata_CICD_diffs)
cnames = names(fakedata_CICD_diffs)

for(i in 1:N){
  cnames_i = cnames[i]
  cat("Now compare  ", cnames_i, paste(rep(" ", 16 - str_count(cnames_i)), collapse = ""))


  exp_i = fakedata_exp[[paste0("exp_", cnames[i])]]
  out_i = fakedata_CICD_diffs[[cnames[i]]]


  if(FALSE %in% (names(exp_i) %in% c("Overview", "NumDiff", "VarDiff", "AttribD", "VarClas", "VarMode", "ExtRows", "ExtCols", "Repeatability", "VarDiff_aggregated"))){
    cat("UNKOWN\n")
  } else {

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
  }
}

