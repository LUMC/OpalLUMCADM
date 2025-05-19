# Start -------------------------------------------------------------------
## Script for the CI/CD check done via gitlab
## With this CI/CD check we compare the expected/true reports for a dataset with newly run reports for the same dataset
## These newly run reports are automatically obtained after each push to gitlab
#
## Procedure is as follows:
## - Create a fake dataset/dictionary, this is done via create_datafile.R and random_data.R. This can be re-used for every new comparison
## - Run here the expected/true reports for this dataset/dictionary. That can be done in this script.
##### This should only be done at big moments, when you know that the current package version is ~100% correct.
##### So when functionalities are really changed, a new package version or the CICD_procedure is extended
## - The CICD_procedure.R script has to be used for both the expected/true reports, as for the newly run reports
##### When you add steps here, dont forget to add also a comparison to CICD_check.R
## - with the .gitlab-ci.yml file you define the CI/CD pipeline
## - In gitlab this .gitlab-ci.yml file will run the CICD_check.R script, which runs CICD_procedure.R and performs the comparison
#
#
## Written by: Lars van der Burg
## Written on: 2024-09-05


# Packages ----------------------------------------------------------------
## Packages need to be included in the docker image
library(tidyverse)
library(tibble); library(dplyr); library(stringr)

library(opalr)
library(diffdf)
# library(keyring)



# Functions ---------------------------------------------------------------
## cannot use package, because that has the functions of the official version, not containing the most recent updates
source("R/func_check_categoriesminmax_generic.R");
source("R/func_check_diffdf_opal_generic.R");
source("R/func_checks_opal_R.R");
source("R/func_datafile_conform_var_change.R");
source("R/func_datafile_conform_var_check.R");
source("R/func_delete_table_opal.R");
source("R/func_import_copy_table_opal_many.R");
source("R/func_import_copy_table_opal.R");
source("R/func_import_create_table_opal.R");
source("R/func_import_table_opal2R.R");
source("R/func_make_opal_view.R");
source("R/func_write_table_R2opal.R")
source("R/func_make_opal_view.R");
source("R/func_number_datapoints.R");
source("R/func_write_table_R2opal.R");


# Fake dataset ------------------------------------------------------------
load("example/FAKE_datafile.RData")
load("example/FAKE_var.RData")
load("example/FAKE_cat.RData")


server = "demo"  # c("demo", "test", "new")

# Datafiles ---------------------------------------------------------------
if(server == "demo"){
  ## Demo server of Yannick
  opal_url = "https://opal-demo.obiba.org"; opal_username = "administrator"; opal_password = "password"; projname = "TESTING"
  opal_token = NULL

} else if(server == "test"){
  ## Current test server
  opal_url = "https://dw-test.clinicalresearch.nl/repo"; opal_token = keyring::key_get("token_opal_testclinicalresearch"); projname = "TEST_LARS"
  opal_username = opal_password = NULL

} else if(server == "new"){
## Future prod server
  # opal_url = "https://opal.clinicalresearch.nl"; opal_username = "administrator"; opal_password = "Testing1!"; projname = "TEST_LARS"
  # opal_token = NULL

  opal_url = "https://opal.clinicalresearch.nl"; opal_token = keyring::key_get("token_opal_clinicalresearch"); projname = "TEST_LARS"
  opal_username = opal_password = NULL

}



# Procedure ---------------------------------------------------------------
fakedata_new = CICD_procedure(opal_url = opal_url, opal_username = opal_username, opal_password = opal_password, opal_token = opal_token,
                              projname = projname, datafile = datafile, var = var, cat = cat, encryption = FALSE)

output_checks = fakedata_new$output_checks
report_checks = fakedata_new$report_checks
report_change = fakedata_new$report_change
report_import1 = fakedata_new$report_import1
report_diffdf = fakedata_new$report_diffdf
report_diffdf2 = fakedata_new$report_diffdf2
report_diffdf3 = fakedata_new$report_diffdf3
report_diffdf4 = fakedata_new$report_diffdf4
report_diffdf5 = fakedata_new$report_diffdf5
report_diffdf6 = fakedata_new$report_diffdf6
report_create = fakedata_new$report_create
report_create2 = fakedata_new$report_create2
report_create3 = fakedata_new$report_create3
report_copy = fakedata_new$report_copy
report_copy_many = fakedata_new$report_copy_many
report_view = fakedata_new$report_view
report_view2 = fakedata_new$report_view2
report_view3 = fakedata_new$report_view3
report_create4 = fakedata_new$report_create4
report_view4 = fakedata_new$report_view4
NDPs = fakedata_new$NDPs

output_checks2 = fakedata_new$output_checks2
report_checks2 = fakedata_new$report_checks2
report_import2 = fakedata_new$report_import2
report_create5 = fakedata_new$report_create5
report_create6 = fakedata_new$report_create6
report_create7 = fakedata_new$report_create7
report_copy2 = fakedata_new$report_copy2
report_copy_many2 = fakedata_new$report_copy_many2
report_view5 = fakedata_new$report_view5
report_view6 = fakedata_new$report_view6

output_checks3 = fakedata_new$output_checks3
report_checks3 = fakedata_new$report_checks3
report_import3 = fakedata_new$report_import3
output_checks4 = fakedata_new$output_checks4
report_checks4 = fakedata_new$report_checks4
report_diffdf7 = fakedata_new$report_diffdf7




# Expected results --------------------------------------------------------
load("CI_CD/fakedata_exp.RData")

exp_output_checks = fakedata_exp$exp_output_checks
exp_report_checks = fakedata_exp$exp_report_checks
exp_report_change = fakedata_exp$exp_report_change
exp_report_import1 = fakedata_exp$exp_report_import1
exp_report_diffdf = fakedata_exp$exp_report_diffdf
exp_report_diffdf2 = fakedata_exp$exp_report_diffdf2
exp_report_diffdf3 = fakedata_exp$exp_report_diffdf3
exp_report_diffdf4 = fakedata_exp$exp_report_diffdf4
exp_report_diffdf5 = fakedata_exp$exp_report_diffdf5
exp_report_diffdf6 = fakedata_exp$exp_report_diffdf6
exp_report_create = fakedata_exp$exp_report_create
exp_report_create2 = fakedata_exp$exp_report_create2
exp_report_create3 = fakedata_exp$exp_report_create3
exp_report_copy = fakedata_exp$exp_report_copy
exp_report_copy_many = fakedata_exp$exp_report_copy_many
exp_report_view = fakedata_exp$exp_report_view
exp_report_view2 = fakedata_exp$exp_report_view2
exp_report_view3 = fakedata_exp$exp_report_view3
exp_report_create4 = fakedata_exp$exp_report_create4
exp_report_view4 = fakedata_exp$exp_report_view4
exp_NDPs = fakedata_exp$exp_NDPs

exp_output_checks2 = fakedata_exp$exp_output_checks2
exp_report_checks2 = fakedata_exp$exp_report_checks2
exp_report_import2 = fakedata_exp$exp_report_import2
exp_report_create5 = fakedata_exp$exp_report_create5
exp_report_create6 = fakedata_exp$exp_report_create6
exp_report_create7 = fakedata_exp$exp_report_create7
exp_report_copy2 = fakedata_exp$exp_report_copy2
exp_report_copy_many2 = fakedata_exp$exp_report_copy_many2
exp_report_view5 = fakedata_exp$exp_report_view5
exp_report_view6 = fakedata_exp$exp_report_view6

exp_output_checks3 = fakedata_exp$exp_output_checks3
exp_report_checks3 = fakedata_exp$exp_report_checks3
exp_report_import3 = fakedata_exp$exp_report_import3
exp_output_checks4 = fakedata_exp$exp_output_checks4
exp_report_checks4 = fakedata_exp$exp_report_checks4
exp_report_diffdf7 = fakedata_exp$exp_report_diffdf7




# Check -------------------------------------------------------------------
fakedata_CICD_diffs = list()

cat("\n\nChecking CI/CD on fakedata\n")
cat("output_checks:     "); if(identical(exp_output_checks, output_checks)){cat("check!\n")} else {cat("THERE IS A DIFFERENCE\n"); fakedata_CICD_diffs$output_checks = output_checks}
cat("report_checks:     "); if(identical(exp_report_checks, report_checks)){cat("check!\n")} else {cat("THERE IS A DIFFERENCE\n"); fakedata_CICD_diffs$report_checks = report_checks}
cat("report_change:     "); if(identical(exp_report_change, report_change)){cat("check!\n")} else {cat("THERE IS A DIFFERENCE\n"); fakedata_CICD_diffs$report_change = report_change}
cat("report_import1:    "); if(identical(exp_report_import1, report_import1)){cat("check!\n")} else {cat("THERE IS A DIFFERENCE\n"); fakedata_CICD_diffs$report_import1 = report_import1}
cat("report_diffdf:     "); if(identical(exp_report_diffdf, report_diffdf)){cat("check!\n")} else {cat("THERE IS A DIFFERENCE\n"); fakedata_CICD_diffs$report_diffdf = report_diffdf}
cat("report_diffdf2:    "); if(identical(exp_report_diffdf2, report_diffdf2)){cat("check!\n")} else {cat("THERE IS A DIFFERENCE\n"); fakedata_CICD_diffs$report_diffdf2 = report_diffdf2}
cat("report_diffdf3:    "); if(identical(exp_report_diffdf3, report_diffdf3)){cat("check!\n")} else {cat("THERE IS A DIFFERENCE\n"); fakedata_CICD_diffs$report_diffdf3 = report_diffdf3}
cat("report_diffdf4:    "); if(identical(exp_report_diffdf4, report_diffdf4)){cat("check!\n")} else {cat("THERE IS A DIFFERENCE\n"); fakedata_CICD_diffs$report_diffdf4 = report_diffdf4}
cat("report_diffdf5:    "); if(identical(exp_report_diffdf5, report_diffdf5)){cat("check!\n")} else {cat("THERE IS A DIFFERENCE\n"); fakedata_CICD_diffs$report_diffdf5 = report_diffdf5}
cat("report_diffdf6:    "); if(identical(exp_report_diffdf6, report_diffdf6)){cat("check!\n")} else {cat("THERE IS A DIFFERENCE\n"); fakedata_CICD_diffs$report_diffdf6 = report_diffdf6}
cat("report_create:     "); if(identical(exp_report_create, report_create)){cat("check!\n")} else {cat("THERE IS A DIFFERENCE\n"); fakedata_CICD_diffs$report_create = report_create}
cat("report_create2:    "); if(identical(exp_report_create2, report_create2)){cat("check!\n")} else {cat("THERE IS A DIFFERENCE\n"); fakedata_CICD_diffs$report_create2 = report_create2}
cat("report_create3:    "); if(identical(exp_report_create3, report_create3)){cat("check!\n")} else {cat("THERE IS A DIFFERENCE\n"); fakedata_CICD_diffs$report_create3 = report_create3}
cat("report_copy:       "); if(identical(exp_report_copy, report_copy)){cat("check!\n")} else {cat("THERE IS A DIFFERENCE\n"); fakedata_CICD_diffs$report_copy = report_copy}
cat("report_copy_many:  "); if(identical(exp_report_copy_many, report_copy_many)){cat("check!\n")} else {cat("THERE IS A DIFFERENCE\n"); fakedata_CICD_diffs$report_copy_many = report_copy_many}
cat("report_view:       "); if(identical(exp_report_view, report_view)){cat("check!\n")} else {cat("THERE IS A DIFFERENCE\n"); fakedata_CICD_diffs$report_view = report_view}
cat("report_view2:      "); if(identical(exp_report_view2, report_view2)){cat("check!\n")} else {cat("THERE IS A DIFFERENCE\n"); fakedata_CICD_diffs$report_view2 = report_view2}
cat("report_view3:      "); if(identical(exp_report_view3, report_view3)){cat("check!\n")} else {cat("THERE IS A DIFFERENCE\n"); fakedata_CICD_diffs$report_view3 = report_view3}
cat("report_create4:    "); if(identical(exp_report_create4, report_create4)){cat("check!\n")} else {cat("THERE IS A DIFFERENCE\n"); fakedata_CICD_diffs$report_create4 = report_create4}
cat("report_view4:      "); if(identical(exp_report_view4, report_view4)){cat("check!\n")} else {cat("THERE IS A DIFFERENCE\n"); fakedata_CICD_diffs$report_view4 = report_view4}
cat("NDP:               "); if(identical(exp_NDPs, NDPs)){cat("check!\n")} else {cat("THERE IS A DIFFERENCE\n"); fakedata_CICD_diffs$NDPs = NDPs}

cat("output_checks2:    "); if(identical(exp_output_checks2, output_checks2)){cat("check!\n")} else {cat("THERE IS A DIFFERENCE\n"); fakedata_CICD_diffs$output_checks2 = output_checks2}
cat("report_checks2:    "); if(identical(exp_report_checks2, report_checks2)){cat("check!\n")} else {cat("THERE IS A DIFFERENCE\n"); fakedata_CICD_diffs$report_checks2 = report_checks2}
cat("report_import2:    "); if(identical(exp_report_import2, report_import2)){cat("check!\n")} else {cat("THERE IS A DIFFERENCE\n"); fakedata_CICD_diffs$report_import2 = report_import2}
cat("report_create5:    "); if(identical(exp_report_create5, report_create5)){cat("check!\n")} else {cat("THERE IS A DIFFERENCE\n"); fakedata_CICD_diffs$report_create5 = report_create5}
cat("report_create6:    "); if(identical(exp_report_create6, report_create6)){cat("check!\n")} else {cat("THERE IS A DIFFERENCE\n"); fakedata_CICD_diffs$report_create6 = report_create6}
cat("report_create7:    "); if(identical(exp_report_create7, report_create7)){cat("check!\n")} else {cat("THERE IS A DIFFERENCE\n"); fakedata_CICD_diffs$report_create7 = report_create7}
cat("report_copy2:      "); if(identical(exp_report_copy2, report_copy2)){cat("check!\n")} else {cat("THERE IS A DIFFERENCE\n"); fakedata_CICD_diffs$report_copy2 = report_copy2}
cat("report_copy_many2: "); if(identical(exp_report_copy_many2, report_copy_many2)){cat("check!\n")} else {cat("THERE IS A DIFFERENCE\n"); fakedata_CICD_diffs$report_copy_many2 = report_copy_many2}
cat("report_view5:      "); if(identical(exp_report_view5, report_view5)){cat("check!\n")} else {cat("THERE IS A DIFFERENCE\n"); fakedata_CICD_diffs$report_view5 = report_view5}
cat("report_view6:      "); if(identical(exp_report_view6, report_view6)){cat("check!\n")} else {cat("THERE IS A DIFFERENCE\n"); fakedata_CICD_diffs$report_view6 = report_view6}

cat("output_checks3:    "); if(identical(exp_output_checks3, output_checks3)){cat("check!\n")} else {cat("THERE IS A DIFFERENCE\n"); fakedata_CICD_diffs$output_checks3 = output_checks3}
cat("report_checks3:    "); if(identical(exp_report_checks3, report_checks3)){cat("check!\n")} else {cat("THERE IS A DIFFERENCE\n"); fakedata_CICD_diffs$report_checks3 = report_checks3}
cat("report_import3:    "); if(identical(exp_report_import3, report_import3)){cat("check!\n")} else {cat("THERE IS A DIFFERENCE\n"); fakedata_CICD_diffs$report_import3 = report_import3}
cat("output_checks4:    "); if(identical(exp_output_checks4, output_checks4)){cat("check!\n")} else {cat("THERE IS A DIFFERENCE\n"); fakedata_CICD_diffs$output_checks4 = output_checks4}
cat("report_checks4:    "); if(identical(exp_report_checks4, report_checks4)){cat("check!\n")} else {cat("THERE IS A DIFFERENCE\n"); fakedata_CICD_diffs$report_checks4 = report_checks4}
cat("report_diffdf7:    "); if(identical(exp_report_diffdf7, report_diffdf7)){cat("check!\n")} else {cat("THERE IS A DIFFERENCE\n"); fakedata_CICD_diffs$report_diffdf7 = report_diffdf7}

save(fakedata_CICD_diffs, file = "fakedata_CICD_diffs.Rdata")

