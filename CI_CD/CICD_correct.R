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
library(keyring)


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

source("CI_CD/CICD_procedure.R")


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
  opal_url = "https://opal.clinicalresearch.nl"; opal_username = "administrator"; opal_password = "Testing1!"; projname = "TEST_TOM"
  opal_token = NULL

}


# Procedure ---------------------------------------------------------------
fakedata_exp = CICD_procedure(opal_url = opal_url, opal_username = opal_username, opal_password = opal_password, opal_token = opal_token,
                              projname = projname, datafile = datafile, var = var, cat = cat)


# Expected outcome --------------------------------------------------------
names(fakedata_exp) = paste0("exp_", names(fakedata_exp))
save(fakedata_exp, file = "CI_CD/fakedata_exp.RData")

save(fakedata_exp, file = paste0("CI_CD/", server, "_fakedata_exp_", format(Sys.Date()), ".RData"))

