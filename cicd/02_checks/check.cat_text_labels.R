
## Load libraries
library(testthat)
library(OpalLUMCADM)

## Load dataset
load("./cicd/02_checks/dataset_cnsim.Rdata")


## Run check with warning
test_that("success", {
  expect_message(
    check.cat_text_labels(
      datafile = datafile,
      categories = categories
    ),
    "Checked categorie text labels"
  )
})


## Add column with integers for warning
datafile$DIS_CVA <- "TEST"


## Run test with warning
test_that("warning", {
  expect_warning(
    check.cat_text_labels(
      datafile = datafile,
      categories = categories
    ),
    "Categorie object is possibly missing a value for column: `DIS_CVA`"
  )
})
