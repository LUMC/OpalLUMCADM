
## Load libraries
library(testthat)
library(OpalLUMCADM)

## Load dataset
load("./cicd/02_checks/dataset_cnsim.Rdata")


## Run check with success
test_that("success", {
  expect_message(
    check.character_ids(
      datafile = datafile
    ),
    "Checked for ID as character"
  )
})


## Set IDs as integer
datafile$id <- 1:nrow(datafile)

## Run test with warning
test_that("warning", {
  expect_warning(
    check.character_ids(
      datafile = datafile
    ),
    "ID values are not listed as character"
  )
})
