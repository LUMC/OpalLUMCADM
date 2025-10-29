
## Load libraries
library(testthat)
library(OpalLUMCADM)

## Load dataset
load("./cicd/02_checks/dataset_cnsim.Rdata")


## Run test with success
test_that("success", {
  expect_message(
    check.duplicated_ids(
      datafile = datafile
    ),
    "Checked for duplicated IDs"
  )
})


## Add a duplicated ID
datafile$id[2] <- datafile$id[1]

## Run check with warning
test_that("warning", {
  expect_warning(
    check.duplicated_ids(
      datafile = datafile
    ),
    "There are some duplicated IDs in column `id`"
  )
})
