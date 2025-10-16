
## Load libraries
library(rlang)
library(testthat)
library(OpalLUMCADM)

## Load dataset
load("./cicd/02_checks/dataset_cnsim.Rdata")


## Run check with warning
test_that("warning", {
  expect_warning(
    adm.check_columns_var(
      datafile = datafile,
      variables = variables
    ),
    "Column 'id' in data: TRUE & column 'id' in variables: FALSE"
  )
})


## Remove 'id' column for success
datafile$id <- NULL

## Run test with success
test_that("success", {
  expect_message(
    adm.check_columns_var(
      datafile = datafile,
      variables = variables
    ),
    "Checked columns between datafile & variables"
  )
})
