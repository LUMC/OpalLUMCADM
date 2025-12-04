
## Load libraries
library(rlang)
library(testthat)
library(OpalLUMCADM)

## Load dataset
load("./cicd/02_checks/dataset_cnsim.Rdata")


## Run test with success
test_that("success", {
  expect_message(
    check.columns_var(
      datafile = datafile,
      variables = variables
    ),
    "Checked columns between datafile & variables"
  )
})


## Add column
datafile$test <- 0

## Run check with warning
test_that("warning", {
  expect_warning(
    check.columns_var(
      datafile = datafile,
      variables = variables
    ),
    "Column 'test' in data: TRUE & column 'test' in variables: FALSE"
  )
})
