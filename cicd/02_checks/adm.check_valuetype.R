
## Load libraries
library(testthat)
library(OpalLUMCADM)

## Load dataset
load("./cicd/02_checks/dataset_cnsim.Rdata")


## Run test with success
test_that("success", {
  expect_message(
    adm.check_valuetype(
      datafile = datafile,
      variables = variables
    ),
    "Checked valuetypes"
  )
})


## Change a valuetype for warning
variables$valueType[3] <- NA

## Run check with warning
test_that("warning", {
  expect_warning(
    adm.check_valuetype(
      datafile = datafile,
      variables = variables
    ),
    "ValueType of 'LAB_HDL' doesn't match: NA vs decimal"
  )
})
