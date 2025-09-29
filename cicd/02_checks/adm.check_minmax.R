
## Load libraries
library(testthat)
library(OpalLUMCADM)

## Load dataset
load("./cicd/02_checks/dataset_cnsim.Rdata")


## Run check with error
test_that("error", {
  expect_error(
    adm.check_minmax(
      datafile = datafile,
      variables = variables
    ),
    "There are no min/max columns in variables object"
  )
})


## Add min/max columns for warning
variables$min <- -1
variables$max <- 50

## Run test with warning
test_that("warning", {
  expect_warning(
    adm.check_minmax(
      datafile = datafile, 
      variables = variables
    ),
    "'LAB_TRIG' minimum value to low: -3.434 < -1"
  )
  expect_warning(
    adm.check_minmax(
      datafile = datafile,
      variables = variables
    ),
    "'PM_BMI_CONTINUOUS' maximum value to high: 52.29 > 50"
  )
})


## Add min/max columns for success
variables$min <- -5
variables$max <- 60

## Run test with success
test_that("success", {
  expect_message(
    adm.check_minmax(
      datafile = datafile, 
      variables = variables
    ),
    "Checking min/max values..."
  )
  expect_message(
    adm.check_minmax(
      datafile = datafile,
      variables = variables
    ),
    "All min/max values checked!"
  )
})
