
## Load libraries
library(testthat)
library(lubridate)
library(OpalLUMCADM)

## Load dataset
load("./cicd/02_checks/dataset_cnsim.Rdata")


## Run test with success
test_that("success", {
  expect_message(
    adm.check_datetime(
      datafile = datafile,
      variables = variables
    ),
    "Checked datetime format"
  )
})


## Set datetime value for success
datafile$LAB_TSC <- as.POSIXct("2025-05-05 12:00")
variables$valueType[1] <- "datetime"

## Run test with success
test_that("success", {
  expect_message(
    adm.check_datetime(
      datafile = datafile,
      variables = variables
    ),
    "Checked datetime format"
  )
})


## Set date value for warning
datafile$LAB_TSC <- "2025-29-29 12:00"
variables$valueType[1] <- "datetime"

## Run test with warning
test_that("warning", {
  expect_warning(
    adm.check_datetime(
      datafile = datafile,
      variables = variables
    ),
    "Some datetime columns don't have POSIXct format: `yyyy-mm-dd hh:mm:ss`: LAB_TSC"
  )
})
