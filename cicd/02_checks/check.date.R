
## Load libraries
library(testthat)
library(lubridate)
library(OpalLUMCADM)

## Load dataset
load("./cicd/02_checks/dataset_cnsim.Rdata")


## Run test with success
test_that("success", {
  expect_message(
    check.date(
      datafile = datafile,
      variables = variables
    ),
    "Checked date format"
  )
})


## Set date value for success
datafile$LAB_TSC <- as.POSIXct("2025-05-05")
variables$valueType[1] <- "date"

## Run test with success
test_that("success", {
  expect_message(
    check.date(
      datafile = datafile,
      variables = variables
    ),
    "Checked date format"
  )
})


## Set date value for warning
datafile$LAB_TSC <- "2025-29-29"
variables$valueType[1] <- "date"

## Run test with warning
test_that("warning", {
  expect_warning(
    check.date(
      datafile = datafile,
      variables = variables
    ),
    "Some date columns don't have Date format: `%Y-%m-%d`: LAB_TSC"
  )
})
