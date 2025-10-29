
## Load libraries
library(testthat)
library(stringr)
library(OpalLUMCADM)

## Load dataset
load("./cicd/02_checks/dataset_cnsim.Rdata")


## Run test with warning
test_that("warning", {
  expect_warning(
    check.required_columns(
      variables = variables
    ),
    "There is no 'encrypted' column in variables object"
  )
})


## Add encrypted for warning & remove entitytype for warning
variables$encrypted <- "test encrypted"
variables$entityType <- NULL

## Run test with warning
test_that("warning", {
  expect_warning(
    check.required_columns(
      variables = variables
    ),
    "There is no 'entityType' column in variables object"
  )
})

## Add entityType & remove label for warning
variables$entityType <- "test entityType"
variables$`label:en` <- NULL

## Run test with warning
test_that("warning", {
  expect_warning(
    check.required_columns(
      variables = variables
    ),
    "There is no 'label' column in variables object"
  )
})


## Add label for success
variables$`label:en` <- "test label"

## Run test with error
test_that("success", {
  expect_message(
    check.required_columns(
      variables = variables
    ),
    "Checked required columns"
  )
})
