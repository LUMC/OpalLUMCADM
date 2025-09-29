
## Load libraries
library(testthat)
library(stringr)
library(OpalLUMCADM)

## Load dataset
load("./cicd/02_checks/dataset_cnsim.Rdata")


## Run test with warning
test_that("warning", {
  expect_warning(
    adm.check_required_columns(
      variables = variables
    ),
    "There is no 'description' column in your variables object!"
  )
})


## Add description for warning
variables$description <- "test description"

## Run test with warning
test_that("warning", {
  expect_warning(
    adm.check_required_columns(
      variables = variables
    ),
    "There is no 'encrypted' column in your variables object!"
  )
})

## Add encrypted & remove label for warning
variables$encrypted <- "test encrypted"
variables$`label:en` <- NULL

## Run test with warning
test_that("warning", {
  expect_warning(
    adm.check_required_columns(
      variables = variables
    ),
    "There is no 'label' column in your variables object!"
  )
})


## Add label for success
variables$`label:en` <- "test label"

## Run test with error
test_that("success", {
  expect_message(
    adm.check_required_columns(
      variables = variables
    ),
    "Checking required columns..."
  )
  expect_message(
    adm.check_required_columns(
      variables = variables
    ),
    "Columns checked!"
  )
})
