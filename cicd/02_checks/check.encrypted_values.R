
## Load libraries
library(testthat)
library(dplyr)
library(OpalLUMCADM)

## Load dataset
load("./cicd/02_checks/dataset_cnsim.Rdata")


## Run test with error
test_that("error", {
  expect_error(
    check.encrypted_values(
      datafile = datafile,
      variables = variables
    ),
    "There is no 'encrypted' column in variables object"
  )
})


## Add 'encrypted' column for success
variables$encrypted <- "no"

## Run test with success
test_that("message", {
  expect_message(
    check.encrypted_values(
      datafile = datafile,
      variables = variables
    ),
    "Checked encrypted values"
  )
})

variables$encrypted[2] <- "test"

## Run check with warning
test_that("warning", {
  expect_warning(
    check.encrypted_values(
      datafile = datafile,
      variables = variables
    ),
    "There are values in encrypted columns that don't match: no, yes or SI"
  )
})


## Add 'encrypted' column for error
variables$encrypted[2] <- "SI"

## Run test with error
test_that("error", {
  expect_error(
    check.encrypted_values(
      datafile = datafile, 
      variables = variables
    ),
    regexp = "Some columns are not encrypted.*LAB_TRIG"
  )
})

## Add 'encrypted' values for error (1:, but to short)
datafile$LAB_TRIG <- paste0("1:", strrep("a", 60))

## Run test with error
test_that("error", {
  expect_error(
    check.encrypted_values(
      datafile = datafile, 
      variables = variables
    ),
    "Some columns are not encrypted.*LAB_TRIG"
  )
})

## Add 'encrypted' values for error (no 1:, but matching 66 characters)
datafile$LAB_TRIG <- paste0("TE", strrep("a", 64))

## Run test with error
test_that("error", {
  expect_error(
    check.encrypted_values(
      datafile = datafile, 
      variables = variables
    ),
    "Some columns are not encrypted.*LAB_TRIG"
  )
})

## Add 'encrypted' values for success
datafile$LAB_TRIG <- paste0("1:", strrep("a", 64))

## Run test with success
test_that("success", {
  expect_message(
    check.encrypted_values(
      datafile = datafile, 
      variables = variables
    ),
    "Checked encrypted values"
  )
})


## Add 'encrypted' column for error
variables$encrypted[2] <- "yes"

## Run test with error
test_that("error", {
  expect_error(
    check.encrypted_values(
      datafile = datafile, 
      variables = variables
    ),
    regexp = "Some columns are not encrypted.*LAB_TRIG"
  )
})

## Add 'encrypted' values for error (3::, but to short)
datafile$LAB_TRIG <- paste0("3::", strrep("a", 90))

## Run test with error
test_that("error", {
  expect_error(
    check.encrypted_values(
      datafile = datafile, 
      variables = variables
    ),
    regexp = "Some columns are not encrypted.*LAB_TRIG"
  )
})

## Add 'encrypted' values for error (no 3::, but matching 100+ characters)
datafile$LAB_TRIG <- paste0("TEE", strrep("a", 97))

## Run test with error
test_that("error", {
  expect_error(
    check.encrypted_values(
      datafile = datafile, 
      variables = variables
    ),
    regexp = "Some columns are not encrypted.*LAB_TRIG"
  )
})

## Add 'encrypted' values for success
datafile$LAB_TRIG <- paste0("3::", strrep("a", 97))

## Run test with success
test_that("success", {
  expect_message(
    check.encrypted_values(
      datafile = datafile, 
      variables = variables
    ),
    "Checked encrypted values"
  )
})
