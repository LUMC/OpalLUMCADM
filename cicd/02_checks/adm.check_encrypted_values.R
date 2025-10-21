
## Load libraries
library(testthat)
library(dplyr)
library(OpalLUMCADM)

## Load dataset
load("./cicd/02_checks/dataset_cnsim.Rdata")


## Run test with error
test_that("error", {
  expect_error(
    adm.check_encrypted_values(
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
    adm.check_encrypted_values(
      datafile = datafile,
      variables = variables
    ),
    "Checked encrypted values"
  )
})


## Add 'encrypted' column for error
variables$encrypted[2] <- "SI"

## Run test with error
test_that("error", {
  expect_error(
    adm.check_encrypted_values(
      datafile = datafile, 
      variables = variables
    ),
    regexp = "Some columns are not encrypted.*LAB_TRIG"
  )
})


## Add 'encrypted' values for success
datafile$LAB_TRIG <- paste0("1:", datafile$LAB_TRIG)

## Run test with success
test_that("success", {
  expect_message(
    adm.check_encrypted_values(
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
    adm.check_encrypted_values(
      datafile = datafile, 
      variables = variables
    ),
    regexp = "Some columns are not encrypted.*LAB_TRIG"
  )
})


## Add 'encrypted' values for success
datafile$LAB_TRIG <- paste0("3::", datafile$LAB_TRIG)

## Run test with success
test_that("success", {
  expect_message(
    adm.check_encrypted_values(
      datafile = datafile, 
      variables = variables
    ),
    "Checked encrypted values"
  )
})
