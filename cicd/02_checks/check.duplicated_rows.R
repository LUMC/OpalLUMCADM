
## Load libraries
library(testthat)
library(OpalLUMCADM)

## Load dataset
load("./cicd/02_checks/dataset_cnsim.Rdata")


## Run test with success
test_that("success", {
  expect_message(
    check.duplicated_rows(
      datafile = datafile,
      variables = variables,
      categories = categories
    ),
    "Checked for duplicated row in variable/categorie objects"
  )
})


## Add a duplicated row in variables
variables[2, ] <- variables[1, ]

## Run check with warning
test_that("warning", {
  expect_warning(
    check.duplicated_rows(
      datafile = datafile,
      variables = variables,
      categories = categories
    ),
    "There are duplicated rows in variables object"
  )
})

## Add a duplicated row in categories
categories[2, ] <- categories[1, ]

## Run check with warning
test_that("warning", {
  expect_warning(
    check.duplicated_rows(
      datafile = datafile,
      variables = variables,
      categories = categories
    ),
    "There are duplicated rows in categories object"
  )
})
