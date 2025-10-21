
## Load libraries
library(rlang)
library(testthat)
library(OpalLUMCADM)

## Load dataset
load("./cicd/02_checks/dataset_cnsim.Rdata")


## Run test with success
test_that("success", {
  expect_message(
    adm.check_columns_cat(
      datafile = datafile,
      categorie = categories
    ),
    "Checked columns between datafile & categories"
  )
})

## Remove 'id' column for success
categories$variable[1] <- "test"

## Run check with warning
test_that("warning", {
  expect_warning(
    adm.check_columns_cat(
      datafile = datafile,
      categories = categories
    ),
    "Column 'test' in data: FALSE & column 'test' in categories: TRUE"
  )
})





