
## Load libraries
library(testthat)
library(OpalLUMCADM)

## Load dataset
load("./cicd/02_checks/dataset_cnsim.Rdata")


## Run check with warning
test_that("success", {
  expect_message(
    check.var_column_datatype(
      variables = variables
    ),
    "Checked variable object datatypes"
  )
})


## Add column with integers for warning
variables$test <- 1:nrow(variables)


## Run test with warning
test_that("warning", {
  expect_warning(
    check.var_column_datatype(
      variables = variables
    ),
    "Not all columns in variable object are character \\(could cause ParseException\\)"
  )
})
