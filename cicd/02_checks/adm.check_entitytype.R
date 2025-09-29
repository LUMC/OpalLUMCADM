
## Load libraries
library(testthat)
library(OpalLUMCADM)

## Load dataset
load("./cicd/02_checks/dataset_cnsim.Rdata")


## Run test with success
test_that("success", {
  expect_message(
    adm.check_entitytype(
      variables = variables
    ),
    "Checking entity type..."
  )
  expect_message(
    adm.check_entitytype(
      variables = variables
    ),
    "Entity type checked!"
  )
})


## Add extra entity type for error
variables$entityType[3] <- "invalid"

## Run test with error
test_that("error", {
  expect_error(
    adm.check_entitytype(
      variables = variables
    ),
    "More then one entity type in use!"
  )
})
