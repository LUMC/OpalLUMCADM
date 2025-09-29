
## Load libraries
library(testthat)
library(OpalLUMCADM)

## Load dataset
load("./cicd/02_checks/dataset_cnsim.Rdata")


## Run test with success
test_that("success", {
  expect_message(
    adm.check_infinite(
      datafile = datafile
    ),
    "Checking infinite values..."
  )
  expect_message(
    adm.check_infinite(
      datafile = datafile
    ),
    "Infinite values checked!"
  )
})


## Set an infinite value for warning
datafile$MEDI_LPD <- as.numeric(1)
datafile$MEDI_LPD[1] <- Inf

## Run test with warning
test_that("warning", {
  expect_warning(
    adm.check_infinite(
      datafile = datafile
    ),
    "Some columns have Infinite values: MEDI_LPD"
  )
})
