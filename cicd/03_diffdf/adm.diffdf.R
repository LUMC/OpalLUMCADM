
## Load libraries
library(rlang)
library(testthat)
library(OpalLUMCADM)

## Load dataset
load("./cicd/03_diffdf/dataset_cnsim.Rdata")


## Run check for success
test_that("success", {
  output <- capture.output(
    adm.diffdf(
      datafile1 = datafile1,
      datafile2 = datafile2
    )
  )
  
  expect_true(grepl("No issues were found!", output))
})


## Set datetime value for differences
datafile1$LAB_TSC[1] <- "different"

## Run check for warning
test_that("warning", {
  expect_warning(
    adm.diffdf(
      datafile1 = datafile1,
      datafile2 = datafile2
    ),
    "Not all Values Compared Equal"
  )
})
