
## Load libraries
library(rlang)
library(testthat)
library(OpalLUMCADM)

## Load dataset
load("./cicd/03_diffdf/dataset_cnsim.Rdata")


## Run check for success
test_that("success", {
  output <- capture.output(
    adm.check_diffdf(
      datafile1 = datafile1,
      datafile2 = datafile2,
      path = NA
    )
  )
  
  expect_true(grepl("No issues were found!", output))
})


## Run check for warning
test_that("warning", {
  expect_warning(
    adm.check_diffdf(
      datafile1 = datafile1,
      datafile2 = datafile2,
      path = "./cicd/03_diffdf/diffdf_output1.xlsx"
    ),
    "Workbook does not contain any worksheets. A worksheet will be added."
  )
  
  expect_true(file.exists("./cicd/03_diffdf/diffdf_output1.xlsx"))
})


## Set datetime value for differences
datafile1$LAB_TSC[1] <- "different"

## Run check for warning
test_that("warning", {
  expect_warning(
    adm.check_diffdf(
      datafile1 = datafile1,
      datafile2 = datafile2,
      path = "./cicd/03_diffdf/diffdf_output2.xlsx"
    ),
    "Not all Values Compared Equal"
  )
  
  expect_true(file.exists("./cicd/03_diffdf/diffdf_output2.xlsx"))
})
