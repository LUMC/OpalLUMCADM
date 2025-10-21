
## Load libraries
library(testthat)
library(OpalLUMCADM)

## Load dataset
load("./cicd/02_checks/dataset_cnsim.Rdata")


## Run test
test_that("success", {
  expect_message(
    test_df <- adm.run_all_checks(
      datafile = datafile,
      variables = variables,
      categories = categories
    ),
    "All checks done!"
  )
})

## Check dimensions of object
if (!all(dim(test_df) == c(11, 3))) {
  stop("Wrong dimensions!", dim(test_df))
}

## Check type output
if (table(test_df$type)[["OK"]] != 7) {
  stop("Wrong number of output `OK`!")
}
if (table(test_df$type)[["WARNING"]] != 3) {
  stop("Wrong number of output `WARNING`!")
}
if (table(test_df$type)[["ERROR"]] != 1) {
  stop("Wrong number of output `ERROR`!")
}
