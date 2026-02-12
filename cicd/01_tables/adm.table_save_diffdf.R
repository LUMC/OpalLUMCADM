
## Load libraries
library(opalr)
library(dplyr)
library(testthat)
library(OpalLUMCADM)

## Set opal-demo login
opal <- opal.login(
  username = "administrator",
  password = "password",
  url = "https://opal-demo.obiba.org/"
)

## Create dataframes
df <- tibble(
  id = 1:3,
  a = c("A", "B", "C"),
  b = c(1, 2, 3)
)

## Save dataframe
findings <- adm.table_save(
  opal = opal,
  project = "CNSIM",
  table = "CNSIM_TEST",
  datafile = df,
  method = "overwrite",
  diffdf = TRUE
)

## Run check for success
test_that("success", {
  output <- capture.output(
    adm.table_save(
      opal = opal,
      project = "CNSIM",
      table = "CNSIM_TEST",
      datafile = df,
      method = "overwrite",
      diffdf = TRUE
    )
  )
  
  expect_equal(
    object = output,
    expected = c("$datafile", "No issues were found!", "", "$variables", "No issues were found!", "")
  )
})
