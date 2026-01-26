
## Load libraries
library(opalr)
library(testthat)
library(OpalLUMCADM)

## Set opal-demo login
opal <- opal.login(
  username = "administrator",
  password = "password",
  url = "https://opal-demo.obiba.org/"
)

## Get dictionary
dict <- opal.table_dictionary_get(
  opal = opal,
  project = "CNSIM",
  table = "CNSIM1"
)

## Create view
test_that("success", {
  expect_message(
    adm.view_create(
      opal = opal,
      project = "CNSIM",
      table = "CNSIM_TEST_VIEW",
      source = c("CNSIM.CNSIM1"),
      variables = dict$variables,
      categories = dict$categories
    ),
    "Creating new view..."
  )
})


## Get view
df_get <- adm.table_get(
  opal = opal,
  project = "CNSIM",
  table = "CNSIM_TEST_VIEW"
)

## Check dimensions of object
if (!all(dim(df_get$datafile) == c(2163, 12))) {
  stop("Wrong dimensions of view!")
}

## Check content of object
if (round(mean(df_get$datafile$LAB_TSC, na.rm = TRUE), 2) != 5.87) {
  stop("Content of data is wrong!")
}

message("View is loaded correctly")


## Update view
test_that("success", {
  expect_message(
    adm.view_create(
      opal = opal,
      project = "CNSIM",
      table = "CNSIM_TEST_VIEW",
      source = c("CNSIM.CNSIM1"),
      variables = dict$variables,
      categories = dict$categories
    ),
    "Updating existing view..."
  )
})
