
## Load libraries
library(opalr)
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
adm.view_create(
  opal = opal,
  view_projname = "CNSIM",
  view_tablename = "CNSIM_TEST_VIEW",
  source = c("CNSIM.CNSIM1", "CNSIM.CNSIM2"),
  variables = dict$variables,
  categories = dict$categories
)

## Get view
df_get <- adm.table_get(
  opal = opal,
  projname = "CNSIM",
  tablename = "CNSIM_TEST_VIEW"
)

## Check dimensions of object
if (!all(dim(df_get$datafile1) == c(3088, 12))) {
  stop("Wrong dimensions of view!")
}

message("View is loaded correctly")
