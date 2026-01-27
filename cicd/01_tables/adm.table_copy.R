
## Load libraries
library(opalr)
library(dplyr)
library(OpalLUMCADM)

## Set opal-demo login
opal <- opal.login(
  username = "administrator",
  password = "password",
  url = "https://opal-demo.obiba.org/"
)

## Copy dataframe
diffdf_output <- adm.table_copy(
  opal_src = opal,
  opal_dst = opal,
  project_src = "CNSIM",
  project_dst = "CNSIM",
  table_src = "CNSIM_TEST",
  table_dst = "CNSIM_TEST2"
)

## Get dataframe original
df_original <- adm.table_get(
  opal = opal,
  project = "CNSIM",
  table = "CNSIM_TEST"
)

## Get dataframe original
df_copy <- adm.table_get(
  opal = opal,
  project = "CNSIM",
  table = "CNSIM_TEST2"
)


## Check dimensions of object
if (!all(dim(df_original$datafile) == dim(df_copy$datafile))) {
  stop("Wrong dimensions on copy!")
}

message("Table was copied correctly")
