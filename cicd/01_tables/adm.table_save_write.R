
## Load libraries
library(opalr)
library(dplyr)
library(OpalLUMCADM)

## Set opal-demo login
opal = opal.login(
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
adm.table_save(
  opal = opal,
  projname = "CNSIM",
  tablename = "CNSIM_TEST",
  datafile = df,
  method = "write"
)

## Get dataframe
df_get <- adm.table_get(
  opal = opal,
  projname = "CNSIM",
  tablename = "CNSIM_TEST"
)

## Check dimensions of object
if (!all(dim(df_get$datafile1) == dim(df))) {
  stop("Wrong dimensions on 'write'!")
}

message("Table was saved correctly")
