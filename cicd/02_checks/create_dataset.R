
## Load libraries
library(opalr)
library(OpalLUMCADM)

## Set opal-demo login
opal <- opal.login(
  username = "administrator",
  password = "password",
  url = "https://opal-demo.obiba.org/"
)

## Get dataframe
df <- adm.table_get(
  opal = opal,
  projname = "CNSIM",
  tablename = "CNSIM1"
)

## Set datasets
datafile <- df$datafile1
variables <- df$dictionary1$variables
categories <- df$dictionary1$categories

## Save as dataframe
save(datafile, variables, categories, file = "./cicd/02_checks/dataset_cnsim.Rdata")
