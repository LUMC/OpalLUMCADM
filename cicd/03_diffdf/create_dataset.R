
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
  project = "CNSIM",
  table = "CNSIM1"
)

## Set datasets
datafile1 <- df$datafile
datafile2 <- df$datafile
variables <- df$dictionary$variables
categories <- df$dictionary$categories

## Save as dataframe
save(datafile1, datafile2, variables, categories, file = "./cicd/03_diffdf/dataset_cnsim.Rdata")
