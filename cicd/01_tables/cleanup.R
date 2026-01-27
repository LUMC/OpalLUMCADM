
## Load libraries
library(opalr)
library(OpalLUMCADM)

## Set opal-demo login
opal <- opal.login(
  username = "administrator",
  password = "password",
  url = "https://opal-demo.obiba.org/"
)

## Cleanup
opal.table_delete(
  opal = opal,
  project = "CNSIM",
  table = "CNSIM_TEST"
)
opal.table_delete(
  opal = opal,
  project = "CNSIM",
  table = "CNSIM_TEST2"
)

message("Cleanup is done!")
