
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

message("Cleanup is done!")
