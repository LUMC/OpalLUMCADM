
## Load libraries
library(opalr)
library(OpalLUMCADM)

## Set opal-demo login
opal = opal.login(
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

## Check dimensions of object
if (!all(dim(df$datafile1) == c(2163, 12))) {
  stop("Wrong dimensions!", dim(df$datafile1))
}
if (!all(dim(df$datafile2) == c(2163, 12))) {
  stop("Wrong dimensions!", dim(df$datafile1))
}
if (!all(dim(df$dictionary1$variables) == c(11, 10))) {
  stop("Wrong dimensions!", dim(df$datafile1))
}
if (!all(dim(df$dictionary1$categories) == c(13, 4))) {
  stop("Wrong dimensions!", dim(df$datafile1))
}

message("Table is loaded correctly")
