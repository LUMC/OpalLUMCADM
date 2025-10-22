
library(OpalLUMCADM)

## For development
## Run all CICD checks

## Script paths
## path = "./cicd/",
## path = "./cicd/02_checks/",

## Get all scripts
scripts <- list.files(
  path = "./cicd/02_checks/",
  pattern = "\\.R$",
  recursive = TRUE,
  full.names = TRUE
)

## Run each script
for (script in scripts) {
  message("Running: ", script)
  source(script)
}
