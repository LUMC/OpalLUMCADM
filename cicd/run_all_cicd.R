
library(OpalLUMCADM)

## For development only

## 01_tables
source("./cicd/01_tables/adm.table_get.R")
source("./cicd/01_tables/adm.table_save_write.R")
source("./cicd/01_tables/adm.table_save_update.R")
source("./cicd/01_tables/adm.table_save_overwrite.R")
source("./cicd/01_tables/cleanup.R")

## 02_checks
source("./cicd/02_checks/check.columns_cat.R")
source("./cicd/02_checks/check.columns_var.R")
source("./cicd/02_checks/check.date.R")
source("./cicd/02_checks/check.datetime.R")
source("./cicd/02_checks/check.duplicated_ids.R")
source("./cicd/02_checks/check.duplicated_rows.R")
source("./cicd/02_checks/check.encrypted_values.R")
source("./cicd/02_checks/check.entitytype.R")
source("./cicd/02_checks/check.infinite.R")
source("./cicd/02_checks/check.minmax.R")
source("./cicd/02_checks/check.required_columns.R")
source("./cicd/02_checks/check.valuetype.R")
source("./cicd/02_checks/check.run_all.R")

## 03_diffdf
source("./cicd/03_diffdf/adm.diffdf.R")

## 04_views
source("./cicd/04_views/adm.view_create.R")
source("./cicd/04_views/cleanup.R")
