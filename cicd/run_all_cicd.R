
library(OpalLUMCADM)

## For development only

## 01_tables
source("./cicd/01_tables/adm.table_get.R")
source("./cicd/01_tables/adm.table_save_write.R")
source("./cicd/01_tables/adm.table_save_update.R")
source("./cicd/01_tables/adm.table_save_overwrite.R")
source("./cicd/01_tables/cleanup.R")

## 02_checks
source("./cicd/02_checks/adm.check_columns_cat.R")
source("./cicd/02_checks/adm.check_columns_var.R")
source("./cicd/02_checks/adm.check_date.R")
source("./cicd/02_checks/adm.check_datetime.R")
source("./cicd/02_checks/adm.check_encrypted_values.R")
source("./cicd/02_checks/adm.check_entitytype.R")
source("./cicd/02_checks/adm.check_ids.R")
source("./cicd/02_checks/adm.check_infinite.R")
source("./cicd/02_checks/adm.check_minmax.R")
source("./cicd/02_checks/adm.check_required_columns.R")
source("./cicd/02_checks/adm.check_valuetype.R")
source("./cicd/02_checks/adm.run_all_checks.R")

## 03_diffdf
source("./cicd/03_diffdf/adm.check_diffdf.R")

## 04_views
source("./cicd/04_views/adm.view_create.R")
source("./cicd/04_views/cleanup.R")
