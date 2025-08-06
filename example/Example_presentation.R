# Start -------------------------------------------------------------------
## Script to show how Opal and Mica work
#
## written by: Lars van der Burg
## written on: 27-05-2025


# Initiations -------------------------------------------------------------
library(tidyverse)
library(keyring)
library(opalr)
library(diffdf)
library(OpalLUMCADM)


## Opal -------------------------------------------------------------------
opal_url = "https://opal-acc93.clinicalresearch.nl"
opal_token = keyring::key_get("token_acc_clinicalresearch")

opal = opalr::opal.login(url = opal_url, token = opal_token)

projname = "Example"
tablename = "Example_data"


## Data -------------------------------------------------------------------
load("example/Example_datafile.RData"); datafile
load("example/Example_var.RData"); var
load("example/Example_cat.RData"); cat


# Export ------------------------------------------------------------------
OpalLUMCADM::write_table_R2opal(opal = opal, projname = projname, tablename = tablename,
                                datafile = datafile, var = var, cat = cat,
                                ent = "Participant", action = "write")


# Taxonomies --------------------------------------------------------------
annotations = openxlsx::read.xlsx("Mica/Taxonomy_example.xlsx", sheet = "Annotations")
opalr::opal.annotate(opal = opal, datasource = projname, table = tablename, annotations = annotations)


# Import ------------------------------------------------------------------
datadict = OpalLUMCADM::import_table_opal2R(opal = opal, projname = projname, tablename = tablename)
datadict$datafile4copy


# View --------------------------------------------------------------------
OpalLUMCADM::make_opal_view(opal = opal, projname = projname, tablename = tablename,
                            opal_view = opal, projname_view = projname, tablename_view = paste0(tablename, "_view"),
                            var = var, cat = cat, ent = "Participant", EntityFilter = c("group" = "A"), report_path = FALSE)

