## Script for the CI/CD check done via gitlab
## With this CI/CD check we compare the expected/true reports for a dataset with newly run reports for the same dataset
## These newly run reports are automatically obtained after each push to gitlab
#
## Procedure is as follows:
## - Create a fake dataset/dictionary, this is done via create_datafile.R and random_data.R. This can be re-used for every new comparison
## - Run here the expected/true reports for this dataset/dictionary. That can be done in this script.
##### This should only be done at big moments, when you know that the current package version is ~100% correct.
##### So when functionalities are really changed, a new package version or the CICD_procedure is extended
## - The CICD_procedure.R script has to be used for both the expected/true reports, as for the newly run reports
##### When you add steps here, dont forget to add also a comparison to CICD_check.R
## - with the .gitlab-ci.yml file you define the CI/CD pipeline
## - In gitlab this .gitlab-ci.yml file will run the CICD_check.R script, which runs CICD_procedure.R and performs the comparison
#
#
## Written by: Lars van der Burg
## Written on: 2024-09-05
#
#
#'
#'
#'
CICD_procedure = function(opal_url = "https://opal-demo.obiba.org", opal_username = "administrator", opal_password = "password", opal_token = NULL,
                          projname = "TESTING", datafile, var, cat, encryption = FALSE){

# Initializations ---------------------------------------------------------
  tablename = "fakedata"; tablename_temp = "fakedata_copy"; tablename_view = "fakedata_view"
  tablenames = rep(tablename, 3); tablenames_temp = c("fakedata_copy1", "fakedata_copy2", "fakedata_copy3")

  if(!is.null(opal_token)){
    opal = opal.login(url = opal_url, token = opal_token)
  } else {
    opal = opal.login(url = opal_url, username = opal_username, password = opal_password)
  }


  ## Intermediate opal version did not allow categories for date(time) variables
  # cat = cat |> filter(!(variable %in% c("date2", "date4", "datetime2", "datetime4")))


  datafile_temp = datafile
  var_temp = var
  cat_temp = cat


  if(isTRUE(opalr::opal.table_exists(opal, projname, tablename))){
    delete_table_opal(opal = opal, projname = projname, tablename = tablename, child_lock = FALSE)
  }
  if(isTRUE(opalr::opal.table_exists(opal, projname, tablename_view))){
    delete_table_opal(opal = opal, projname = projname, tablename = tablename_view, child_lock = FALSE)
  }



# Procedure ---------------------------------------------------------------
  output_checks = capture.output(report_checks <<- checks_opal_R(datafile = datafile, var = var, cat = cat, key = "id", min_max = TRUE, silent = FALSE))
  report_change = datafile_conform_var_change(datafile = datafile, var = var)

  write_table_R2opal(opal, projname = projname, tablename = tablename, datafile = datafile, var = var, cat = cat, ent = "Participant", action = "write", child_lock = FALSE)

  report_import1 = import_table_opal2R(opal = opal, projname = projname, tablename = tablename)
  datafile2 = report_import1$datafile; var2 = report_import1$var; cat2 = report_import1$cat


  delete_table_opal(opal = opal, projname = projname, tablename = tablename, child_lock = FALSE)


  report_diffdf = check_diffdf_opal_generic(datafile, datafile2, var, var2, cat, cat2, comparison = "both", comp_key = "id", suppress_warnings = TRUE,
                                            report_path = NULL, opt_rm_VarDiff_null = TRUE, opt_calc_VarDiff_diff = TRUE, opt_calc_VarDiff_spaces = TRUE)

  report_diffdf2 = check_diffdf_opal_generic(datafile2, report_import1$datafile4copy, var2, report_import1$var4copy, cat2, report_import1$cat4copy,
                                             comparison = "both", comp_key = "id", suppress_warnings = TRUE,
                                             report_path = NULL, opt_rm_VarDiff_null = TRUE, opt_calc_VarDiff_diff = TRUE, opt_calc_VarDiff_spaces = TRUE)

  report_diffdf3 = check_diffdf_opal_generic(datafile, datafile2, NULL, NULL, NULL, NULL, comparison = "both", comp_key = "id", suppress_warnings = TRUE,
                                             report_path = NULL, opt_rm_VarDiff_null = TRUE, opt_calc_VarDiff_diff = TRUE, opt_calc_VarDiff_spaces = TRUE)

  report_diffdf4 = check_diffdf_opal_generic(NULL, NULL, var, var2, NULL, NULL, comparison = "both", comp_key = "id", suppress_warnings = TRUE,
                                             report_path = NULL, opt_rm_VarDiff_null = TRUE, opt_calc_VarDiff_diff = TRUE, opt_calc_VarDiff_spaces = TRUE)

  report_diffdf5 = check_diffdf_opal_generic(NULL, NULL, NULL, NULL, cat, cat2, comparison = "both", comp_key = "id", suppress_warnings = TRUE,
                                             report_path = NULL, opt_rm_VarDiff_null = TRUE, opt_calc_VarDiff_diff = TRUE, opt_calc_VarDiff_spaces = TRUE)

  report_diffdf6 = check_diffdf_opal_generic(datafile, datafile2, var, var2, cat, cat2, comparison = "both", comp_key = "id", suppress_warnings = TRUE,
                                             report_path = NULL, opt_rm_VarDiff_null = TRUE, opt_calc_VarDiff_diff = TRUE, opt_calc_VarDiff_spaces = TRUE,
                                             opt_rm_VarDiff_diff_0 = TRUE, opt_repl_castor = TRUE, aggregate_VarDiff = TRUE)


## Create
  report_create = import_create_table_opal(opal = opal, projname = projname, tablename = tablename,
                                           datafile = datafile, var = var, cat = cat,
                                           ent = "Participant", action = "write", id.name = "id", report_path = NULL, comparison = "both")

  report_create2 = import_create_table_opal(opal = opal, projname = projname, tablename = tablename, child_lock = FALSE,
                                            datafile = datafile, var = var, cat = cat,
                                            ent = "Participant", action = "update", id.name = "id", report_path = NULL, comparison = "both")

  report_create3 = import_create_table_opal(opal = opal, projname = projname, tablename = tablename, child_lock = FALSE,
                                            datafile = datafile, var = var, cat = cat,
                                            ent = "Participant", action = "overwrite", id.name = "id", report_path = NULL, comparison = "both")


## Copy
  report_copy = import_copy_table_opal(opal = opal, projname = projname, tablename = tablename,
                                       opal2 = opal, projname2 = projname, tablename2 = paste0(tablename, "_copy"),
                                       report_path = NULL, comparison = "both")
  delete_table_opal(opal = opal, projname = projname, tablename = paste0(tablename, "_copy"), child_lock = FALSE)


## Copy many
  report_copy_many = import_copy_table_opal_many(opal = opal, projnames = projname, tablenames = tablenames,
                                                 opal2 = opal, projnames2 = projname, tablenames2 = paste0(tablenames, "_copy", 1:3),
                                                 report_path = NULL, comparison = "both")
  for(i in 1:length(tablenames)){
    delete_table_opal(opal = opal, projname = projname, paste0(tablenames, "_copy", 1:3)[i], child_lock = FALSE)
  }


## Views
  report_view = make_opal_view(opal = opal, projname = projname, tablename = tablename,
                               opal_view = opal, projname_view = projname, tablename_view = tablename_view,
                               var = var, cat = cat, ent = "Participant", update = FALSE, comparison = "both", report_path = NULL, child_lock = FALSE)

  report_view2 = make_opal_view(opal = opal, projname = projname, tablename = tablename,
                                opal_view = opal, projname_view = projname, tablename_view = tablename_view,
                                var = var, cat = cat, ent = "Participant", update = TRUE, comparison = "both", report_path = NULL, child_lock = FALSE)

  report_view3 = make_opal_view(opal = opal, projname = projname, tablename = tablename,
                                opal_view = opal, projname_view = projname, tablename_view = tablename_view, EntityFilter = "$this('group').eq('A')",
                                var = var, cat = cat, ent = "Participant", update = TRUE, comparison = "both", report_path = NULL, child_lock = FALSE)

### What happens when the view is deprecated? So original table is reduced
  report_create4 = import_create_table_opal(opal = opal, projname = projname, tablename = tablename, child_lock = FALSE,
                                            datafile = datafile |> select(-integer4),
                                            var = var |> filter(name != "integer4"),
                                            cat = cat |> filter(variable != "integer4"),
                                            ent = "Participant", action = "overwrite", id.name = "id", report_path = NULL, comparison = "both")

  report_view4 = make_opal_view(opal = opal, projname = projname, tablename = tablename,
                                opal_view = opal, projname_view = projname, tablename_view = tablename_view,
                                var = var, cat = cat, ent = "Participant", update = TRUE, comparison = "both", report_path = NULL, child_lock = FALSE)


  delete_table_opal(opal = opal, projname = projname, tablename = tablename, child_lock = FALSE)
  delete_table_opal(opal = opal, projname = projname, tablename = tablename_view, child_lock = FALSE)



## Encryption -------------------------------------------------------------
  if(isTRUE(encryption)){
    con <- tres_connect(base_url = "https://mereden.msbi.nl/Tres", domain = "dw.clinicalresearch.nl", project = "Lumina", username = "ADMs2s", password = keyring::key_get("TRES_opals2s_lumina"),
                        search_image = TRUE)
    vars_to_encrypt = var |> filter(encrypted == "yes") |> pull(name)
    vars_to_encrypt_SI = var |> filter(encrypted == "SI") |> pull(name)

    encryption_checks1 = capture.output(datafile_encr <<- datafile |> mutate(across(all_of(vars_to_encrypt), ~encrypt_data(con, .x, search_image = FALSE))))
    decryption_checks1 = capture.output(datafile_decr <<- datafile_encr |> mutate(across(all_of(vars_to_encrypt), ~decrypt_data(con, .x))))

    report_diffdf8 = check_diffdf_opal_generic(datafile, datafile_decr, NULL, NULL, NULL, NULL, comparison = "both", comp_key = "id", suppress_warnings = TRUE,
                                               report_path = NULL, opt_rm_VarDiff_null = TRUE, opt_calc_VarDiff_diff = TRUE, opt_calc_VarDiff_spaces = TRUE,
                                               opt_rm_VarDiff_diff_0 = TRUE, opt_repl_castor = TRUE, aggregate_VarDiff = TRUE)

    var_encr = var |> mutate(valueType = ifelse(name %in% vars_to_encrypt, "text", valueType))
    write_table_R2opal(opal, projname = projname, tablename = tablename, datafile = datafile_encr, var = var_encr, cat = cat, ent = "Participant", action = "write", child_lock = FALSE)

    report_import4 = import_table_opal2R(opal = opal, projname = projname, tablename = tablename)
    delete_table_opal(opal = opal, projname = projname, tablename = tablename, child_lock = FALSE)

    decryption_checks2 = capture.output(datafile_decr2 <<- report_import4$datafile4copy |> mutate(across(all_of(vars_to_encrypt), ~decrypt_data(con, .x))))

    report_diffdf9 = check_diffdf_opal_generic(datafile, datafile_decr2, NULL, NULL, NULL, NULL, comparison = "both", comp_key = "id", suppress_warnings = TRUE,
                                               report_path = NULL, opt_rm_VarDiff_null = TRUE, opt_calc_VarDiff_diff = TRUE, opt_calc_VarDiff_spaces = TRUE,
                                               opt_rm_VarDiff_diff_0 = TRUE, opt_repl_castor = TRUE, aggregate_VarDiff = TRUE)

    encryption_checks2 = capture.output(datafile_encr2 <<- datafile_encr |> mutate(across(all_of(vars_to_encrypt_SI), ~encrypt_data(con, .x, search_image = TRUE))))
    var_encr = var_encr |> mutate(valueType = ifelse(name %in% vars_to_encrypt_SI, "text", valueType))

    decryption_checks3 = tryCatch({datafile_encr2 |> mutate(across(all_of(vars_to_encrypt_SI), ~decrypt_data(con, .x)))},
                                  error = function(e){e$parent$parent})
  }


## Number_datapoints ------------------------------------------------------
  NDP1 = number_datapoints(datafile, var, cat, count_Missings = FALSE)
  NDP2 = number_datapoints(datafile, var, cat, count_Missings = TRUE)

  NDPs = NDP1 |>
    full_join(NDP2, by = join_by(name, valueType, `label:en`, unit, `description:en`, min, max, encrypted, index)) |>
    select(name, starts_with("Mlstr_area::"))



# Without cat dictionary --------------------------------------------------
  rm(cat)

  output_checks2 = capture.output(report_checks2 <<- checks_opal_R(datafile = datafile, var = var, key = "id", min_max = TRUE, silent = FALSE))

  write_table_R2opal(opal, projname = projname, tablename = tablename, datafile = datafile, var = var, ent = "Participant", action = "write", child_lock = FALSE)

  report_import2 = import_table_opal2R(opal = opal, projname = projname, tablename = tablename)
  datafile2 = report_import2$datafile; var2 = report_import2$var; cat2 = report_import2$cat


  delete_table_opal(opal = opal, projname = projname, tablename = tablename, child_lock = FALSE)



  ## Create
  report_create5 = import_create_table_opal(opal = opal, projname = projname, tablename = tablename, datafile = datafile, var = var,
                                            ent = "Participant", action = "write", id.name = "id", report_path = NULL, comparison = "both")

  report_create6 = import_create_table_opal(opal = opal, projname = projname, tablename = tablename, child_lock = FALSE, datafile = datafile, var = var,
                                            ent = "Participant", action = "update", id.name = "id", report_path = NULL, comparison = "both")

  report_create7 = import_create_table_opal(opal = opal, projname = projname, tablename = tablename, child_lock = FALSE, datafile = datafile, var = var,
                                            ent = "Participant", action = "overwrite", id.name = "id", report_path = NULL, comparison = "both")


  ## Copy
  report_copy2 = import_copy_table_opal(opal = opal, projname = projname, tablename = tablename,
                                        opal2 = opal, projname2 = projname, tablename2 = paste0(tablename, "_copy"),
                                        report_path = NULL, comparison = "both")
  delete_table_opal(opal = opal, projname = projname, tablename = paste0(tablename, "_copy"), child_lock = FALSE)


  ## Copy many
  report_copy_many2 = import_copy_table_opal_many(opal = opal, projnames = projname, tablenames = tablenames,
                                                  opal2 = opal, projnames2 = projname, tablenames2 = paste0(tablenames, "_copy", 1:3),
                                                  report_path = NULL, comparison = "both")
  for(i in 1:length(tablenames)){
    delete_table_opal(opal = opal, projname = projname, paste0(tablenames, "_copy", 1:3)[i], child_lock = FALSE)
  }


  report_view5 = make_opal_view(opal = opal, projname = projname, tablename = tablename,
                                opal_view = opal, projname_view = projname, tablename_view = tablename_view,
                                var = var, ent = "Participant", update = FALSE, comparison = "both", report_path = NULL, child_lock = FALSE)

  report_view6 = make_opal_view(opal = opal, projname = projname, tablename = tablename,
                                opal_view = opal, projname_view = projname, tablename_view = tablename_view,
                                var = var, ent = "Participant", update = TRUE, comparison = "both", report_path = NULL, child_lock = FALSE)


  delete_table_opal(opal = opal, projname = projname, tablename = tablename, child_lock = FALSE)
  delete_table_opal(opal = opal, projname = projname, tablename = tablename_view, child_lock = FALSE)




# Errors ------------------------------------------------------------------
  cat = cat_temp

  datafile[1:2, "id"] = "Sample1"  ## Key not unique
  var$entityType = "Individual"; var$entityType[c(3, 8, 19)] = "Sample"
  var = var |> rename(upperLimit = max)


  output_checks3 = capture.output(report_checks3 <<- checks_opal_R(datafile = datafile, var = var, cat = cat, key = "id", min_max = TRUE, silent = FALSE))


  write_table_R2opal(opal, projname = projname, tablename = tablename, datafile = datafile, var = var, cat = cat, ent = "Participant", action = "write", child_lock = FALSE)


  report_import3 = import_table_opal2R(opal = opal, projname = projname, tablename = tablename)
  datafile4copy = report_import3$datafile4copy; var4copy = report_import3$var4copy; cat4copy = report_import3$cat4copy

  delete_table_opal(opal = opal, projname = projname, tablename = tablename, child_lock = FALSE)


  output_checks4 = capture.output(report_checks4 <<- checks_opal_R(datafile = datafile4copy, var = var4copy, cat = cat4copy, key = "id", min_max = TRUE, silent = FALSE))


  report_diffdf7 = check_diffdf_opal_generic(datafile, datafile4copy, var, var4copy, cat, cat4copy, comparison = "both", comp_key = c("id", "integer1"), suppress_warnings = TRUE,
                                             report_path = NULL, opt_rm_VarDiff_null = TRUE, opt_calc_VarDiff_diff = TRUE, opt_calc_VarDiff_spaces = TRUE)



# Return ------------------------------------------------------------------
  out = list(output_checks = output_checks,
             report_checks = report_checks,
             report_change = report_change,
             report_import1 = report_import1,
             report_diffdf = report_diffdf,
             report_diffdf2 = report_diffdf2,
             report_diffdf3 = report_diffdf3,
             report_diffdf4 = report_diffdf4,
             report_diffdf5 = report_diffdf5,
             report_diffdf6 = report_diffdf6,
             report_create = report_create,
             report_create2 = report_create2,
             report_create3 = report_create3,
             report_copy = report_copy,
             report_copy_many = report_copy_many,
             report_view = report_view,
             report_view2 = report_view2,
             report_view3 = report_view3,
             report_create4 = report_create4,
             report_view4 = report_view4,
             NDPs = NDPs,

             output_checks2 = output_checks2,
             report_checks2 = report_checks2,
             report_import2 = report_import2,
             report_create5 = report_create5,
             report_create6 = report_create6,
             report_create7 = report_create7,
             report_copy2 = report_copy2,
             report_copy_many2 = report_copy_many2,
             report_view5 = report_view5,
             report_view6 = report_view6,

             output_checks3 = output_checks3,
             report_checks3 = report_checks3,
             report_import3 = report_import3,
             output_checks4 = output_checks4,
             report_checks4 = report_checks4,
             report_diffdf7 = report_diffdf7)

  if(isTRUE(encryption)){
    out2 = list(encryption_checks1 = encryption_checks1,
                decryption_checks1 = decryption_checks1,
                report_diffdf8 = report_diffdf8,
                decryption_checks2 = decryption_checks2,
                report_diffdf9 = report_diffdf9,
                encryption_checks2 = encryption_checks2,
                decryption_checks3 = decryption_checks3)

    out = append(out, out2)
  }


  return(out)
}
