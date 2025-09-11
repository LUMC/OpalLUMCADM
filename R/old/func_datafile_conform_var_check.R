#' @title Function to check whether all datafile variable classes are conform the valueType in the dictionary.
#'
#' @description In this function we check which cases are not conform the valueTypes acceptable for opal, and whether the classes of the datafile are conform the specified valueTypes, if you want to adjust them, used \code{datafile_conform_var_check}
#'
#' @param datafile data.frame. Containing the dataset with for each column the variables described by \code{var}. The datafile that will be supplied to \code{opalr::dictionary.apply()}.
#' @param var data.frame.Containing per row the description of each variable. The dictionary that will be supplied to \code{opalr::dictionary.apply()}.
#' @param silent boolean. Whether or not to silence the printing of a short conclusion for each check (FALSE is default).
#'
#' @return A matrix with rows equal to the amount of variables for which there is a data issue and three rows:
#' * name: indicating the variable with the data issue
#' * issue: the data issue
#' * info: information about the data issue
#'
#' @examples
#' var = var |> mutate(valueType = ifelse(valueType == "integer", str_to_title(valueType), valueType))
#' datafile_conform_var_check(datafile, var)
#'
#' @import opalr
#' @import dplyr
#'
#' @author Lars van der Burg
#'
#' @export
datafile_conform_var_check = function(datafile, var, silent = FALSE){


# Checks ------------------------------------------------------------------
  ## Check whether all essential arguments are specified when the function is called
  if(missing(datafile)){stop("Make sure that argument datafile is specified, it's essential")}
  if(missing(var)){stop("Make sure that argument var is specified, it's essential")}


  ## Check if everything is a tibble
  if(isFALSE(is_tibble(datafile))){stop("The datafile is not a tibble, provide datafile, var and cat as tibble.")}
  if(isFALSE(is_tibble(var))){stop("The var dictionary is not a tibble, provide datafile, var and cat as tibble.")}



  problems = NULL
  for(i in 1:nrow(var)){
    var_i = var[i, ]

# valueType check ---------------------------------------------------------
    ## There are only a couple of valueTypes allowed by Opal (https://opaldoc.obiba.org/en/latest/variables-data.html#value-types)
    if(!(var_i$valueType %in% c("integer", "decimal", "text", "binary", "locale", "boolean", "datetime", "date", "point", "linestring", "polygon"))){
      problems = problems |>
        bind_rows(bind_cols(check = "valueType", issue = "opal compatible", info = paste0(var_i$name, ": ", var_i$valueType)))
    }



# Match check -------------------------------------------------------------
    datafile_i_class = class(datafile[[var_i$name]]); nr_datafile_i_class = length(datafile_i_class)
    if(nr_datafile_i_class > 1){
      problems = problems |>
        bind_rows(bind_cols(check = "Data class", issue = "multiple", info = paste0(var_i$name, ": ", paste(datafile_i_class, collapse = "; "))))
    }

    for(j in 1:nr_datafile_i_class){
      datafile_i_class_j = datafile_i_class[j]

      if(isFALSE((datafile_i_class_j == "integer" & var_i$valueType == "integer") |
                 (datafile_i_class_j == "numeric" & var_i$valueType == "decimal") |
                 (datafile_i_class_j == "character" & var_i$valueType %in% c("text", "datetime", "date")))){

        problems = problems |>
          bind_rows(bind_cols(check = "valueType", issue = "conversion", info = paste0(var_i$name, ": ", datafile_i_class_j, "->", var_i$valueType)))
      }
    }
  }

  if(is.null(problems) && isFALSE(silent)){
    cat("No valueType issues\n")
  }


  return(problems)
}
