#' @title Function to count number of datapoints for every column
#'
#' @description For MICA we can indicate the number of actual datapoints for every column in the dataset, can be calculated with this function. Whether or not values that in the cat dictionary are indicated as missing in this calculation can be indicated
#'
#' @param datafile tibble. Containing the datafile with for each column the variables described by \code{var}. The datafile that will be supplied to \code{opalr::dictionary.apply()}.
#' @param var tibble. Containing the variable dictionary for datafile. The dictionary that will be supplied to \code{opalr::dictionary.apply()}.
#' @param cat tibble. Containing the category dictionary for datafile. The dictionary that will be supplied to \code{opalr::dictionary.apply()}.
#' @param count_Missings Boolean. Whether or not to count the missings indicated in the cat dictionary as a datapoint (FALSE is default).
#'
#' @return vector called `Mlstr_area::nr_data_points` (name required by MICA) that indicates the number of datapoints per column
#'
#' @import dplyr
#'
#' @author Lars van der burg
#'
#'
number_datapoints = function(datafile, var, cat = NULL, count_Missings = FALSE){

  nr_points = NULL
  for(i in 1:nrow(var)){
    cname = var$name[i]

    if(isFALSE(count_Missings)){
      if(!is.null(cat)){
        cat_i = cat |>
          filter(variable == cname & missing == TRUE) |>
          pull(name)
      } else {
        cat_i = NULL
      }

      nr_points[i] = datafile |>
        filter(!(is.na(!!sym(cname)) | (!!sym(cname) %in% cat_i))) |>
        nrow()


    } else if(isTRUE(count_Missings)){
      nr_points[i] = datafile |>
        filter(!(is.na(!!sym(cname)))) |>
        nrow()
    }
  }

  var$`Mlstr_area::nr_data_points` = nr_points

  return(var)
}
