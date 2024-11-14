#' @title Function to count number of datapoints for every column
#'
#' @description
#'
#' @param datafile
#' @param var
#' @param cat
#' @param count_Missings Boolean. Whether or not to to count the missings indicated in the cat dictionary as a datapoint (FALSE is default).
#'
#' @return
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
