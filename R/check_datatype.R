
#' Function to check valuetypes of data vs variables
#'
#' @param datafile a working opalr::opal_login
#' @param variables Origin opal project name
#'
#' @import opalr
#' 
#' @export

## Retrieved from datafile_conform_var_change()
adm.check_valuetype <- function(datafile, variables, ...) {
  ## Type translation
  translation_opal <- c(
    integer = "integer",
    decimal = "numeric",
    text = "character",
    boolean = "logical",
    datetime = "POSIXct",
    date = "POSIXct"
  )

  ## Get valuetypes
  valuetypes_data <- sapply(datafile, function(x) tail(class(x), n = 1))
  valuetypes_vars <- setNames(translation_opal[variables$valueType], variables$name)

  ## Compare valuetypes
  for (column in names(valuetypes_vars)) {
    compare <- identical(valuetypes_vars[column], valuetypes_data[column])
    if (!compare) {
      warning(
        "ValueType of ", column, " doesn't match: ",
        valuetypes_vars[column], " vs ", valuetypes_data[column]
      )
    }
  }
  message("All valueTypes checked")
}
