
#' Loading message package OpalLUMCADM
#'
#' @export

.onLoad <- function(libname, pkgname){
  ## https://patorjk.com/software/taag/#p=display&f=Big&t=OpalLUMCADM
  packageStartupMessage(
    paste0(
      "
   ____              _ _     _    _ __  __  _____          _____  __  __
  / __ \\            | | |   | |  | |  \\/  |/ ____|   /\\   |  __ \\|  \\/  |
 | |  | |____   ____| | |   | |  | | \\  / | |       /  \\  | |  | | \\  / |
 | |  | |  _ \\ / _  | | |   | |  | | |\\/| | |      / /\\ \\ | |  | | |\\/| |
 | |__| | |_) | (_| | | |___| |__| | |  | | |____ / ____ \\| |__| | |  | |
  \\____/|  __/ \\____|_|______\\____/|_|  |_|\\_____/_/    \\_\\_____/|_|  |_|
        | |
        |_|
      "
    )
  )
}
