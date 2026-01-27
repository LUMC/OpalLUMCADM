
#' Load the OpalLUMCADM Package
#'
#' This function is called when the OpalLUMCADM package is loaded into R.
#' It displays a ASCII art logo upon package startup.
#' 
#' @param libname Character string indicating the library path where the package is loaded.
#' @param pkgname Character string indicating the name of the package.
#' 
#' @return Nothing is returned; this is a startup function.
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
