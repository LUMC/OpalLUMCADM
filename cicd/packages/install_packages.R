
## Install required packages
install.packages("opalr")
install.packages("dplyr")
install.packages("rlang")
install.packages("diffdf")
install.packages("openxlsx")
install.packages("stringr")
install.packages("lubridate")
install.packages("remotes")

## Need to install local, due to API rate limits in GitHub
remotes::install_local("./cicd/packages/rtres-2.0.0.9000.zip")
