# OpalLUMCADM

R package to interact between opal and R.

## Installation
1. Install the "remotes" & "git2r" package (if not already installed):
  ```R
install.packages("remotes")
install.packages("git2r")
```
2. Install the "OpalLUMCADM" package:
  ```R
## Install package
remotes::install_git(
  url = "https://git.lumc.nl/research-it/OpalLUMCADM.git",
  credentials = git2r::cred_user_pass("USERNAME", "GIT_ACCESS_TOKEN")
)

## Specific version
remotes::install_git(
  url = "https://git.lumc.nl/research-it/OpalLUMCADM.git",
  ref = "v1.0.0", ## Or other tag
  credentials = git2r::cred_user_pass("USERNAME", "GIT_ACCESS_TOKEN")
)

## Load package
library(OpalLUMCADM)
```

## Usage
Check `_Function_template.Rmd` in the example folder
