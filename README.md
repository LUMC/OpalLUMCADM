# OpalLUMCADM
R package to interact between Opal and R.

## Installation
1. Install the "remotes" & "git2r" package (if not already installed):
```R
install.packages("remotes")
install.packages("git2r")
```

2. Install the "Rtres" package:
```R
## Install package
remotes::install_github("ZorgTTP/rtres")
```

3. Install the "OpalLUMCADM" package:
```R
## Install package
remotes::install_git(
  url = "https://git.lumc.nl/adm/r-packages/OpalLUMCADM.git",
  credentials = git2r::cred_user_pass("USERNAME", "GIT_ACCESS_TOKEN")
)

## Specific version
remotes::install_git(
  url = "https://git.lumc.nl/adm/r-packages/OpalLUMCADM.git",
  ref = "v2.0.1", ## Or other tag
  credentials = git2r::cred_user_pass("USERNAME", "GIT_ACCESS_TOKEN")
)

## Load package
library(OpalLUMCADM)
```

## Usage
Check `example.Rmd` in the example folder

