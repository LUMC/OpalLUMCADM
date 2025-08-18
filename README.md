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
  url = "https://git.lumc.nl/adm/r-packages/OpalLUMCADM.git",
  credentials = git2r::cred_user_pass("USERNAME", "GIT_ACCESS_TOKEN")
)

## Specific version
remotes::install_git(
  url = "https://git.lumc.nl/adm/r-packages/OpalLUMCADM.git",
  ref = "v1.0.3", ## Or other tag
  credentials = git2r::cred_user_pass("USERNAME", "GIT_ACCESS_TOKEN")
)

## Load package
library(OpalLUMCADM)
```

## Usage
Check `_Function_template.Rmd` in the example folder

## New in this version
Newly implemented things:
- Allow for EntityFilters in make_opal_view
- Correctly counting the warnings in encrypt_data and decrypt_data
- Included the checks "Inf values", "duplicated var columns" and "duplicated cat columns" in check_opal_R. Also discarded "more than four decimals" which became redundant with Opal5
- If cannot download table with import_table_opal2R will terminate function and return NULL (so is empty)
