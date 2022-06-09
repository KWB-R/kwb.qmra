[![R-CMD-check](https://github.com/KWB-R/kwb.qmra/workflows/R-CMD-check/badge.svg)](https://github.com/KWB-R/kwb.qmra/actions?query=workflow%3AR-CMD-check)
[![pkgdown](https://github.com/KWB-R/kwb.qmra/workflows/pkgdown/badge.svg)](https://github.com/KWB-R/kwb.qmra/actions?query=workflow%3Apkgdown)
[![codecov](https://codecov.io/github/KWB-R/kwb.qmra/branch/master/graphs/badge.svg)](https://codecov.io/github/KWB-R/kwb.qmra)
[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![DOI](https://zenodo.org/badge/68301647.svg)](https://zenodo.org/badge/latestdoi/68301647)
[![Binder](http://mybinder.org/badge.svg)](https://mybinder.org/v2/gh/kwb-r/kwb.qmra/master?urlpath=rstudio)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/kwb.qmra)]()
[![R-Universe_Status_Badge](https://kwb-r.r-universe.dev/badges/kwb.qmra)](https://kwb-r.r-universe.dev/)

An R package for QMRA (quantitative microbial risk assessment) of water supply systems


## Installation

For installing the latest release of this R package run the following code below:

```r
# Enable repository from kwb-r
options(repos = c(
  kwbr = 'https://kwb-r.r-universe.dev',
  CRAN = 'https://cloud.r-project.org'))
  
# Download and install kwb.qmra in R
install.packages('kwb.qmra')

# Browse the kwb.qmra manual pages
help(package = 'kwb.qmra')
```