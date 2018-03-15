# kwb.qmra
An R package for QMRA (quantitative microbial risk assessment) of water supply systems

[![Build Status](https://travis-ci.org/KWB-R/kwb.qmra.svg?branch=master)](https://travis-ci.org/KWB-R/kwb.qmra)

Cite as: [![DOI](https://zenodo.org/badge/68301647.svg)](https://zenodo.org/badge/latestdoi/68301647)

**Launch repository (from master) in cloud RStudio session:** [![Binder](http://mybinder.org/badge.svg)](https://mybinder.org/v2/gh/kwb-r/kwb.qmra/master?urlpath=rstudio)

# Tutorial
## 1. Install from GitHub 


```r
if(!require("devtools")) { install.packages("devtools") }
devtools::install_github(repo = "KWB-R/kwb.qmra", 
                         build_vignettes = TRUE,
                         dependencies = TRUE)
```

## 2. Using the package 

### 2.1 Loading the package

```r
library(kwb.qmra)
```

### 2.2 Using the package

For a first impression check out the package  vignettes:

```r
browseVignettes("kwb.qmra")
```

***More details will follow soon!*** 
