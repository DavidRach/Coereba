# Welcome to `Coereba` <img src="inst/hex/hex.png" width="200" align="right"/>

<!-- To modify Package/Title/Description/Authors fields, edit the DESCRIPTION file -->
<!-- badges: start -->

[![R build
status](https://github.com/DavidRach/Coereba/workflows/rworkflows/badge.svg)](https://github.com/DavidRach/Coereba/actions)
[![License: AGPL (\>=
3)](https://img.shields.io/badge/license-AGPL%20(%3E=%203)-blue.svg)](https://cran.r-project.org/web/licenses/AGPL%20(%3E=%203))
[![](https://img.shields.io/badge/devel%20version-0.1.0-black.svg)](https://github.com/DavidRach/Coereba)
[![](https://img.shields.io/github/languages/code-size/DavidRach/Coereba.svg)](https://github.com/DavidRach/Coereba)
[![](https://img.shields.io/github/last-commit/DavidRach/Coereba.svg)](https://github.com/DavidRach/Coereba/commits/master)
[![codecov](https://codecov.io/gh/DavidRach/Coereba/graph/badge.svg?token=1SRXI13GOE)](https://codecov.io/gh/DavidRach/Coereba)
<br> <!-- badges: end -->

## `Coereba`: Dichotomized Clustering for Spectral Flow Cytometry

Coereba is a collection of tools allowing researchers to extend their understanding of their panels and datasets. 


### Installation

We are in the process of getting Coereba ready to submit to Bioconductor. Until then, please download the package from github. 

``` r
if(!require("remotes")) install.packages("remotes")

remotes::install_github("https://github.com/DavidRach/Coereba")

library(Coereba)

# install.packages("BiocManager")
# BiocManager::install("Coereba")
```

### Get Started

Please check out our how-to [vignettes](https://davidrach.github.io/Coereba/articles/GettingStarted.html)
to get started


### Found a bug? Report it!

While we have caught a lot of bugs, there's still unknown ones that we haven't encountered. If you find a suspected bug, please report it here [here](https://github.com/DavidRach/Coereba/issues)



