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

## `Coereba`: Dichotomized Boolean Clustering and Heatmaps for Spectral Flow Cytometry

Traditionally, supervised (manual) analysis involves drawing gates, drilling down with subsequent gates filtering for a desired cell
population of interest. Combined with expert knowledge and few markers, it allows handling of batch effects. Unsupervised approaches 
have become more frequent employed with increased markers employed by both Spectral Flow Cytometry (SFC) and mass cytometry. Their limitation
has been in addressing what is a cluster and how many of them should be present for a given panel. 

Coereba is a collection of tools that attempt to allow the researcher to better understand their panels and datasets,
employing a semi-supervised approach between the two approaches. 

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

**Co-authors please note** 2-27-2025: Vignette rewrite is in progress after updates for Coereba from using individual data.frame intermediates to a single Summarized Experiment object. Please check the help files for updated arguments (vignette updating scheduled for this weekend)

Please check out our how-to [vignettes](https://davidrach.github.io/Coereba/articles/GettingStarted.html)
to get started


### Found a bug? Report it!

While we have caught a lot of bugs, there's still unknown ones that we haven't encountered. If you find a suspected bug, please report it here [here](https://github.com/DavidRach/Coereba/issues)



