
<!-- README.md is generated from README.Rmd. Please edit that file -->

# chemviewR

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/chemviewR)](https://CRAN.R-project.org/package=chemviewR)
<!-- badges: end -->

The goal of chemviewR is to visualise chemical structures.

## Installation

You can install the development version of chemviewR like so:

``` r
if (!require("remotes"))
    install.packages("remotes")

remotes::install_github("selkamand/chemviewR")
```

## Quick Start

``` r
library(chemviewR)
library(structures)
library(rgl)
#> Warning: package 'rgl' was built under R version 4.4.1
setupKnitr()

# Read Mol file
benzene_file = system.file(package = "chemviewR", "benzene.mol2")
benzene = structures::read_mol2(benzene_file)


plot_molecule(benzene, show_anchor = TRUE)
rgl::rglwidget()
#> Error in with_random_port(launch_chrome_impl, path = path, args = args) : 
#>   Cannot find an available port. Please try again.
#> Caused by error in `startup()`:
#> ! Failed to start chrome. Error:
#> Old Headless mode has been removed from the Chrome binary. Please use the new Headless mode (https://developer.chrome.com/docs/chromium/new-headless) or the chrome-headless-shell which is a standalone implementation of the old Headless mode (https://developer.chrome.com/blog/chrome-headless-shell).
#> Warning in snapshot3d(scene = x, width = width, height = height):
#> webshot2::webshot() failed; trying rgl.snapshot()
```

<img src="../../../../private/var/folders/d9/x2yygv_13_15dw5f8fspdn880000gp/T/Rtmpqwn6sF/file14c2367813918.png" width="100%" />
