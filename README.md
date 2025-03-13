<!-- spell-check: ignore:start -->
[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)
[![License](http://img.shields.io/badge/license-GPL--3-blue.svg?style=flat)](http://www.gnu.org/licenses/gpl-3.0.html)
[![Release](https://img.shields.io/github/release/inbo/dhcurve.svg)](https://github.com/inbo/dhcurve/releases)
[![R build status](https://github.com/inbo/dhcurve/actions/workflows/check_on_different_r_os.yml/badge.svg)](https://github.com/inbo/dhcurve/actions)
[![codecov](https://codecov.io/gh/inbo/dhcurve/branch/main/graph/badge.svg)](https://codecov.io/gh/inbo/dhcurve)
![r-universe
name](https://inbo.r-universe.dev/badges/:name?color=c04384)
[![r-universe package](https://inbo.r-universe.dev/badges/dhcurve)](https://inbo.r-universe.dev/dhcurve)
![GitHub code size in
bytes](https://img.shields.io/github/languages/code-size/inbo/dhcurve.svg)
![GitHub repo
size](https://img.shields.io/github/repo-size/inbo/dhcurve.svg)
<!-- badges: end -->

<a href="https://www.natuurenbos.be/">
<img src="man/figures/logo.png" align="right" alt="logo of ANB: Agentschap Natuur & Bos" width="200"/>
</a>
<!-- spell-check: ignore:end -->

# Automated Modelling of Diameter Height Curves for Trees

The `dhcurve` package bundles a number of functions that allow to model 'Diameter Height Curves' starting from a dataset of tree measures (tree species, location, perimeter and height).  The final result is a dataset that provides an estimated height for each combination of tree species, location and perimeter, or a dataset that provides for each combination of tree species and location the parameters A, B and C of the model curve $Height = A + B\log(Perimeter)+C\log(Perimeter)^2$.

The package is written in Dutch.

An English poster on this package, presented at the International Statistical Ecology Conference in the University of St. Andrews in 2018, is available [here](https://www.vlaanderen.be/inbo/en-GB/publications/estimating-tree-height-for-a-given-girth).

## Installation

To install `dhcurve` from the [INBO universe](https://inbo.r-universe.dev/ui#builds),
start a new R session and run this code (before loading any packages):

```r
# Enable the INBO universe (not needed for INBO employees, as this is the default setting)
options(
  repos = c(
    inbo = "https://inbo.r-universe.dev", CRAN = "https://cloud.r-project.org"
  )
)
# Install the packages
install.packages("dhcurve")
```

To install `dhcurve` from GitHub, start a new R session and run this code (before loading any packages):

```r
#install.packages("remotes")
remotes::install_github("inbo/dhcurve")
```

<!-- spell-check: ignore:start -->
<a href="https://www.natuurenbos.be/">
<img src="man/figures/name.png" alt="logo of ANB: Vlaanderen is natuur" width="200"/>
</a>
<!-- spell-check: ignore:end -->
