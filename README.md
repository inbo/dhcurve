[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)
[![License](http://img.shields.io/badge/license-GPL--3-blue.svg?style=flat)](http://www.gnu.org/licenses/gpl-3.0.html)
[![wercker status](https://app.wercker.com/status/99290c273f1542a89027cab7f625bee0/s/master "wercker status")](https://app.wercker.com/project/byKey/99290c273f1542a89027cab7f625bee0)
[![codecov](https://codecov.io/gh/inbo/dhcurve/branch/master/graph/badge.svg)](https://codecov.io/gh/inbo/dhcurve)

<img src="man/figures/logo.png" align="right" alt="logo of ANB" width="200"/>
# Automated Modelling of Diameter Height Curves for Trees

The `dhcurve` package bundles a number of functions that allow to model 'Diameter Height Curves' starting from a dataset of tree measures (tree species, location, perimeter and height).  The final result is a dataset that provides an estimated height for each combination of tree species, location and perimeter, or a dataset that provides for each combination of tree species and location the paramaters A, B and C of the model curve $Height = A + B\log(Perimeter)+C\log(Perimeter)^2$.

The package is written in Dutch.
