
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Travis-CI Build
Status](https://travis-ci.org/mdsumner/quadmesh.svg?branch=master)](https://travis-ci.org/mdsumner/quadmesh)
[![CRAN
status](https://www.r-pkg.org/badges/version/quadmesh)](https://cran.r-project.org/package=quadmesh)

# quadmesh

A *quadmesh* is a dense mesh describing a topologically continuous
surface of 4-corner primitives. This is also known as a *cell-based
raster* but in those contexts the corner coordinates and the continuous
nature of the mesh is completely implicit.

This package provides helpers for working with mesh interpretation of
gridded data to enable

  - arbitrary reprojection of raster cells without information loss.
  - easy plotting of grids in 3D visualization tools (like `rgl`).
  - the corner-based interpretation of a grid.
  - fast polygonization of individual cells.

You can install:

  - the latest released version from CRAN with

<!-- end list -->

``` r
install.packages("quadmesh")
```

  - the latest development version from Github with

<!-- end list -->

``` r
devtools::install_github("mdsumner/quadmesh")
```

Please note that this project is released with a [Contributor Code of
Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree
to abide by its terms.
