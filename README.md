
<!-- README.md is generated from README.Rmd. Please edit that file -->

# quadmesh <img src="man/figures/logo.png" align="right" />

[![Travis-CI Build
Status](https://travis-ci.org/hypertidy/quadmesh.svg?branch=master)](https://travis-ci.org/hypertidy/quadmesh)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/hypertidy/quadmesh?branch=master&svg=true)](https://ci.appveyor.com/project/hypertidy/quadmesh)
[![Coverage
status](https://codecov.io/gh/hypertidy/quadmesh/branch/master/graph/badge.svg)](https://codecov.io/github/hypertidy/quadmesh?branch=master)
[![CRAN
status](https://www.r-pkg.org/badges/version/quadmesh)](https://cran.r-project.org/package=quadmesh)[![CRAN\_Download\_Badge](http://cranlogs.r-pkg.org/badges/quadmesh)](https://cran.r-project.org/package=quadmesh)

A *quadmesh* is a dense mesh describing a topologically continuous
surface of 4-corner primitives. This is also known as a *cell-based
raster* but in those contexts the corner coordinates and the continuous
nature of the mesh is completely implicit. By making the *dense mesh*
explicit we have access to every corner coordinate (not just the
centres) which allows for some extra facilities over raster grids.

This package provides helpers for working with this mesh interpretation
of gridded data to enable

  - arbitrary reprojection of raster cells without information loss
    (`mesh_plot)`.
  - the corner-based interpretation of a grid (`quadmesh()`).
  - easy plotting of grids in 3D visualization tools (`quad primitives`
    in `rgl mesh3d`).
  - fast polygonization of individual cells, in
    [spex::polygonize](https://CRAN.R-project.org/package=spex).

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
