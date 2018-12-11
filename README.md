
<!-- README.md is generated from README.Rmd. Please edit that file -->

# quadmesh <img src="man/figures/logo.png" align="right" />

[![Travis-CI Build
Status](https://travis-ci.org/hypertidy/quadmesh.svg?branch=master)](https://travis-ci.org/hypertidy/quadmesh)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/mdsumner/quadmesh-x25a2?branch=master&svg=true)](https://ci.appveyor.com/project/mdsumner/quadmesh-x25a2)
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
    (`mesh_plot`).
  - the corner-based interpretation of a grid (`quadmesh()`).
  - the centre-based interpretation of a grid (`triangmesh()`).
  - easy plotting of grids in 3D visualization tools (quad or triangle
    primitives for `rgl::shade3d`).
  - fast polygonization of individual cells, in
    [spex::polygonize](https://CRAN.R-project.org/package=spex).
  - barycentric interpolation from a triangle mesh (`bary_index()`).
  - conversion from quad to triangle primitives (`triangulate_quads()`).

You can install:

  - the latest released version from CRAN with

<!-- end list -->

``` r
install.packages("quadmesh")
```

  - the latest development version from Github with

<!-- end list -->

``` r
devtools::install_github("hypertidy/quadmesh")
```

Many aspects of this package have developed in conjunction with the
[angstroms
package](https://github.com/AustralianAntarcticDivision/angstroms) for
dealing with ROMS model output.

Please note that this project is released with a [Contributor Code of
Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree
to abide by its terms.
