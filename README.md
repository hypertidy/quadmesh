
<!-- README.md is generated from README.Rmd. Please edit that file -->
quadmesh
========

Build a quadmesh in R.

``` r
library(quadmesh)
library(raster)
data(volcano)
r <- setExtent(raster(volcano), extent(0, 100, 0, 200))


qm <- quadmesh(r)

library(rgl)
scl <- function(x) (x - min(x))/diff(range(x))
shade3d(qm, col = grey(scl(qm$vb[3,qm$ib])))
```
