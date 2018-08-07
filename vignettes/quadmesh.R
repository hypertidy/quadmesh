## ------------------------------------------------------------------------
library(quadmesh)
library(raster)

data(volcano)
r <- setExtent(raster(volcano), extent(0, 100, 0, 200))


qm <- quadmesh(r)

library(rgl)
scl <- function(x) (x - min(x))/diff(range(x))
shade3d(qm, col = grey(scl(qm$vb[3,qm$ib])))

rglwidget()

## ------------------------------------------------------------------------
qm <- quadmesh(r)

qm$vb[1,] <- qm$vb[1,] * qm$vb[2,]/54
open3d()
shade3d(qm, col = grey(scl(qm$vb[3,qm$ib])))

rglwidget()

## ------------------------------------------------------------------------
(m <- matrix(1:12, nrow = 3))

## ------------------------------------------------------------------------
x <- seq(1, nrow(m)) - 0.5
y <- seq(1, ncol(m)) - 0.5
image(x, y, m)

text(expand.grid(x, y), lab = m[])


## ------------------------------------------------------------------------
library(raster)
(r <- raster(t(m[, ncol(m):1]), xmn = 0, xmx =ncol(m), ymn = 0, ymx = nrow(m)))

## ------------------------------------------------------------------------
xyz <- as.data.frame(r, xy = TRUE)
head(xyz)
tail(xyz)

## ------------------------------------------------------------------------
as(as(raster::rasterToPolygons(r), "SpatialLinesDataFrame"), 
   "SpatialPointsDataFrame")


## ------------------------------------------------------------------------
library(quadmesh)
qm <- quadmesh(r)
str(qm)

