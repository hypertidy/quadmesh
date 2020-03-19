## -----------------------------------------------------------------------------
library(quadmesh)
library(raster)

data(volcano)
volcano <- volcano[seq(1, nrow(volcano), by = 3), seq(1, ncol(volcano), by = 3)]
r <- setExtent(raster(volcano), extent(0, 100, 0, 200))


qm <- quadmesh(r)

library(rgl)
scl <- function(x) (x - min(x))/diff(range(x))
shade3d(qm, col = grey(scl(qm$vb[3,qm$ib])))

rglwidget()

## -----------------------------------------------------------------------------
qm1 <- quadmesh(r)

qm1$vb[1,] <- qm$vb[1,] * qm$vb[2,]/54
open3d()
shade3d(qm1, col = grey(scl(qm1$vb[3,qm1$ib])))

rglwidget()

## -----------------------------------------------------------------------------
(m <- matrix(1:12, nrow = 3))

## -----------------------------------------------------------------------------
x <- seq(1, nrow(m)) - 0.5
y <- seq(1, ncol(m)) - 0.5
image(x, y, m)

text(expand.grid(x, y), lab = m[])


## -----------------------------------------------------------------------------
library(raster)
(r <- raster(t(m[, ncol(m):1]), xmn = 0, xmx =ncol(m), ymn = 0, ymx = nrow(m)))

## -----------------------------------------------------------------------------
xyz <- as.data.frame(r, xy = TRUE)
head(xyz)
tail(xyz)

## -----------------------------------------------------------------------------
as(as(raster::rasterToPolygons(r), "SpatialLinesDataFrame"), 
   "SpatialPointsDataFrame")


## -----------------------------------------------------------------------------
library(quadmesh)
qm <- quadmesh(r)
str(qm)

## -----------------------------------------------------------------------------
image(r)
op <- par(xpd = NA)
text(t(qm$vb), lab = 1:20)
par(op)

## -----------------------------------------------------------------------------
qm$ib

## -----------------------------------------------------------------------------
rgl.clear()
library(rgl)
shade3d(qm)
rglwidget()

rgl.clear()
quads3d(t(qm$vb)[qm$ib,], col = rep(c("grey", "black"), each = 4))
aspect3d(1, 1, 1)
rglwidget()


## ----triangles----------------------------------------------------------------
rgl.clear()
wire3d(qm)
tm <- triangmesh(r)
shade3d(tm, col = rep(c("firebrick", "dodgerblue", "grey"), each = 3))
rglwidget()

## ----discrete-quad------------------------------------------------------------
dqm <- dquadmesh(r)
rgl.clear()
shade3d(dqm)
rglwidget()

## ----discrete-triangle--------------------------------------------------------
dtm <- dtriangmesh(r)
dtm$vb[3, ] <- jitter(dtm$vb[3, ], 8)
rgl.clear()
shade3d(dtm, col = rep(c("firebrick", "dodgerblue", "grey"), each = 3))
rglwidget()

