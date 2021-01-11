## -----------------------------------------------------------------------------
library(quadmesh)
library(raster)

data(volcano)
volcano <- volcano[seq(1, nrow(volcano), by = 3), seq(1, ncol(volcano), by = 3)]
r <- setExtent(raster(volcano), extent(0, 100, 0, 200))


qm <- quadmesh(r)


## -----------------------------------------------------------------------------
qm1 <- quadmesh(r)

qm1$vb[1,] <- qm$vb[1,] * qm$vb[2,]/54

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

## ----discrete-triangle--------------------------------------------------------
dtm <- dtriangmesh(r)
dtm$vb[3, ] <- jitter(dtm$vb[3, ], 8)


