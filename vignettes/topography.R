## -----------------------------------------------------------------------------
library(quadmesh)
scl <- function(x) (x - min(x))/diff(range(x))
data(etopo)
## very low resolution, simply to keep vignette size down
etopo <- raster::setExtent(raster::aggregate(etopo, fact = 10), raster::extent(etopo))
qmtopo <- quadmesh(etopo, etopo)

## ----topo1--------------------------------------------------------------------
lltopo <- quadmesh(etopo, etopo)
lltopo$vb[1:3, ] <- t(llh2xyz(t(lltopo$vb[1:3, ]), exag = 50))

## ----eval=FALSE, include=TRUE-------------------------------------------------
#  data(etopo)
#  lt <- raster::crop(etopo, raster::extent(100, 168, -58, -40))
#  ltt <- ltt0 <- quadmesh(lt, lt)
#  ltt$vb[1:3, ] <- t(llh2xyz(t(ltt$vb[1:3, ])))

## -----------------------------------------------------------------------------
library(raster)
plot(etopo, col = palr::bathy_deep_pal(20))
prj <- "+proj=stere +lat_ts=-71 +lat_0=-90 +lon_0=147 +datum=WGS84"
mesh_plot(etopo, crs = prj, asp = 1, col = palr::bathy_deep_pal(20))
xy <- reproj::reproj(xymap, prj, source = 4326)
lines(xy)


