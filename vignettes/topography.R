## -----------------------------------------------------------------------------
library(quadmesh)
library(rgl)
scl <- function(x) (x - min(x))/diff(range(x))
data(etopo)
## very low resolution, simply to keep vignette size down
etopo <- raster::setExtent(raster::aggregate(etopo, fact = 10), raster::extent(etopo))
qmtopo <- quadmesh(etopo, etopo)
open3d()
shade3d(qmtopo, col = grey(scl(qmtopo$vb[3,qmtopo$ib])), asp = c(1, 1, 0.0001))

aspect3d(1, 1, 0.1)


rglwidget()

## -----------------------------------------------------------------------------

qmtopo$vb[1:2, ] <- t(reproj::reproj(t(qmtopo$vb[1:2, ]), "+proj=laea +datum=WGS84 +lat_0=-90", source = "+proj=longlat +datum=WGS84")[,1:2, drop = FALSE])
open3d()
shade3d(qmtopo, col = grey(scl(qmtopo$vb[3,qmtopo$ib])))
aspect3d(1, 1, .1)

rglwidget()

lltopo <- quadmesh(etopo, etopo)
lltopo$vb[1:3, ] <- t(llh2xyz(t(lltopo$vb[1:3, ]), exag = 50))
shade3d(lltopo, col = grey(scl(qmtopo$vb[3,lltopo$ib])))
rglwidget()

## ----eval=FALSE, include=TRUE-------------------------------------------------
#  data(etopo)
#  lt <- raster::crop(etopo, raster::extent(100, 168, -58, -40))
#  ltt <- ltt0 <- quadmesh(lt, lt)
#  ltt$vb[1:3, ] <- t(llh2xyz(t(ltt$vb[1:3, ])))
#  open3d()
#  shade3d(ltt, col = grey(scl(ltt0$vb[3,ltt$ib])))
#  rglwidget()

## -----------------------------------------------------------------------------
library(raster)
plot(etopo, col = palr::bathy_deep_pal(20))
prj <- "+proj=stere +lat_ts=-71 +lat_0=-90 +lon_0=147 +datum=WGS84"
mesh_plot(etopo, crs = prj, asp = 1, col = palr::bathy_deep_pal(20))
xy <- proj4::project(xymap, prj)
lines(xy)



