
library(raster)
library(quadmesh)
etopo_aus <- crop(etopo, projectExtent(mapbox_sat, projection(etopo)))
etopo_aus[etopo_aus < -1000] <- -1000

qm <- quadmesh(etopo_aus, texture = mapbox_sat)
library(rgl)
white <- "#FFFFFF"
qm$material$col <- rep(white, length(qm$ib) * 4)
## code is absolutely correct, the problem is a bug in rgl reported 2018-11-12
rgl.clear(); shade3d(qm, col = "white"); aspect3d(1, 1, 0.1) ; rglwidget()
rgl.clear(); shade3d(qm, texcoords = t(qm$texcoords)[qm$ib, ], texture = qm$texture); aspect3d(1, 1, 0.1) ; rglwidget()


#
#
#
# qm <- quadmesh(etopo_aus)
# texture <- tempfile(fileext = ".png")
# rgdal::writeGDAL(as(mapbox_sat, "SpatialGridDataFrame"), texture, driver = "PNG")
#
#
# texcoords <- xyFromCell(setExtent(mapbox_sat, extent(0, 1, 0, 1)),
#                         cellFromXY(mapbox_sat, rgdal::project(t(qm$vb[1:2, ]), projection(mapbox_sat))))
#
# library(rgl)
# rgl.clear()
# ## qm <- addNormals(qm)  ## consider doing this
# shade3d(qm, texture = texture, texcoords = texcoords[qm$ib, ], col = "white")
# aspect3d(1, 1, .2)
