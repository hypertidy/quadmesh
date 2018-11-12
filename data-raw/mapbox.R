library(ceramic)
mapbox_sat  <- ceramic:::cc_location(cbind(137, -32), buffer = c(1e6, 5e5), type = "mapbox.satellite")
projection(mapbox_sat) <- "+proj=merc +a=6378137 +b=6378137"

usethis::use_data(mapbox_sat)


#
#
#
library(raster)
 etopo_aus <- crop(etopo, projectExtent(mapbox_sat, projection(etopo)))
 etopo_aus[etopo_aus < -1000] <- -1000
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
