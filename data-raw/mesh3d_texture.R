
library(raster)
etopo_aus <- crop(etopo, projectExtent(mapbox_sat, projection(etopo)))
etopo_aus[etopo_aus < -1000] <- -1000

qm <- quadmesh(etopo_aus, texture = mapbox_sat)
library(rgl)
rgl.clear(); shade3d(qm); aspect3d(1, 1, 0.1) #; rglwidget()

