library(quadmesh)
library(raster)
g <- raster::raster(system.file("extdata/gebco1.tif", package = "anglr"))


qm <- quadmesh(g)
qm <- quadmesh(aggregate(g/100, fact = 10))

library(rgl)
rgl.clear()
shade3d(qm)
rglwidget()

tm <- qm
tm$primitivetype <- "triangle"
tm$it <- triangulate_quads(qm$ib)
tm$ib <- NULL
rgl.clear()
shade3d(tm)
rglwidget()
