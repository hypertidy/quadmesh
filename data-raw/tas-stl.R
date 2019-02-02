raster::plot(etopo)
x <- raster::crop(etopo, raster::extent(130, 150, -50, -30))
qm <- reproj::reproj(triangmesh(x), "+proj=laea +lon_0=147 +lat_0=-42 +datum=WGS84")
qm$vb[3, ] <- qm$vb[3, ] * 3
rgl::rgl.clear()
rgl::shade3d(qm, col = "grey")
rgl::writeSTL("examples/stl/tas_etopo.stl")
library(rgl)
open3d()
shade3d( translate3d( tetrahedron3d(col = "red"), 0, 0, 0) )
shade3d( translate3d( cube3d(col = "green"), 3, 0, 0) )
shade3d( translate3d( octahedron3d(col = "blue"), 6, 0, 0) )
shade3d( translate3d( dodecahedron3d(col = "cyan"), 9, 0, 0) )
shade3d( translate3d( icosahedron3d(col = "magenta"), 12, 0, 0) )


rgl::writeSTL("examples/stl/shapes.stl")


qm <- quadmesh(raster::aggregate(etopo, fact = 8))
qm <- reproj(qm, target = "+proj=geocent +a=637812")
qm$it <- triangulate_quads(qm$ib, clockwise = TRUE)
qm$ib <- NULL
qm$primitivetype <- "triangle"
rgl::rgl.clear()
rgl::shade3d(qm, col = "grey")
rgl::writeSTL("examples/stl/globe_etopo.stl")
