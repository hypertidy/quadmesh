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

library(rgl)
library(quadmesh)
qm <- quadmesh(raster::aggregate(etopo, fact = 8)/200)
#qm <- reproj(qm, target = "+proj=laea +lon_0=147 +lat_0=-42 +unit=km")
qm$it <- triangulate_quads(qm$ib, clockwise = FALSE)
qm$ib <- NULL
qm$primitivetype <- "triangle"
rgl::rgl.clear()
rgl::shade3d(addNormals(qm), col = "grey")
rgl::writeSTL("examples/stl/etopo.stl")


library(rgl)
library(quadmesh)
qm <- quadmesh(raster::aggregate(etopo, fact = 8)*50)
qm <- reproj(qm, target = "+proj=geocent +datum=WGS84")
qm$it <- triangulate_quads(qm$ib, clockwise = FALSE)
qm$ib <- NULL
qm$primitivetype <- "triangle"
rgl::rgl.clear()
rgl::shade3d(qm, col = "grey")
rgl::writeSTL("examples/stl/globe_etopo.stl")



#' https://en.wikipedia.org/wiki/Monkey_saddle
monkey <- function(q, scale = TRUE) {
  xy <- q$vb[1:2, ]
  q$vb[3L, ] <- xy[1L, ]^3 - 3 * xy[1L, ] * xy[2L, ]^2

  if (scale) {
    ## just based on x for now
    q$vb[3L, ] <- scales::rescale(q$vb[3L, ], range(xy[1L, ]))
  }
  q
}
hp <- function(q, a = 1, b = 1, scale = TRUE) {
  xy <- q$vb[1:2, ]
  q$vb[3L, ] <- ((xy[1, ]^2)/a) - ((xy[2, ]^2)/b)
  if (scale) {
    ## just based on x for now
    q$vb[3L, ] <- scales::rescale(q$vb[3L, ], range(xy[1L, ]))
  }
 q
}
qm <- quadmesh(raster::raster(matrix(0, 20, 20), xmn = -10, xmx = 10, ymn = -10, ymx = 10))
m <- monkey(qm)
h <- hp(qm, 1, 1)
rgl.clear();shade3d(h, col = "white"); aspect3d(1, 1, 1);rglwidget()
rgl::writeSTL("examples/stl/hyperbolic_paraboloid.stl")
rgl.clear();shade3d(m, col = "white"); aspect3d(1, 1, 1);rglwidget()
rgl::writeSTL("examples/stl/monkey_saddle.stl")

rgl.clear();shade3d(Rvcg::, col = "white"); aspect3d(1, 1, 1);rglwidget()

rgl::writeSTL("examples/stl/humface.stl")

uluru <- raster::raster("../rvs")
