library(raster)
r <- raster(matrix(c(1:4,10, 5:4, 9, 4:1), 3, 4))

## AREA interpretation
a <- quadmesh::quadmesh(r)
a$it <- quadmesh:::triangulate_quads(a$ib)


## POINT interpretation
ex <- extent(r)
ex <- c(xmin(ex), xmax(ex), ymin(ex), ymax(ex)) + c(-1, 1, -1, 1)/2 * c(res(r)[c(1, 1, 2, 2)])
b <- quadmesh::quadmesh(crop(r, extent(r, 1, nrow(r) - 1, 1, ncol(r) - 1)), z = NULL)
b$vb[1:2, ] <- t(coordinates(r))
b$vb[3, ] <- extract(r, t(b$vb[1:2, ]))
b$it <- quadmesh:::triangulate_quads(b$ib)


plot(r)
## AREA interpretation
bind1 <- function(x) rbind(x, x[1, ])
apply(bind1(a$ib), 2, function(x) lines(t(a$vb[1:2, x]), lwd = 7))

## POINT interpretation
points(t(b$vb))
## POINT as triangles
apply(bind1(b$it), 2, function(x) lines(t(b$vb[1:2, x])))

#' @examples
#' rgl::rgl.clear()
#' r <- raster::raster(volcano)
#' tm <- triangle_mesh(r)
#' rgl::shade3d(tm, col = viridis::viridis(25)[scales::rescale(tm$vb[3, tm$it], c(0, 1))*24 + 1])
#' rgl::rglwidget()
triangle_mesh <- function(x) {
  ## POINT interpretation
  ex <- raster::extent(x)
  ex <- c(raster::xmin(ex), raster::xmax(ex), raster::ymin(ex), raster::ymax(ex)) + c(-1, 1, -1, 1)/2 * c(raster::res(x)[c(1, 1, 2, 2)])
  b <- quadmesh::quadmesh(raster::crop(x, raster::extent(x, 1, raster::nrow(x) - 1, 1, raster::ncol(x) - 1)), z = NULL)
  b$vb[1:2, ] <- t(coordinates(x))
  b$vb[3, ] <- values(x)
  b$it <- quadmesh:::triangulate_quads(b$ib)
  delete_quads(b)
}

#' Drop quad or triangle index
#'
#' @param x
#'
#' @return
#' @export
#' @aliases delete_triangles
#' @examples
delete_quads <- function(x) {
  x$ib <- NULL
  x
}
#'
delete_triangles <- function(x) {
  x$it <- NULL
  x
}
#' unmeshed quads.
#'
#' Quads as discrete tiles, from a raster.
#'
#' @param x a Raster
#'
#' @return mesh3d
#' @export
#'
#' @examples
#' rgl::rgl.clear()
#' r <- raster::raster(volcano)
#' qu <- quad_unmeshed(r)
#' rgl::shade3d(qu, col = viridis::viridis(25)[scales::rescale(qu$vb[3, ], c(0, 1))*24 + 1][qu$ib])
#' rgl::rglwidget()
quad_unmeshed <- function(x) {
  z <- values(x)
  x <- quadmesh::quadmesh(x, z = NULL)

  vb <- ib <- NULL
  for (i in 1:ncol(x$ib)) {
    quadv <- x$vb[, x$ib[,i]]
    quadv[3, ] <- z[i]
    vb <- cbind(vb, quadv)
    ib <- cbind(ib, 1:4 + (i-1) * 4)
  }
  x$ib <- ib
  x$vb <- vb
  x
}
library(rgl)

rgl.clear()
material3d(specular = "black")
bg3d("darkgrey")

## unmeshed keeps a constant value on the quad
shade3d(quad_unmeshed(r), col = "grey")
## b was built as literally the triangulation on the centre coordinates (no interpolation)
shade3d(delete_triangles(b))
aspect3d(1, 1, 0.5)
rglwidget()


