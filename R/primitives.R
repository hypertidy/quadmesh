#' Triangles from quads
#'
#' Convert quad index to triangles.
#'
#' Triangle pairs from each quad are interleaved in the result, so that neighbour
#' triangles from a single quad are together.
#'
#' @param quad_index
#'
#' @return
#' @export
#'
#' @examples
#' ## quads to triangles
#' # d <- raster::raster(matrix(1:12, 3, 4))
#' # r <- quadmesh::quadmesh(d, d*0)
#' # library(raster)
#' # plot(d)
#' # p <- spex::spex()
#' # plot(raster::extent(p) + .5)
#' #text(t(r$vb)[r$ib[,1], ], lab = r$ib[,1])
#' #t(r$ib)
#' ## rbind and then reshape to interleave quad pairs together
#' # r$it <- triangulate_quads(r$ib)
#' #library(rgl)
#' #rgl.clear()
#' #rgl.triangles(t(r$vb[1:3, r$it]), col = rep(c("black", "grey", "dodgerblue", "firebrick", "hotpink"), each = 3), specular = "black")
#' #rglwidget()
#' #'  library(quadmesh)
#' #library(raster)
#' #g <- raster::raster(system.file("extdata/gebco1.tif", package = "anglr"))
#' #
#' #
#' #qm <- quadmesh(g)
#' #qm <- quadmesh(aggregate(g/100, fact = 10))
#' #
#' #library(rgl)
#' #rgl.clear()
#' #shade3d(qm)
#' #rglwidget()
#' #
#' #tm <- qm
#' #tm$primitivetype <- "triangle"
#' #tm$it <- triangulate_quads(qm$ib)
#' #tm$ib <- NULL
#' #rgl.clear()
#' #shade3d(tm)
#' #rglwidget()
triangulate_quads <- function(quad_index) {
  matrix(rbind(quad_index[c(1L, 2L, 4L), ], quad_index[c(2L, 3L, 4L), ]), 3L)
}






