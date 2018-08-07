#' Triangles from quads
#'
#' Convert quad index to triangles.
#'
#' Triangle pairs from each quad are interleaved in the result, so that neighbour
#' triangles from a single quad are together.
#' @param quad_index the 'ib' index of quads from 'quadmesh'
#' @return matrix of triangle indices
#' @export
#' @examples
#' triangulate_quads(cbind(c(1, 2, 4, 3), c(3, 4, 6, 5)))
#'
#' qm <- quadmesh(raster::crop(worldll, raster::extent(140, 160, -50, -30)))
#' tri <- triangulate_quads(qm$ib)
#' plot(t(qm$vb))
#' ## tri is qm$ib converted to triangles for the same vertex set
#' polygon(t(qm$vb)[rbind(tri, NA), ])
triangulate_quads <- function(quad_index) {
  matrix(rbind(quad_index[c(1L, 2L, 4L), ], quad_index[c(2L, 3L, 4L), ]), 3L)
}






