#' Triangles from quads
#'
#' Convert quad index to triangles.
#'
#' Triangle pairs from each quad are interleaved in the result, so that neighbour
#' triangles from a single quad are together.
#'
#' @param quad_index
#'
#' @return matrix of triangle indices
#' @export
triangulate_quads <- function(quad_index) {
  matrix(rbind(quad_index[c(1L, 2L, 4L), ], quad_index[c(2L, 3L, 4L), ]), 3L)
}






