#' Reprojection methods
#'
#' A `quadmesh` method for [reproj::reproj()].
#'
#' @param x coordinates
#' @param source source specification (PROJ.4 string or epsg code)
#' @param target target specification (PROJ.4 string or epsg code)
#' @param ... arguments passed to [proj4::ptransform()]
#' @importFrom reproj reproj
#' @export reproj
#' @export
#' @name reproj
reproj.quadmesh <- function(x, target, ..., source = NULL) {
  existingproj <- x$crs
  x$vb[1:3, ] <- t(reproj::reproj(t(x$vb[1:3, ]), target = target, source = existingproj))
  x$raster_metadata <- x$crs <- NULL
  warning("quadmesh raster information cannot be preserved after reprojection, dropping to mesh3d class")
  class(x) <- setdiff( class(x), "quadmesh")
  x
}

reproj.triangmesh <- function(x, target, ..., source = NULL) {
  reproj.quadmesh(x, target = target, source = source)
}
