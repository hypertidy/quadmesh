#' @importFrom reproj reproj
#' @export reproj
#' @export
#' @name reproj
reproj.quadmesh <- function(x, target, ..., source = NULL) {
  existingproj <- x$crs
  if (!is.null(source)) {
    if (!is.null(existingproj)) {
      warning("'source' provided and object has a recorded '$crs', will be ignored and 'source' used")
    }
    existingproj <- source
  }

  x$vb[1:3, ] <- t(reproj::reproj(t(x$vb[1:3, ]), target = target, source = existingproj))
  x$crs <- target
  x
}
