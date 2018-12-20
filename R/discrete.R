
#' @name quadmesh
#' @export
dquadmesh <- function (x, z = x, na.rm = FALSE, ...,
                           texture = NULL, texture_filename = NULL) {
 UseMethod("dquadmesh")
}

#' @name quadmesh
#' @export
dquadmesh.default  <- function (x, z = x, na.rm = FALSE, ...,
                       texture = NULL, texture_filename = NULL) {

  qm <- quadmesh(x, na.rm = na.rm, ...,
                  texture = texture, texture_filename = texture_filename)


  if (inherits(z, "BasicRaster")) {
    z <- raster::values(x)
  } else {
    z <- raster::values(raster::raster(x))
  }
  ## break the mesh!
  qm$vb <- qm$vb[, qm$ib]
  qm$ib <- matrix(seq_len(ncol(qm$vb)), 4L)
  qm$vb[3L, ] <- rep(z, each = 4L)
  qm
}
