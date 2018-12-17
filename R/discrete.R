#' @name dquadmesh
#' @export
dquadmesh <- function (x, z = x, na.rm = FALSE, ...,
                           texture = NULL, texture_filename = NULL) {
 UseMethod("dquadmesh")
}
#' @name dquadmesh
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

  qm$ib <- matrix(seq_len(ncol(qm$vb)), nrow(qm$vb))
  ## this needs to be an option to quadmesh, it's prior to the continuous form in this sense
  ## and we shouldn't be rederiving this constant value here
  ## also discrete will require exploding out the texture coordinates as well
  #  qm$vb[3, ] <- rep(colMeans(matrix(qm$vb[3,], 4)), each = 4)
  qm$vb[3, ] <- rep(z, each = 4)  ## SO MUCH SIMPLER
  qm
}
