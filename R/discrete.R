
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

  ## the discrete z is applied per cell below, no need for the
  ## corner interpolation (z = NULL skips it)
  qm <- quadmesh(x, z = NULL, na.rm = na.rm, ...,
                  texture = texture, texture_filename = texture_filename)

  if (inherits(z, "SpatRaster")) z <- raster::raster(z)
  if (inherits(z, "BasicRaster")) {
    z <- raster::values(z[[1L]])
  } else {
    z <- raster::values(raster::raster(x))
  }
  if (na.rm) {
    ## quads were dropped for cells missing in x, keep z aligned
    xv <- if (inherits(x, "BasicRaster")) raster::values(x[[1L]]) else raster::values(raster::raster(x))
    z <- z[!is.na(xv)]
  }
  ## break the mesh (expands vertices, and texcoords when present)
  qm <- textures::break_mesh(qm)
  qm$vb[3L, ] <- rep(z, each = 4L)
  qm
}
