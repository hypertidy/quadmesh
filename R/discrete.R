#' @name dquadmesh
#' @noRd
dquadmesh <- function (x, z = x, na.rm = FALSE, ...,
                           texture = NULL, texture_filename = NULL) {
 UseMethod("dquadmesh")
}
#' @name dquadmesh
#' @noRd
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


#' @name dtriangmesh
#' @noRd
dtriangmesh <- function (x, z = x, na.rm = FALSE, ...,
                       texture = NULL, texture_filename = NULL) {
  UseMethod("dtriangmesh")
}
#' @name dtriangmesh
#' @noRd
dtriangmesh.default  <- function (x, z = x, na.rm = FALSE, ...,
                                texture = NULL, texture_filename = NULL) {

  #tm <- triangmesh(x, z = z, na.rm = na.rm, ...,
  #               texture = texture, texture_filename = texture_filename)
 ## break the mesh!
  #tm$vb <- tm$vb[, tm$it]

  #tm$it <- matrix(seq_len(ncol(tm$vb)), 3L)
  # tm$vb[3L,] <- tapply(tm$vb[3L,], rep(seq_len(ncol(tm$vb)/3), each = 3))
  tm <- dquadmesh(x, z = z, na.rm = na.rm, ..., texture = texture, texture_filename = NULL)
  if (inherits(z, "BasicRaster")) {
    tm$vb[3, ] <- raster::extract(z, t(tm$vb[1:2, ]), type = "bilinear")
  }
  tm$primitivetype <- "triangle"
  tm$it <- triangulate_quads(tm$ib)
  tm$ib <- NULL
 tm
}
