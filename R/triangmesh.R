#' Triangle mesh.
#'
#' @inheritParams quadmesh
#' @return mesh3d (primitivetype triangle)
#' @export
#'
#' @examples
#' library(raster)
#' r <- setExtent(raster(volcano), extent(0, nrow(volcano), 0, ncol(volcano)))
#' tm <- triangmesh(r)
#' #rgl::shade3d(tm)
triangmesh <- function (x, z = x, na.rm = FALSE, ..., texture = NULL, texture_filename = NULL) {
  UseMethod("triangmesh")
}
#' @name triangmesh
#' @export
triangmesh.matrix <- function (x, z = x, na.rm = FALSE, ..., texture = NULL, texture_filename = NULL)  {
  x <- raster::setExtent(raster::raster(x), raster::extent(0, ncol(x), 0, nrow(x)))
  if (is.matrix(z)) {
    #warning("z is a matrix ...")
    z <- x
  }
  triangmesh(x, z = z, na.rm = na.rm, ..., texture = texture, texture_filename = texture_filename)
}

#' @name triangmesh
#' @export
triangmesh.BasicRaster <- function (x, z = x, na.rm = FALSE, ..., texture = NULL, texture_filename = NULL)  {
  x <- x[[1]]  ## just the oneth raster for now

  coords <- sp::coordinates(x)

  exy <- edgesXY(x)
  nc1 <- ncol(x) + 1
  aa <- rbind(t(prs(seq_len(ncol(x)))),
              seq_len(ncol(x)-1) + ncol(x))
  bb <- rbind(seq_len(ncol(x) - 1) + 1, t(prs(seq_len(ncol(x)) + ncol(x))))
  ind <- cbind(aa, bb)  ## TODO interleave these so they paired in the expected order
  ## all face indexes
  ind0 <- as.integer(as.vector(ind) +
                       rep(seq(0, length.out = nrow(x) - 1, by = ncol(x)), each = length(ind)))
  ind1 <- matrix(ind0, nrow = 3)

  if (na.rm) {
    ind1 <- ind1[,!is.na(raster::values(x))]
  }


  if (is.null(z)) z <- 0 else z <- raster::values(x)
  structure(list(it = ind1, vb = t(cbind(coords, z, 1)), primitivetype = "triangle", material = list()), class = c("mesh3d", "shape3d"))
}
