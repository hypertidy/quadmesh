## from a raster, calculate values at cell corner
## allow only 1-layer in-mem rasters, not lazy ones
exy <- function(x, ...) {
  if (is.numeric(x@data@values) && length(x@data@values) > 0) {
    nr <- x@nrows
    nc <- x@ncols
    m <- if (is.matrix(x@data@values)) x@data@values[,1L, drop = TRUE] else x@data@values
    m <- matrix(m, nc, nr)
  } else {
    stop("raster is not in-memory, cannot obtain values - use 'raster::readAll(x[[1]])' before conversion to mesh")
  }
  ## top left
  tl <- cbind(NA_integer_, rbind(NA_integer_, m))
  ## top right
  tr <- cbind(NA_integer_, rbind(m, NA_integer_))
  ## bottom left
  bl <- cbind(rbind(NA_integer_, m), NA_integer_)
  ## bottom right
  br <- cbind(rbind(m, NA_integer_), NA_integer_)

 .colMeans(matrix(c(tl, tr, bl, br), 4L, byrow = TRUE),
                  m = 4L, n = (nr + 1L) * (nc + 1L),
                 na.rm = TRUE)
}


as_mesh3d_raster <- function(x, ...) {
  nc <- x@ncols
  nc1 <- nc + 1
  aa <- t(prs(seq(nc1)))
  ind <- matrix(c(rbind(aa, aa[2:1, ])) + c(0, 0, nc1, nc1), 4)
  ind0 <- as.integer(as.vector(ind) +
                       rep(seq(0, length = nrow(x), by = ncol(x) + 1), each = 4 * dim(x)[2L]))
  ind1 <- matrix(ind0, nrow = 4)
  v <- exy(x)
  cols <- viridis::viridis(100)
  rgl::qmesh3d(rbind(t(quadmesh::edgesXY(x)),  v, 1),
               ind1,
               material = list(color = cols[scales::rescale(v, to = c(1, 100))])
               )
}
