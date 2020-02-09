## from a raster, calculate values at cell corners
## TODO: remove need for raster functions values(), dim()
## and port into mesh3d package (allow in-mem rasters, not lazy ones)
exy <- function(x, ...) {
  v <- raster::values(x)
  nr <- dim(x)[1]
  nc <- dim(x)[2]
  m <- matrix(v, nc, nr)
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


as_mesh3d <- function(x, ...) {
  nc1 <- ncol(x) + 1
  aa <- t(prs(seq(nc1)))
  ind <- matrix(c(rbind(aa, aa[2:1, ])) + c(0, 0, nc1, nc1), 4)
  ind0 <- as.integer(as.vector(ind) +
                       rep(seq(0, length = nrow(x), by = ncol(x) + 1), each = 4 * dim(x)[2L]))
  ind1 <- matrix(ind0, nrow = 4)
  rgl::qmesh3d(rbind(t(quadmesh::edgesXY(x)),  exy(x), 1),
               ind1
               )
}
