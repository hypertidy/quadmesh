exy <- function(x, ...) {
  v <- raster::values(x)
  nr <- dim(x)[1]
  nc <- dim(x)[2]
  m <- matrix(v, nc, nr)
  ## top left
  (tl <- cbind(NA_integer_, rbind(NA_integer_, m)))
  ## top right
  (tr <- cbind(NA_integer_, rbind(m, NA_integer_)))
  ## bottom left
  (bl <- cbind(rbind(NA_integer_, m), NA_integer_))
  ## bottom right
  (br <- cbind(rbind(m, NA_integer_), NA_integer_))

  evals <- colMeans(rbind(c(tl),
                               c(tr),
                               c(bl),
                               c(br)), na.rm = TRUE)

  evals
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
