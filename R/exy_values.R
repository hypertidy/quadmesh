expand_xy <- function(x, y) {
  ## fast matrix version of expand.grid
  cbind(x, rep(y, each = length(x)))
}

edges_xy <- function(x, ...) {
  ## corner coordinates from a matrix in 0,nrow 0,ncol
  dm <- dim(x)
  xx <- seq(0, dm[1L], length = dm[1L] + 1L)
  yy <- seq(dm[2L], 0, length = dm[2L] + 1L)
  expand_xy(x = xx, y = yy)
}


## from a raster, calculate values at cell corner
## allow only 1-layer in-mem rasters, not lazy ones
vxy_raster <- function(x, ...) {
  if (is.numeric(x@data@values) && length(x@data@values) > 0) {
    nr <- x@nrows
    nc <- x@ncols
    m <- if (is.matrix(x@data@values)) x@data@values[,1L, drop = TRUE] else x@data@values
    m <- matrix(m, nc, nr)
  } else {
    stop("raster is not in-memory, cannot obtain values - use 'raster::readAll(x[[1]])' before conversion to mesh")
  }
  vxy(m)
}
to_raster <- function(x, ...) {
  t(x[nrow(x):1, ])
}
## from a matrix, calculate values at cell corner
vxy <- function(x, ...) {
  dm <- dim(x)
  nr <- dm[1L]
  nc <- dm[2L]
  ## top left
  tl <- cbind(NA_integer_, rbind(NA_integer_, x))
  ## top right
  tr <- cbind(NA_integer_, rbind(x, NA_integer_))
  ## bottom left
  bl <- cbind(rbind(NA_integer_, x), NA_integer_)
  ## bottom right
  br <- cbind(rbind(x, NA_integer_), NA_integer_)

 .colMeans(matrix(c(tl, tr, bl, br), 4L, byrow = TRUE),
                  m = 4L, n = (nr + 1L) * (nc + 1L),
                 na.rm = TRUE)
}


as_mesh3d_matrix <- function(x,...) {
  v <- vxy(x)
  exy <- edges_xy(x)

  dm <- dim(x)
  ## this was developed against raster, so nc is nr ;)
  nc <- dm[1L]
  nr <- dm[2L]
  nc1 <- nc + 1
  aa <- t(prs(seq_len(nc1)))
  ind <- matrix(c(rbind(aa, aa[2:1, ])) + c(0, 0, nc1, nc1), 4)
  ind0 <- as.integer(as.vector(ind) +
                       rep(seq(0, length = nr, by = nc1), each = 4 * nc))
  ind1 <- matrix(ind0, nrow = 4)
  ## for a matrix, we are done
  ## for raster, we have to apply the extent transformation
  cols <- viridis::viridis(100)
  rgl::qmesh3d(rbind(t(exy), v, 1),
               ind1,
               material = list(color = cols[scales::rescale(v, to = c(1, 100))])
  )
}

as_mesh3d_raster <- function(x, ...) {
  v <- vxy_raster(x)
  exy <- edgesXY(x)
  nc <- x@ncols
  nc1 <- nc + 1
  aa <- t(prs(seq(nc1)))
  ind <- matrix(c(rbind(aa, aa[2:1, ])) + c(0, 0, nc1, nc1), 4)
  ind0 <- as.integer(as.vector(ind) +
                       rep(seq(0, length = nrow(x), by = ncol(x) + 1), each = 4 * dim(x)[2L]))
  ind1 <- matrix(ind0, nrow = 4)

  cols <- viridis::viridis(100)
  rgl::qmesh3d(rbind(t(exy),  v, 1),
               ind1,
               material = list(color = cols[scales::rescale(v, to = c(1, 100))])
               )
}
