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

