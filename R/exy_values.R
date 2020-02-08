exy <- function(x, ...) {
  exy <- quadmesh:::edgesXY(x)

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

  exy_values <- colMeans(rbind(c(tl),
                               c(tr),
                               c(bl),
                               c(br)), na.rm = TRUE)

  cbind(exy, exy_values)
}
