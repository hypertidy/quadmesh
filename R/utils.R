#' Angular coordinates to X, Y, Z.
#'
#' @param lonlatheight matrix or data.frame of lon,lat,height values
#' @param rad radius of sphere
#' @param exag exaggeration to apply to height values (added to radius)
#'
#' @return matrix
#' @export
llh2xyz <- function(lonlatheight, rad = 6378137.0, exag = 1) {
  cosLat = cos(lonlatheight[,2] * pi / 180.0)
  sinLat = sin(lonlatheight[,2] * pi / 180.0)
  cosLon = cos(lonlatheight[,1] * pi / 180.0)
  sinLon = sin(lonlatheight[,1] * pi / 180.0)

  rad <- (exag * lonlatheight[,3] + rad)
  x = rad * cosLat * cosLon
  y = rad * cosLat * sinLon
  z = rad * sinLat

  cbind(x, y, z)
}

.ok_ll <- function(x, ...) {
  UseMethod(".ok_ll")
}
.ok_ll.matrix <- function (x, ...)
{
  .ok_ll(c(range(x[, 1L], na.rm = TRUE), range(x[, 2L],
                                                   na.rm = TRUE)))
}
.ok_ll.numeric <- function (x, ...)
{
  x[1] >= -360 && x[2] <= 360 && x[3] >= -90 && x[4] <= 91
}
