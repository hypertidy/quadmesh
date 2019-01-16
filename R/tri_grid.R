
#' Barycentric triangle index for interpolation
#'
#' This function returns the barycentric weight for a grid of coordinates
#' from a geographic raster.
#'
#' It's not as fast as [raster::projectRaster()] (e.g. `projectRaster(x, grid)`) but it
#' also accepts a `coords` argument and so can be used for non-regular raster
#' reprojection.
#'
#' 'coords' may be 'NULL' or longitude, latitude in a 2-layer raster brick or stack as with
#' `mesh_plot`.
#' @param x a 'RasterLayer' source
#' @param grid target 'RasterLayer', a target regular grid
#' @param ... ignored
#' @param coords optional input coordinates
#'
#' @return RasterLayer
#' @export
#' @importFrom geometry tsearch
#' @examples
#' library(raster)
#' p_srs <- "+proj=stere +lat_0=-90 +lat_ts=-71 +datum=WGS84"
#' polar <- raster(extent(-5e6, 5e6, -5e6, 5e6), crs = p_srs, res = 25000)
#' etopo <- aggregate(etopo, fact = 4)
#' index <- bary_index(etopo, grid = polar)
#' ok <- !is.na(index$idx)
#' r <- setValues(polar, NA_integer_)
#' r[ok] <- colSums(matrix(values(etopo)[index$tri[, index$idx[ok]]], nrow = 3) * t(index$p)[, ok])
#' plot(r)
bary_index <- function(x, coords = NULL, grid = NULL, ...) {
  tm <- triangmesh(x)
  if (!is.null(coords)) {
    xy <- raster::values(coords)
  } else {
    xy <- sp::coordinates(x)
  }
  if (is.null(grid)) grid <- default_grid(xy)

  a_srs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  xy <- target_coordinates(xy, a_srs, raster::projection(grid), xyz = FALSE)
  #value <- tm$vb[3, ]
  rxy <- sp::coordinates(grid)

 # nn <- RANN::nn2(xy[x$it[1, ], 1:2], rxy, k = 1)
 #  ord <- order(nn$nn.idx)
  pid0 <- geometry::tsearch(xy[,1], xy[,2], t(tm$it), rxy[,1], rxy[, 2],
                            bary = TRUE)
  pid0$tri <- tm$it
  pid0
}

default_grid <- function (xy, ncols = 60, nrows = 50, prj = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")
{
  raster::raster(raster::extent(xy), ncols = ncols, nrows = nrows,
                 crs = prj)
}
