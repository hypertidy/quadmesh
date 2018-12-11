
#' Experimental
#'
#' @param x a triangle mesh (see `triangmesh`)
#' @param grid raster grid
#' @param ... ignored
#' @param a_srs projection that belongs to x coordinates
#'
#' @return RasterLayer
#' @noRd
#'
#' @examples
#' library(raadtools)
#' ice <- readice()
#' ex <- extent(-180, 180, -90, -30)
#' sst <- readsst(xylim = ex)
#' tm <- triangmesh(sst)
#' index <- tri_index(tm, a_srs = projection(sst), grid = ice)
#' ok <- !is.na(index$idx)
#' r <- ice * NA
#' files <- sstfiles()
#' for (i in seq(1, 1000)) {
#' r[ok] <- rowSums(matrix(values(readsst(files$date[i], inputfiles = files, xylim = ex))[tm$it[, index$idx[ok]]], ncol = 3, byrow = TRUE) * index$p[ok, ])
#'
#' plot(r, zlim = c(-2, 14), col = palr::sstPal(36)[1:30])
#'}
tri_index <- function(x, a_srs = NULL, grid = NULL, ...) {
  xy <- t(x$vb[1:2, ])
  if (is.null(grid)) grid <- defaultgrid(xy)
  tri <- t(tm$it)
  xy <- target_coordinates(xy, a_srs, raster::projection(grid))
  value <- tm$vb[3, ]
  rxy <- sp::coordinates(grid)
  pid0 <- geometry::tsearch(xy[,1], xy[,2], t(tm$it), coordinates(grid)[,1], coordinates(grid)[, 2],
                            bary = TRUE)
  pid0
}
