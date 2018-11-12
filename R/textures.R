target_coordinates <- function(xy, src.proj = NA_character_, target) {
  dst.proj <- raster::projection(target)
  if (!is.na(src.proj) && !is.na(src.proj)) {
    if (raster::isLonLat(src.proj)) {
      ## src is lonlat, so to radians
      xy <- xy * pi / 180
    }
    xy <- proj4::ptransform(xy, src.proj = src.proj, dst.proj = dst.proj)
    if (is.list(xy)) xy <- do.call(cbind, xy)[, 1:2, drop = FALSE]
    if (raster::isLonLat(dst.proj)) {
      xy <- xy * 180 / pi
    }
  }  ## otherwise we just assume they are in the same crs
  xy
}



texture_coordinates <- function(texture_image,vertices) {
  #browser()
  raster::xyFromCell(raster::setExtent(texture_image, raster::extent(0, 1, 0, 1)),
                     raster::cellFromXY(texture_image, vertices))

}
