target_coordinates <- function(xy, src.proj = NA_character_, target = NULL) {
  if (is.null(target)) return(xy)
  dst.proj <- raster::projection(target)
  if (!is.na(src.proj) && !is.na(src.proj)) {
    xy <- reproj::reproj(xy, source = src.proj, target = dst.proj)
   }  ## otherwise we just assume they are in the same crs
  xy
}



texture_coordinates <- function(texture_image,vertices) {
  #browser()
  raster::xyFromCell(raster::setExtent(texture_image, raster::extent(0, 1, 0, 1)),
                     raster::cellFromXY(texture_image, vertices))

}
