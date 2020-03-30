test_that("multiplication works", {
  ## WHAT FAILS
  ##mesh_plot(worldll)
  ## crop otherwise out of bounds from PROJ
  rr <- raster::crop(worldll, raster::extent(-179, 179, -89, 89))
  mesh_plot(rr, crs = "+proj=laea +datum=WGS84")
  #mesh_plot(rr, crs = "+proj=moll +datum=WGS84")
  prj <- "+proj=lcc +datum=WGS84 +lon_0=147 +lat_0=-40 +lat_1=-55 +lat_2=-20"
  mesh_plot(etopo, crs = prj, add = FALSE, col = grey(seq(0, 1, length = 20)))

})
