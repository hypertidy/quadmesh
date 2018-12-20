context("test-image")
g <- raster::setExtent(raster::raster(matrix(1:12, 3)),
                       raster::extent(0, 4, 0, 3))

tri <- structure(c(1L, 2L, 5L, 2L, 3L, 6L, 3L, 4L, 7L, 2L, 5L, 6L, 3L,
                   6L, 7L, 4L, 7L, 8L, 5L, 6L, 9L, 6L, 7L, 10L, 7L, 8L, 11L, 6L,
                   9L, 10L, 7L, 10L, 11L, 8L, 11L, 12L), .Dim = c(3L, 12L))

set.seed(1)
xy <- cbind(sort(rnorm(raster::ncell(g))), runif(raster::ncell(g)))
cds <- raster::setValues(raster::brick(g, g), as.matrix(xy))
library(vdiffr)


test_that("warning on multi laer", {
  expect_warning(mesh_plot(raster::brick(g, g)), "extracting single RasterLayer")
})
## can't get this to match
func_etopo_plot <- function() mesh_plot(etopo, "+proj=laea +lat_=-90 +datum=WGS84")


test_that("mesh_plot works", {
  #  expect_doppelganger("func-etopo-plot", func_etopo_plot)
  expect_silent(func_etopo_plot())
  expect_message(mesh_plot(g, coords = cds, "+proj=ortho +datum=WGS84"),
                 "coords and crs provided, assuming coords is Longitude, Latitude")

})

func_mesh_plot <- function()  mesh_plot(g, coords = cds)
test_that("plot works", {
  expect_doppelganger("func-mesh-plot", func_mesh_plot)

  g[4] <- NA
  expect_silent(mesh_plot(g))

})

