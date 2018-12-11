context("test-texture")

g <- raster::setExtent(raster::raster(matrix(1:12, 3)), raster::extent(0, 4, 0, 3))
test_that("texturing works", {
  expect_silent(texture_coordinates(raster::brick(etopo, etopo, etopo), etopo))

  expect_error(quadmesh(g, texture = 1e6), "texture must be a 3-layer raster with RGB values")
  expect_error(quadmesh(g, texture = raster::brick(g, g)), "texture must be a 3-layer raster with RGB values")

  expect_s3_class(quadmesh(matrix(1:12, 3)), "mesh3d")
})

test_that("textures", {
  skip_on_cran()
  expect_message(quadmesh(g, texture = raster::brick(g, g, g)), "writing texture image")
  expect_message(quadmesh(g, texture = raster::brick(g, g, g), texture_filename = tempfile(fileext = "png")), "writing texture image")

})
