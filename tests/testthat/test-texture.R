context("test-texture")

test_that("texturing works", {
  expect_silent(texture_coordinates(raster::brick(etopo, etopo, etopo), etopo))
})
