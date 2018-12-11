context("test-raster")
g <- raster::setExtent(raster::raster(matrix(1:12, 3)),
                       raster::extent(0, 4, 0, 3))

qm <- quadmesh(g)
qm1 <- qm
qm1$raster_metadata <- NULL
test_that("round-trip raster structure works", {
  expect_equal(raster::raster(g), raster::raster(qm_as_raster(qm)))
  x1 <- expect_warning(raster::raster(qm_as_raster(qm1)), "original raster_metadata has been stripped")
  expect_equal(raster::raster(g),x1)
  expect_silent(qm_as_raster(quadmesh(g), index = 1))
  expect_error(qm_as_raster(quadmesh(g), index = 0), "index is out of range")
  expect_error(qm_as_raster(g), "only mesh3d supported")

})


test_that("missing values handled", {
  g[4] <- NA
  expect_silent(quadmesh(g))
})
