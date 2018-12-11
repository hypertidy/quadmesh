context("test-raster")
g <- raster::setExtent(raster::raster(matrix(1:12, 3)),
                       raster::extent(0, 4, 0, 3))

qm <- quadmesh(g)
qm1 <- qm
qm1$raster_meta <- NULL
test_that("round-trip raster structure works", {
  expect_equal(raster::raster(g), raster::raster(qm_as_raster(qm)))
  expect_equal(raster::raster(g), raster::raster(qm_as_raster(qm1)))

})
