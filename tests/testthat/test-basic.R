context("test-basic.R")


g <- raster::raster(matrix(1:12, 3))
g[4] <- NA
test_that("quadmesh works", {
  qm <- quadmesh(worldll)
  expect_equal(dim(qm$vb), c(4L, 65341L))
  expect_equal(dim(qm$ib), c(4L, 64800L))
   expect_silent(quadmesh(g, na.rm = TRUE))
   expect_silent(triangmesh(g, na.rm = TRUE))
   expect_silent(quadmesh(g, z = NULL, na.rm = TRUE))
   expect_silent(triangmesh(g, z = NULL, na.rm = TRUE))

})
