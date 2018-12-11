context("test-tri-quads")

g <- raster::setExtent(raster::raster(matrix(1:12, 3)),
                       raster::extent(0, 4, 0, 3))

qm <- quadmesh(g)
qtri <- structure(c(1L, 6L, 2L, 6L, 7L, 2L, 2L, 7L, 3L, 7L, 8L, 3L, 3L,
                    8L, 4L, 8L, 9L, 4L, 4L, 9L, 5L, 9L, 10L, 5L, 6L, 11L, 7L, 11L,
                    12L, 7L, 7L, 12L, 8L, 12L, 13L, 8L, 8L, 13L, 9L, 13L, 14L, 9L,
                    9L, 14L, 10L, 14L, 15L, 10L, 11L, 16L, 12L, 16L, 17L, 12L, 12L,
                    17L, 13L, 17L, 18L, 13L, 13L, 18L, 14L, 18L, 19L, 14L, 14L, 19L,
                    15L, 19L, 20L, 15L), .Dim = c(3L, 24L))

qtri_clock <- structure(c(1L, 2L, 6L, 2L, 7L, 6L, 2L, 3L, 7L, 3L, 8L, 7L, 3L,
                          4L, 8L, 4L, 9L, 8L, 4L, 5L, 9L, 5L, 10L, 9L, 6L, 7L, 11L, 7L,
                          12L, 11L, 7L, 8L, 12L, 8L, 13L, 12L, 8L, 9L, 13L, 9L, 14L, 13L,
                          9L, 10L, 14L, 10L, 15L, 14L, 11L, 12L, 16L, 12L, 17L, 16L, 12L,
                          13L, 17L, 13L, 18L, 17L, 13L, 14L, 18L, 14L, 19L, 18L, 14L, 15L,
                          19L, 15L, 20L, 19L), .Dim = c(3L, 24L))
test_that("triangulating quads works", {
  expect_equal(triangulate_quads(qm$ib), qtri)
  expect_equal(triangulate_quads(qm$ib, clockwise = TRUE), qtri_clock)

})
