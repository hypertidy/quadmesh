context("test-barycentric")

g <- raster::setExtent(raster::raster(matrix(1:12, 3)),
                       raster::extent(0, 4, 0, 3))

tri <- structure(c(1L, 2L, 5L, 2L, 3L, 6L, 3L, 4L, 7L, 2L, 5L, 6L, 3L,
                   6L, 7L, 4L, 7L, 8L, 5L, 6L, 9L, 6L, 7L, 10L, 7L, 8L, 11L, 6L,
                   9L, 10L, 7L, 10L, 11L, 8L, 11L, 12L), .Dim = c(3L, 12L))

set.seed(1)
xy <- cbind(sort(rnorm(raster::ncell(g))), runif(raster::ncell(g)))
cds <- raster::setValues(raster::brick(g, g), as.matrix(xy))

test_that("barycentric index works", {
  bi <- bary_index(g) %>% expect_length(3) %>% expect_named(c("idx", "p", "tri"))
  expect_equal(bi$tri, tri)
  bd <- bary_index(g, coords = cds)
  expect_equal(sum(is.na(bd$idx)), 1699)
  expect_equal(sum(which(!is.na(bd$idx))), 1843261)

})

