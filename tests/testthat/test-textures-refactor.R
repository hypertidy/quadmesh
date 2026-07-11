r0 <- raster::setExtent(raster::raster(matrix(1:12, 3)), raster::extent(0, 4, 0, 3))

test_that("mesh structure is unchanged by the textures backend", {
  qm <- quadmesh(r0)
  ## 5 x 4 vertices, 12 quads
  expect_equal(dim(qm$vb), c(4L, 20L))
  expect_equal(dim(qm$ib), c(4L, 12L))
  ## historical corner order (TL, TR, BR, BL) with vertices enumerated
  ## from the top row of the raster, x fastest
  expect_equal(qm$ib[, 1], c(1L, 2L, 7L, 6L))
  ## first vertex is the top-left corner of the extent
  expect_equal(qm$vb[1:2, 1], c(0, 3))
  ## quads are in raster cell order: discrete z recovers cell values exactly
  dq <- dquadmesh(r0)
  expect_equal(.colMeans(matrix(dq$vb[3, dq$ib], nrow = 4L), 4L, ncol(dq$ib)),
               raster::values(r0))
})

test_that("dquadmesh na.rm aligns z with retained quads", {
  g <- raster::setExtent(raster::raster(matrix(1:12, 3)), raster::extent(0, 4, 0, 3))
  g[4] <- NA
  dq <- dquadmesh(g, na.rm = TRUE)
  expect_equal(ncol(dq$ib), 11L)
  expect_equal(ncol(dq$vb), 44L)
  expect_false(anyNA(dq$vb[3, ]))
})

test_that("break_mesh expands texcoords for discrete meshes", {
  skip_on_cran()
  tex <- raster::brick(r0, r0, r0)
  dqt <- suppressMessages(dquadmesh(r0, texture = tex))
  expect_equal(ncol(dqt$texcoords), ncol(dqt$vb))
  expect_equal(dqt$ib, matrix(seq_len(ncol(dqt$vb)), 4L))
})

test_that("texture may be a PNG file path", {
  tf <- tempfile(fileext = ".png")
  png::writePNG(matrix(seq(0, 1, length.out = 12), 3), tf)

  qmt <- quadmesh(r0, texture = tf)
  expect_identical(qmt$material$texture, tf)
  expect_equal(dim(qmt$texcoords), c(2L, ncol(qmt$vb)))
  expect_true(all(qmt$texcoords >= 0 & qmt$texcoords <= 1))
  ## corner vertices map to the image corners
  expect_equal(range(qmt$texcoords[1, ]), c(0, 1))
  expect_equal(range(qmt$texcoords[2, ]), c(0, 1))

  tmt <- triangmesh(r0, texture = tf)
  expect_identical(tmt$material$texture, tf)
  expect_equal(dim(tmt$texcoords), c(2L, ncol(tmt$vb)))

  ## a missing file warns but still constructs
  expect_warning(quadmesh(r0, texture = tempfile(fileext = ".png")),
                 "does not exist")
})
