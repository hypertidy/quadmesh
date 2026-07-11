r0 <- raster::setExtent(raster::raster(matrix(1:12, 3)), raster::extent(0, 4, 0, 3))

test_that("z argument is honoured", {
  z2 <- r0 * 10

  ## continuous quadmesh: bilinear corner values are linear in the data,
  ## so scaling the z raster scales the vertex z exactly
  qm1 <- quadmesh(r0)
  qm2 <- quadmesh(r0, z = z2)
  expect_equal(qm2$vb[3, ], qm1$vb[3, ] * 10)

  ## discrete quadmesh takes cell values directly
  dq <- dquadmesh(r0, z = z2)
  expect_equal(dq$vb[3, ], rep(raster::values(z2), each = 4L))

  ## triangmesh takes cell values directly (vertices are cell centres)
  tm <- triangmesh(r0, z = z2)
  expect_equal(tm$vb[3, ], raster::values(z2))

  ## discrete triangles are constant at the mean of their three cell values
  ## (triangles straddle cells, there is no single pixel value per triangle,
  ## use dquadmesh + triangulate_quads for a pixel-valued triangle mesh)
  dtm <- dtriangmesh(r0, z = z2)
  expected <- rep(.colMeans(matrix(tm$vb[3, tm$it], nrow = 3L), 3L, ncol(tm$it)),
                  each = 3L)
  expect_equal(dtm$vb[3, ], expected)
})

test_that("constant numeric z is honoured", {
  qmc <- quadmesh(r0, z = 1.5)
  expect_true(all(qmc$vb[3, ] == 1.5))

  tmc <- triangmesh(r0, z = 1.5)
  expect_true(all(tmc$vb[3, ] == 1.5))
})

test_that("triangmesh na.rm drops exactly the triangles that touch NA cells", {
  ## use a grid where cell count (20) differs from triangle count (24),
  ## which guards against logical-recycling in the index subset
  r1 <- raster::setExtent(raster::raster(matrix(1:20, 4)), raster::extent(0, 5, 0, 4))
  r1[7] <- NA

  full <- triangmesh(r1, na.rm = FALSE)
  tm <- triangmesh(r1, na.rm = TRUE)

  expect_equal(ncol(full$it), 2L * 3L * 4L)
  expect_lt(ncol(tm$it), ncol(full$it))
  ## no retained triangle references a missing cell
  expect_false(any(is.na(tm$vb[3, tm$it])))
})
