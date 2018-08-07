context("test-basic.R")



test_that("quadmesh works", {
  qm <- quadmesh(worldll)
  expect_equal(dim(qm$vb), c(4L, 65341L))
  expect_equal(dim(qm$ib), c(4L, 64800L))

})
