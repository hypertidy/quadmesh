context("test-triangle-mesh")

test_that("triangle mesh works", {
  ## test will change when triangle mesh is properly featured
  triangmesh(etopo) %>%
    expect_s3_class("mesh3d") %>%
    expect_s3_class("triangmesh") %>%
    expect_named(c("vb",   "material", "texcoords", "meshColor", "it", "raster_metadata", "crs"))
})
