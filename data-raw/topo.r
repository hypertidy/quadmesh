library(raadtools)
topo <- readtopo("etopo2")
decimate <- function(x, dec = 10) {
  r0 <- raster(x)
  res(r0) <- res(x) * dec
  setValues(r0, extract(x, coordinates(r0), method = "bilinear"))
}

etopo <- decimate(crop(topo, extent(-180, 180, -90, 0)), dec = 20)
devtools::use_data(etopo, overwrite = TRUE, compress = "xz")
