u <- "https://eoimages.gsfc.nasa.gov/images/imagerecords/73000/73909/world.topo.bathy.200412.3x5400x2700.png"
f <- file.path("data-raw", basename(u))
if (!file.exists(f)) curl::curl_download(u, f)
library(lazyraster)
library(raster)
#dm <- c(216, 108)
#bm <- setExtent(raster::brick(as_raster(lazyraster::lazyraster(f, band = 1), dm),
#                    as_raster(lazyraster::lazyraster(f, band = 2), dm),
#                    as_raster(lazyraster::lazyraster(f, band = 3), dm,)),
#                extent(-180, 180, -90, 90))
bm <- setExtent(raster::brick(f), extent(-180, 180, -90, 90))
projection(bm) <- "+proj=longlat +datum=WGS84"

bm <- aggregate(bm, fact = 25)
.hcl_colors <- hcl.colors(12, "YlOrRd", rev = TRUE)

usethis::use_data(.hcl_colors, bm, internal=TRUE, overwrite = TRUE)
