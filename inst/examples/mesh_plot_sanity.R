# quadmesh benchmark
library(raster)
library(quadmesh)

r <- quadmesh::etopo

## basic plot
mesh_plot(r)

laea <- "+proj=laea +lat_0=-90 +ellps=sphere"
## projected plot
mesh_plot(r, crs = laea, asp = 1)

coords <- setValues(brick(r, r), reproj::reproj(coordinates(r), target = laea, source = projection(r)))
coords_ll <- setValues(brick(r, r), coordinates(r))
## plot base on input coords
mesh_plot(r, coords = coords, asp = 1, col = viridis::viridis(26))

## this is undefined? (added a warning before it errors)
mesh_plot(r, coords = coords, crs = "+proj=lcc +lat_0=-50 +lat_2=-40 +ellps=WGS84")
## this does work, so we need it for model grids
mesh_plot(r, coords = coords_ll, crs = "+proj=lcc +lat_0=-50 +lat_2=-40 +ellps=WGS84")

cmip6 <- crop(cmip6, extent(-1, 256, 150, 220))
## for e.g.
plot(cmip6[[1]])
# lon <- cmip6[[2]]
# lon2 <- crop(lon, extent(lon, 1, nrow(lon), 100, ncol(lon)))
# lon2[lon2 < 180] <- lon2[lon2 < 180] + 360
#
# llon <- raster::merge(crop(lon, extent(lon, 1, nrow(lon), 1, 99)),
#               lon2)
# brick(llon - 360, cmip6[[3]])
lon <- cmip6[[2]]
lon[lon > 180] <- lon[lon > 180] - 360
lat <- cmip6[[3]]
plot(NA, xlim = c(0, 360), ylim = c(-90, 80))
mesh_plot(cmip6[[1]], coords = brick(lon, lat),
          col = viridis::viridis(100), add = F, crs = "+proj=laea +lat_0=-90 +datum=WGS84")


mesh <- quadmesh(cmip6[[1]])
mesh$vb[1, ]  <- quadmesh(cmip6[[2]])$vb[3, ]
mesh$vb[2, ]  <- quadmesh(cmip6[[3]])$vb[3, ]
mesh$vb[1:3, ] <- t(reproj::reproj(t(mesh$vb[1:2, ]), source = "+proj=longlat +datum=WGS84", target = "+proj=geocent +datum=WGS84"))[1:3, ]
mesh_plot(mesh)

maps::map(add  = TRUE)
