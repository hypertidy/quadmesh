
library(quadmesh)
##remotes::install_github("hypertidy/ceramic")
library(ceramic)
library(sf)
library(mapdeck)
sd <- rnaturalearth::ne_states(returnclass = "sf") %>%
  dplyr::filter(name == "Tasmania")

im <- cc_location(st_coordinates(st_centroid(sd)), buffer = 3e5, zoom = 9,  debug = TRUE)
el <- cc_elevation(st_coordinates(st_centroid(sd)), buffer = 4e5, zoom = 7)


prsd <- st_transform(sd, raster::projection(im))
im <- raster::mask(im, prsd)
el <- raster::mask(el, prsd)

tm  <- triangmesh(el)

tm_to_sf <- function(x, exag = 1) {
  ## convert bad z verts to bad triangles
  ibad <- which(is.na(x$vb[3, ]))
  ii <- x$it
  ii[ii %in% ibad] <- NA
  bad <- is.na(colSums(ii))
  x$vb[3, ibad] <- min(x$vb[3, ], na.rm = TRUE)
  x$vb[3, ] <- x$vb[3, ] * exag
  sfc <- vector("list", ncol(x$it))
  sfg <- sf::st_polygon(list(cbind(c(1, 1, 2, 1), c(1, 2, 2, 1), 0)))
  for (i in seq_along(sfc)) {
    sfg[[1]] <- t(x$vb[1:3,x$it[c(1L, 2L, 3L, 1L),i]])
    sfc[[i]] <- sfg
  }
  sf::st_sf(a = seq_len(i), geometry = sf::st_sfc(sfc))[!bad, ]
}
z <- tm_to_sf(tm, exag = 10)
z <- st_set_crs(z, st_crs(im))

## now map colours on
v <- raster::extract(im, st_coordinates(st_centroid(z))[, 1:2])
v[is.na(v)] <- 0
z$my_colour <- rgb(v[,1], v[,2], v[,3], maxColorValue = 255, alpha = 255)
library(mapdeck)

zll <- st_transform(z, 4326)
mapdeck( ) %>%
  add_sf(
    data = zll
    , fill_colour = "my_colour", update_view = TRUE
  )

