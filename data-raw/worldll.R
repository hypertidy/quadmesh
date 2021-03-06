data("wrld_simpl", package = "maptools")
wrld_simpl$id <- 1:nrow(wrld_simpl)
worldll <- fasterize::fasterize(sf::st_as_sf(wrld_simpl),
                                raster::raster(extent(-180, 180, -90, 90), crs = projection(wrld_simpl),  res = 1), field = "id")

usethis::use_data(worldll)
