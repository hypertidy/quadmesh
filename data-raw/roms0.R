itime <- 1
cpolar <- raadtools:::cpolarfiles()
## we are expected to read  via romsdata3d(fullname[itime], slice =  dim4_slice[itime], lvar = 4)
## as fullname is replicated out for each time step ()
file_db <- purrr::map_df(cpolar$fullname,
                         function(x) tibble::tibble(fullname = x,
                                                    time = tidync(x)$transforms$ocean_time$ocean_time) %>%
                           dplyr::mutate(dim4_slice = row_number()))


roms_file <- file_db$fullname[itime]
roms_slice <- file_db$dim4_slice[itime]
library(angstroms)
roms_ll <- romscoords(roms_file, transpose = TRUE)
saveRDS(list(roms3d = romsdata3d(roms_file, "temp", slice = roms_slice),
             coords = roms_ll), file = "roms3d.rds")


d <- readRDS("data-raw/roms3d.rds")
coords <- d$coords
x <- d$roms3d
ex <- raster::extent(x, 50, 350, 200, 450)
x <- raster::crop(x[[1]], ex)
coords <- raster::crop(coords, ex)
crs = NULL; colfun = NULL; add = FALSE
crs = "+proj=laea +lon_0=100 +lat_0=-60 +datum=WGS84"

mesh_plot(x,crs = "+proj=laea +lon_0=80 +lat_0=-60 +datum=WGS84", coords = coords)
