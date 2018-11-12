library(ceramic)
mapbox_sat  <- ceramic:::cc_location(cbind(137, -32), buffer = c(1e6, 5e5), type = "mapbox.satellite")
projection(mapbox_sat) <- "+proj=merc +a=6378137 +b=6378137"
library(raster)
mapbox_sat <- aggregate(mapbox_sat, fact = 4)


#' Mapbox satellite image.
#'
#' Satellite image of southern Australia.
#'
#'
#' @docType data
#' @name mapbox_sat
NULL

## TOS says no
## https://www.mapbox.com/tos/#[YmtcYmns]
#usethis::use_data(mapbox_sat, compress = "xz")

