#' @keywords internal
#' @aliases quadmesh-package
"_PACKAGE"

#' World raster map
#'
#' A rasterized version of `wrld_simpl`, created by burning the country
#' polygon ID number into a one-degree world raster. (This is a very
#' out of date polygon data set used for example only).See code in
#' 'data-raw/worldll.R'.
#' @docType data
#' @name worldll
NULL

#' World topography map
#'
#' A simplified version of 'Etopo2'. The Etopo2 data set was
#' reduced 20X to create this raster layer of global relief. See code
#' in 'data-raw/topo.R'.
#' @docType data
#' @name etopo
NULL

#' World map
#'
#' The world coastline coordinates. A simple matrix of lon, lat, separated by NA.
#'
#' From the maps package, see 'data-raw/xymap.R'.
#' @docType data
#' @name xymap
NULL

