#' @keywords internal
#' @aliases quadmesh-package
"_PACKAGE"


#' CMIP6 sample
#'
#' A small extract of model output and native grid ('gn') coordinates from CMIP6. Derived from
#' 'CMIP6/ssp245/intpp/intpp_Omon_MPI-ESM1-2-LR_ssp245_r1i1p1f1_gn_201501-203412.nc'.
#'
#' The [cmip6] object is a 'RasterBrick', defined by the raster package with three
#' layers: 'intpp', 'longitude', 'latitude'. The model data is primary organic carbon production 'intpp'.
#' @section Source:
#' \itemize{
#'   \item A small extract of data and grid coordinates from CMIP 6 produced by the MPI-M.
#'     \itemize{
#'       \item Description: Primary Organic Carbon Production by All Types of Phytoplankton.
#'       \item Source: Max Planck Institute for Meteorology, Hamburg 20146, Germany (MPI-M)
#'       \item URL: https://data.ccamlr.org/dataset/small-scale-management-units
#'       \item Reference: doi:10.1029/2017MS001217
#'       \item License: CC BY-SA 4.0
#'     }
#'     }
#' @docType data
#' @keywords datasets
#' @examples
#' mesh_plot(cmip6[[1]])
"cmip6"


#' World raster map
#'
#' A rasterized version of `wrld_simpl`, created by burning the country
#' polygon ID number into a one-degree world raster. (This is a very
#' out of date polygon data set used for example only).See code in
#' 'data-raw/worldll.R'.
#' @docType data
#' @name worldll
#' @importFrom reproj reproj
#' @export reproj
#' @keywords datasets
"worldll"



#' World topography map
#'
#' A simplified version of 'Etopo2'. The Etopo2 data set was
#' reduced 20X to create this raster layer of global relief. See code
#' in 'data-raw/topo.R'.
#' @docType data
#' @name etopo
#' @keywords datasets
"etopo"

#' World map
#'
#' The world coastline coordinates. A simple matrix of lon, lat, separated by NA.
#'
#' From the maps package, see 'data-raw/xymap.R'.
#' @docType data
#' @name xymap
#' @keywords datasets
"xymap"
