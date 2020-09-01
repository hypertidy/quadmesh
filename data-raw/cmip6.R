f <- "/perm_storage/home/-----/CMIP6/ssp245/intpp/intpp_Omon_MPI-ESM1-2-LR_ssp245_r1i1p1f1_gn_201501-203412.nc"
cmip6_data <- raster::readAll(raster::raster(f, varname = "intpp"))
cmip6_coords <- raster::brick(raster::raster(f, varname = "longitude"),
                              raster::raster(f, varname = "latitude"))



cmip6 <- raster::brick(cmip6_data, cmip6_coords)
usethis::use_data(cmip6, compress = "xz")

# derived from CMIP6/ssp245/intpp/intpp_Omon_MPI-ESM1-2-LR_ssp245_r1i1p1f1_gn_201501-203412.nc
# CC BY-SA 4.0

#'   \item A small extract of data and grid coordinates from CMIP 6 produced by the MPI-M.
#'     \itemize{
#'       \item Description: Primary Organic Carbon Production by All Types of Phytoplankton.
#'       \item Source: Max Planck Institute for Meteorology, Hamburg 20146, Germany (MPI-M)
#'       \item URL: https://data.ccamlr.org/dataset/small-scale-management-units
#'       \item Reference: doi:10.1029/2017MS001217
#'       \item License: CC BY-SA 4.0
#'     }

# CMIP6 model data produced by MPI-M is licensed under a Creative Commons
# Attribution ShareAlike 4.0 International License
# (https://creativecommons.org/licenses). Consult
# https://pcmdi.llnl.gov/CMIP6/TermsOfUse for terms of use governing CMIP6
# output, including citation requirements and proper acknowledgment. Further
# information about this data, including some limitations, can be found via the
# further_info_url (recorded as a global attribute in this file) and. The data
# producers and data providers make no warranty, either express or implied,
# including, but not limited to, warranties of merchantability and fitness for a
# particular purpose. All liabilities arising from the supply of the information
# (including any liability arising in negligence) are excluded to the fullest
# extent permitted by law."


