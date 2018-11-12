# .open_ncdf <- function(x) {
#   if (requireNamespace("ncdf4", quietly = TRUE)) {
#     out <- try(ncdf4::nc_open(x))
#   } else {
#     stop(sprintf("cannot open %s as a NetCDF file", x))
#   }
#   out
# }
