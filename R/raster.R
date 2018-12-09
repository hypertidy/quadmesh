#' Quadmesh to raster
#'
#' Approximate re-creation of a raster from a quadmesh.
#'
#' The raster is populated with the mean of the values at each corner, which is
#' closest to the interpretation use to create mesh3d from rasters. This can be over ridden
#' by setting 'index' to 1, 2, 3, or 4.
#' @param x 'mesh3d' object
#' @param index optional index to specify which z coordinate to use as raster values
#'
#' @return RasterLayer
#' @export
#'
#' @examples
#' qm_as_raster(quadmesh(etopo))
qm_as_raster <- function(x, index = NULL) {
  if (is.numeric(index) && (index < 1 || index > 4)) stop("index is out of range, set to NULL or 1, 2, 3, or 4")
  if (!inherits(x, "mesh3d")) stop("only mesh3d supported")
      if (is.null(index)) {
        v <- .colMeans(matrix(x$vb[3, x$ib], nrow = 4), 4, ncol(x$ib))
      } else {
        v <- x$vb[3, x$ib[index, ]]
      }
      r_meta <- .raster_meta(x)
      raster::setValues(do.call(raster::raster, r_meta),
                        v)
}


.raster_meta <- function(x) {
  if ("raster_metadata" %in% names(x)) {
    x$raster_metadata
  } else  {
    ## how to do this?
    stop("no raster metadata available")
    #list(xmn = min(x$vb[1, ], na.rm = TRUE),
    #     xmx = max(x$vb[1, ], na.rm = TRUE),
    #     ymn = min(x$vb[2, ], na.rm = TRUE),
    #     ymx = max(x$vb[2, ], na.rm = TRUE),
    #     ncols = ?)
  }
}
