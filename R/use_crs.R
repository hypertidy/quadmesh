#' In-use coordinate system
#'
#' Set or return the coordinate system currently in use.
#'
#' If argument `crs` is NULL, the function returns the current value (which may be `NULL``).
#' @param crs provide PROJ string to set the value
#' @export
#' @examples
#' \dontrun{
#' use_crs()
#' use_crs("+proj=laea +datum=WGS84")
#' use_crs()
#' }
use_crs <- function(crs = NULL) {
  if (!is.null(crs)) {
    options(crs.in.use = crs)
    return(crs)
  }
  crs <- getOption("crs.in.use")
  if (is.null(crs)) warning("No crs.in.use")
  crs
}

