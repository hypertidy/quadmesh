scl <- function(x) {
  rg <- range(x, na.rm = TRUE);
  (x - rg[1])/diff(rg)
}

#' Plot as a mesh
#'
#' @param x object to convert to mesh and plot
#' @param crs target map projection
#' @param colfun colour function to use, `viridis` is the default
#' @param add add to existing plot or start a new one
#' @param ... ignored
#'
#' @return nothing
#' @export
#'
#' @examples
#' mesh_plot(worldll)
#'
#' mesh_plot(worldll, crs = "+proj=laea")
#' mesh_plot(worldll, crs = "+proj=moll")
#' prj <- "+proj=lcc +datum=WGS84 +lon_0=147 +lat_0=-40 +lat_1=-55 +lat_2=-20"
#' mesh_plot(etopo, crs = prj, add = TRUE, colfun = function(n = 20) grey(seq(0, 1, length = n)))
#' mesh_plot(worldll, crs = prj, add = TRUE)
mesh_plot <- function(x, crs = NULL, colfun = NULL, add = FALSE, ...) {
  UseMethod("mesh_plot")
}
#' @name mesh_plot
#' @export
mesh_plot.BasicRaster <- function(x, crs = NULL, colfun = NULL, add = FALSE, ...) {
  print("converting to single RasterLayer")
  mesh_plot(x[[1]])
}
#' @name mesh_plot
#' @export
mesh_plot.RasterLayer <- function(x, crs = NULL, colfun = NULL, add = FALSE, ...) {
  qm <- quadmesh::quadmesh(x, na.rm = FALSE)
 if (is.null(colfun)) colfun <- viridis::viridis
  ib <- qm$ib
  xy <- t(qm$vb[1:2, ])

  isLL <- raster::isLonLat(x)
  if (!is.null(crs) ) {
    if (!isLL) {
      xy <- proj4::project(xy, raster::projection(x), inv = TRUE)
    }
    if (!raster::isLonLat(crs)) xy <- proj4::project(xy, crs)
  }
  ## we have to remove any infinite vertices
  ## as this affects the entire thing
  bad <- !is.finite(xy[,1]) | !is.finite(xy[,2])
  ## but we must identify the bad xy in the index
  if (any(bad)) ib <- ib[,-which(bad)]

  xx <- xy[c(ib),1]
  yy <- xy[c(ib),2]
  ## we need a identifier grouping for each 4-vertex polygon
  id <- rep(seq_len(ncol(ib)), each  = nrow(ib))

  ## we also have to deal with any values that are NA
  ## because they propagate to destroy the id
  cols <- colfun(100)[scl(values(x)) * 99 + 1]
  if (any(is.na(cols))) {
    colsna <- rep(cols, each = nrow(ib))
    bad2 <- is.na(colsna)
    xx <- xx[!bad2]
    yy <- yy[!bad2]
    id <- id[!bad2]
    cols <- cols[!is.na(cols)]
  }
  x <- list(x = xx, y = yy, id = id, col = cols)

  if (!add) {
    graphics::plot.new()
    graphics::plot.window(xlim = range(x$x, finite = TRUE), ylim = range(x$y, finite = TRUE), asp = if (isLL) 1/cos(mean(x$y, na.rm = TRUE) * pi/180) else 1  )
  }
  vps <- gridBase::baseViewports()

  grid::pushViewport(vps$inner, vps$figure, vps$plot)


  grid::grid.polygon(x$x, x$y, x$id, gp = grid::gpar(col = NA, fill = x$col),
                     default.units = "native")


  grid::popViewport(3)
  invisible(NULL)
}

