scl <- function(x) {
  rg <- range(x, na.rm = TRUE);
  (x - rg[1])/diff(rg)
}

#' Plot as a mesh
#'
#' Convert to a quadmesh and plot in efficient vectorized form using 'grid'.
#'
#' The mesh may be reprojected prior to plotting using the 'crs' argument to
#' define the target map projection in 'PROJ string' format. (There is no
#' "reproject" function for quadmesh, this is performed directly on the x-y
#' coordinates of the 'quadmesh' output). The 'colfun' argument is used to
#' generate colours which are mapped to the input object data as in 'image'.
#'
#' If `coords` is supplied, it is currently assumed to be a 2-layer `RasterBrick` with
#' longitude and latitude as the *cell values*. These are used to geographically locate
#' the resulting mesh, and will be transformed to the `crs` if that is supplied. This is
#' modelled on the approach to curvilinear grid data used in the `angstroms` package. There
#' the function [angstroms::romsmap()] and [angstroms::romscoords()] are used to separate the complicated
#' grid geometry from the grid data itself. A small fudge is applied to extend the coordinates
#' by 1 cell to avoid losing any data due to the half cell outer margin (get in touch if this causes problems!).
#'
#' @param x object to convert to mesh and plot
#' @param crs target map projection
#' @param colfun colour function to use, `viridis` is the default
#' @param add add to existing plot or start a new one
#' @param ... ignored
#' @param coords optional input raster of coordinates of each cell, see details
#' @return nothing, used for the side-effect of creating or adding to a plot
#' @export
#'
#' @examples
#' ##mesh_plot(worldll)
#' ## crop otherwise out of bounds from PROJ
#' rr <- raster::crop(worldll, raster::extent(-179, 179, -89, 89))
#' mesh_plot(rr, crs = "+proj=laea +datum=WGS84")
#' mesh_plot(worldll, crs = "+proj=moll +datum=WGS84")
#' prj <- "+proj=lcc +datum=WGS84 +lon_0=147 +lat_0=-40 +lat_1=-55 +lat_2=-20"
#' mesh_plot(etopo, crs = prj, add = FALSE, colfun = function(n = 20) grey(seq(0, 1, length = n)))
#' mesh_plot(worldll, crs = prj, add = TRUE)
mesh_plot <- function(x, crs = NULL, colfun = NULL, add = FALSE, ..., coords = NULL) {
  UseMethod("mesh_plot")
}
#' @name mesh_plot
#' @export
mesh_plot.BasicRaster <- function(x, crs = NULL, colfun = NULL, add = FALSE, ..., coords = NULL) {
  if (raster::nlayers(x) > 1L) warning("extracting single RasterLayer from multilayered input")
  mesh_plot(x[[1]], crs = crs, colfun = colfun, add = add, ..., coords = coords)
}
#debug <- TRUE
#' @name mesh_plot
#' @export
mesh_plot.RasterLayer <- function(x, crs = NULL, colfun = NULL, add = FALSE, ..., coords = NULL) {
  qm <- quadmesh::quadmesh(x, na.rm = FALSE)
 if (is.null(colfun)) colfun <- viridis::viridis
  ib <- qm$ib
  ## take the coordinates as given
  xy <- t(qm$vb[1:2, ])
  if (!is.null(coords)) {
    ## apply coordinates as provided explicitly
    ## fudge for test  (must be + res because could be any raster)
    coords_fudge <- raster::setExtent(coords, raster::extent(coords) + raster::res(coords) )
    cells <- raster::cellFromXY(coords_fudge, xy)
    xy <- raster::extract(coords_fudge, cells)
  }
  isLL <- raster::isLonLat(x) || !is.null(coords)  ## we just assume it's longlat if coords given
  if (!is.null(crs) ) {
    if (!raster::isLonLat(crs)) {
      isLL <- FALSE
    }
  }
  srcproj <- raster::projection(x)
  if (is.na(srcproj) && !is.null(crs)) {
    if (is.null(coords)) {
      stop("no projection defined on input raster, and no 'coords' provided - \n either set the CRS of the raster, or supply a two-layer 'coords' brick with longitude and latitude layers")
    } else {
      message("coords and crs provided, assuming coords is Longitude, Latitude")
      srcproj <- "+proj=longlat +datum=WGS84"
    }
  }
  xy <- target_coordinates(xy, src.proj = srcproj, target = crs, xyz = FALSE)
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
  xx <- list(x = xx, y = yy, id = id, col = cols)

  if (!add) {
    graphics::plot.new()
    graphics::plot.window(xlim = range(xx$x, finite = TRUE), ylim = range(xx$y, finite = TRUE),
                          asp = if (isLL) 1/cos(mean(xx$y, na.rm = TRUE) * pi/180) else 1  )
  }
  vps <- gridBase::baseViewports()

  grid::pushViewport(vps$inner, vps$figure, vps$plot)


  grid::grid.polygon(xx$x, xx$y, xx$id, gp = grid::gpar(col = NA, fill = xx$col),
                     default.units = "native")


  grid::popViewport(3)
  #if (debug) return(xx)

  invisible(NULL)
}



#' @name mesh_plot
#' @export
mesh_plot.TRI <- function(x, crs = NULL, colfun = NULL, add = FALSE, ..., coords = NULL) {
  if (is.null(colfun)) colfun <- viridis::viridis
  idx <- matrix(match(t(as.matrix(x$triangle[c(".vx0", ".vx1", ".vx2")])),
                     x$vertex$vertex_), nrow = 3)
  ## take the coordinates as given
  xy <- as.matrix(x$vertex[c("x_", "y_")])
  if (!is.null(coords)) {
   warning("coords given but will be ignored")
  }
  srcproj <- x$meta$proj[1]
  xy <- target_coordinates(xy, src.proj = srcproj, target = crs, xyz = FALSE)
  ## we have to remove any infinite vertices
  ## as this affects the entire thing
  bad <- !is.finite(xy[,1]) | !is.finite(xy[,2])
  ## we need a identifier grouping for each 3-vertex polygon
  id <- rep(seq_len(dim(idx)[2L]), each = 3)


  ## but we must identify the bad xy in the index
  if (any(bad)) idx <- idx[,-which(bad)]

  xx <- xy[c(idx),1]
  yy <- xy[c(idx),2]

  ## we also have to deal with any values that are NA
  ## because they propagate to destroy the id
  #browser()
  cols <- colfun(nrow(x$object))[factor(x$triangle$object_)]
  if (any(is.na(cols))) {
    colsna <- rep(cols, each = nrow(idx))
    bad2 <- is.na(colsna)
    xx <- xx[!bad2]
    yy <- yy[!bad2]
    id <- id[!bad2]
    cols <- cols[!is.na(cols)]
  }

  xx <- list(x = xx, y = yy, id = id, col = cols)

isLL <- FALSE
  if (!add) {
    graphics::plot.new()
    graphics::plot.window(xlim = range(xx$x, finite = TRUE), ylim = range(xx$y, finite = TRUE),
                          asp = if (isLL) 1/cos(mean(xx$y, na.rm = TRUE) * pi/180) else 1  )
  }
  vps <- gridBase::baseViewports()

  grid::pushViewport(vps$inner, vps$figure, vps$plot)
 grid::grid.polygon(xx$x, xx$y, xx$id, gp = grid::gpar(col = NA, fill = xx$col),
                     default.units = "native")
  grid::popViewport(3)
#  if (debug) return(xx)
  invisible(NULL)
}



#debug <- TRUE
#' @name mesh_plot
#' @export
mesh_plot.quadmesh <- function(x, crs = NULL, colfun = NULL, add = FALSE, ..., coords = NULL) {
  qm <- x
  if (is.null(colfun)) colfun <- viridis::viridis
  ib <- qm$ib
  ## take the coordinates as given
  xy <- t(qm$vb[1:2, ])
  if (!is.null(coords)) {
    ## apply coordinates as provided explicitly
    ## fudge for test  (must be + res because could be any raster)
    coords_fudge <- raster::setExtent(coords, raster::extent(coords) + raster::res(coords) )
    cells <- raster::cellFromXY(coords_fudge, xy)
    xy <- raster::extract(coords_fudge, cells)
  }
  isLL <- raster::isLonLat(x) || !is.null(coords)  ## we just assume it's longlat if coords given
  if (!is.null(crs) ) {
    if (!raster::isLonLat(crs)) {
      isLL <- FALSE
    }
  }
  srcproj <- raster::projection(x)
  if (is.na(srcproj) && !is.null(crs)) {
    if (is.null(coords)) {
      stop("no projection defined on input raster, and no 'coords' provided - \n either set the CRS of the raster, or supply a two-layer 'coords' brick with longitude and latitude layers")
    } else {
      message("coords and crs provided, assuming coords is Longitude, Latitude")
      srcproj <- "+proj=longlat +datum=WGS84"
    }
  }
  xy <- target_coordinates(xy, src.proj = srcproj, target = crs, xyz = FALSE)
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
  valx <- colMeans(matrix(qm$vb[3, qm$ib], nrow = 4L))
  cols <- colfun(100)[scl(valx) * 99 + 1]
  if (any(is.na(cols))) {
    colsna <- rep(cols, each = nrow(ib))
    bad2 <- is.na(colsna)
    xx <- xx[!bad2]
    yy <- yy[!bad2]
    id <- id[!bad2]
    cols <- cols[!is.na(cols)]
  }
  xx <- list(x = xx, y = yy, id = id, col = cols)

  if (!add) {
    graphics::plot.new()
    graphics::plot.window(xlim = range(xx$x, finite = TRUE), ylim = range(xx$y, finite = TRUE),
                          asp = if (isLL) 1/cos(mean(xx$y, na.rm = TRUE) * pi/180) else 1  )
  }
  vps <- gridBase::baseViewports()

  grid::pushViewport(vps$inner, vps$figure, vps$plot)


  grid::grid.polygon(xx$x, xx$y, xx$id, gp = grid::gpar(col = NA, fill = xx$col),
                     default.units = "native")


  grid::popViewport(3)
  #if (debug) return(xx)

  invisible(NULL)
}
