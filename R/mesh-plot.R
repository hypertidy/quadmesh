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
#' coordinates of the 'quadmesh' output). The 'col' argument are mapped to the input pplied
#' object data as in 'image', and applied relative to 'zlim' if su.
#'
#' If `coords` is supplied, it is currently assumed to be a 2-layer `RasterBrick` with
#' longitude and latitude as the *cell values*. These are used to geographically locate
#' the resulting mesh, and will be transformed to the `crs` if that is supplied. This is
#' modelled on the approach to curvilinear grid data used in the `angstroms` package. There
#' the function `angstroms::romsmap()` and `angstroms::romscoords()`` are used to separate the complicated
#' grid geometry from the grid data itself. A small fudge is applied to extend the coordinates
#' by 1 cell to avoid losing any data due to the half cell outer margin (get in touch if this causes problems!).
#'
#' @param x object to convert to mesh and plot
#' @param crs target map projection
#' @param col colours to use, defaults to that used by [graphics::image()]
#' @param add add to existing plot or start a new one
#' @param zlim absolute range of data to use for colour scaling (if `NULL` the data range is used)
#' @param ... passed through to `base::plot`
#' @param coords optional input raster of coordinates of each cell, see details
#' @return nothing, used for the side-effect of creating or adding to a plot
#' @export
#' @importFrom scales rescale
#' @importFrom grDevices hcl.colors colorRampPalette
#' @examples
#' ##mesh_plot(worldll)
#' ## crop otherwise out of bounds from PROJ
#' rr <- raster::crop(worldll, raster::extent(-179, 179, -89, 89))
#' mesh_plot(rr, crs = "+proj=laea +datum=WGS84")
#' mesh_plot(worldll, crs = "+proj=moll +datum=WGS84")
#' prj <- "+proj=lcc +datum=WGS84 +lon_0=147 +lat_0=-40 +lat_1=-55 +lat_2=-20"
#' mesh_plot(etopo, crs = prj, add = FALSE, col = grey(seq(0, 1, length = 20)))
#' mesh_plot(rr, crs = prj, add = TRUE)
mesh_plot <- function(x, crs = NULL, col = NULL, add = FALSE, zlim = NULL, ..., coords = NULL) {
  if ("colfun" %in% names(list(...))) {
    stop("argument colfun is defunct, please use 'col' as per base plot")
  }
  UseMethod("mesh_plot")
}
#' @name mesh_plot
#' @export
mesh_plot.BasicRaster <- function(x, crs = NULL, col = NULL, add = FALSE, zlim = NULL, ..., coords = NULL) {
  if (raster::nlayers(x) > 1L) warning("extracting single RasterLayer from multilayered input")
  mesh_plot(x[[1L]], crs = crs, col = col, add = add, zlim = zlim, ..., coords = coords)
}
#debug <- TRUE
#' @name mesh_plot
#' @export
mesh_plot.RasterLayer <- function(x, crs = NULL, col = NULL, add = FALSE, zlim = NULL, ..., coords = NULL) {

  crs_wasnull <- FALSE
  if (is.null(crs)) crs_wasnull <- TRUE
  if (add && is.null(crs)) crs <- use_crs()
  if (!is.null(crs)) use_crs(crs) else use_crs(raster::projection(x))

  qm <- quadmesh::quadmesh(x, na.rm = FALSE)
  if (!is.null(coords) && is.na(qm$crs)) {
    ## we need this otherwise no reprojection is done in the case where coords and crs are both given
    qm$crs <- "+proj=longlat +datum=WGS84"  ## assume
  }
  if (is.null(col)) col <- hcl.colors(12, "YlOrRd",
                                      rev = TRUE)
  ib <- qm$ib
  ## take the coordinates as given
  xy <- t(qm$vb[1:2, ])
  if (!is.null(coords)) {
    ## apply coordinates as provided explicitly
    ## fudge for test  (must be + res because could be any raster)
    coords_fudge <- raster::setExtent(coords, raster::extent(coords) + raster::res(coords) )
    cells <- raster::cellFromXY(coords_fudge, xy)
    xy <- raster::extract(coords_fudge, cells)
    if (!crs_wasnull && !.ok_ll(xy)) warning("'coords' do not look like longlat, so 'crs' arg won't work\n please see Details in '?quadmesh'")

  }

  if (!is.na(use_crs()) && !is.na(qm$crs)) {

   xy <- reproj::reproj(xy, target = use_crs(), source = qm$crs)
  }
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
  lc <- length(col)
  cols <- if (is.null(zlim)) col[scales::rescale(values(x), c(1, lc))] else col[scales::rescale(values(x), c(1, lc), zlim)]
  if (any(is.na(cols))) {
    colsna <- rep(cols, each = nrow(ib))
    bad2 <- is.na(colsna)
    xx <- xx[!bad2]
    yy <- yy[!bad2]
    id <- id[!bad2]
    cols <- cols[!is.na(cols)]
  }

  xx <- list(x = xx, y = yy, id = id, col = cols)
## if (isLL) 1/cos(mean(xx$y, na.rm = TRUE) * pi/180) else 1
  if (!add) {
    graphics::plot.new()
    graphics::plot.window(xlim = range(xx$x, finite = TRUE), ylim = range(xx$y, finite = TRUE),
                          ...)
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
mesh_plot.stars <- function(x,  crs = NULL, col = NULL, add = FALSE, zlim = NULL, ..., coords = NULL) {

  if (add && is.null(crs)) crs <- use_crs()
  if (!is.null(crs)) use_crs(crs) else use_crs(raster::projection(x))

   attr(x[[1]], "units") <- NULL
  class(x[[1]]) <- "array"
  dm <- dim(x[[1]])
  if (length(dm) == 2) r <- raster::raster(t(x[[1]]))
  if (length(dm) == 3) r <- raster::raster(t(x[[1]][,,1L, drop = TRUE]))
  if (length(dm) == 4) r <- raster::raster(t(x[[1]][,,1L, 1L, drop = TRUE]))
  if (length(dm) == 5) r <- raster::raster(t(x[[1]][,,1L, 1L, 1L, drop = TRUE]))
   ## etc, do your own slicing ...


  #r <- raster::setExtent(r, raster::extent(0, ncol(r), 0, nrow(r)))
  if (is.null(coords) ) {
    dims <- attr(x, "dimensions")
    #r <- raster::setExtent(r, raster::extent(0, ncol(r), 0, nrow(r)))
    if (attr(attr(x, "dimensions"), "raster")$curvilinear) {
      coords <- raster::t(raster::setExtent(raster::brick(raster::raster(dims[[1]]$values), raster::raster(dims[[2]]$values)),
                             raster::extent(r)))
    } else {


      #dims[[1]]
      offs <- c(dims[[1]]$offset, dims[[2]]$offset)
      del <- c(dims[[1]]$delta, dims[[2]]$delta)
      if (any(is.na(offs))) {
        ## rectilinear case
        X <- if (is.na(offs[1])) dims[[1]]$values else seq(offs[1], by = del[1], length.out = ncol(r))
        Y <- if (is.na(offs[2])) dims[[2]]$values else sort(seq(offs[2], by = del[2], length.out = nrow(r)))
#        if (flip_y) Y <- rev(Y)
        xy <- expand.grid(X, Y)
        coords <- raster::setValues(raster::brick(raster::raster(r), raster::raster(r)),
                                    as.matrix(xy))

      } else {
        ## regular case
      ylim <- sort(c(offs[2], offs[2] + nrow(r) * del[2]))
      ext <- raster::extent(offs[1], offs[1] + ncol(r) * del[1],
                            ylim[1], ylim[2])
      raster::projection(r) <- dims[[1]]$refsys
      r <- raster::setExtent(r, ext)
      }
    }
  }
 # browser()
  mesh_plot(r, crs = crs, coords = coords, col = col, add = add, zlim = zlim, ...)
}

#' @name mesh_plot
#' @export
mesh_plot.TRI <- function(x, crs = NULL, col = NULL, add = FALSE, zlim = NULL, ..., coords = NULL) {

  if (add && is.null(crs)) crs <- use_crs()
  if (!is.null(crs)) use_crs(crs) else use_crs(raster::projection(x))

   if (is.null(col)) col <- grDevices::hcl.colors(12, "YlOrRd",
                                      rev = TRUE)
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
  cols <- colorRampPalette(col)(nrow(x$object))[factor(x$triangle$object_)]
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
mesh_plot.quadmesh <- function(x, crs = NULL, col = NULL, add = FALSE, zlim = NULL, ..., coords = NULL) {

  srcproj <- x$crs
  if (add && is.null(crs)) crs <- use_crs()
  if (!is.null(crs)) use_crs(crs) else use_crs(srcproj)


   qm <- x
  if (is.null(col)) col <- hcl.colors(12, "YlOrRd",
                                      rev = TRUE)
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


  lc <- length(col)
  cols <- if (is.null(zlim)) col[scales::rescale(valx, c(1, lc))] else col[scales::rescale(valx, c(1, lc), zlim)]

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
