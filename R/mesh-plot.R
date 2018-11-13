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
#' @param x object to convert to mesh and plot
#' @param crs target map projection
#' @param colfun colour function to use, `viridis` is the default
#' @param add add to existing plot or start a new one
#' @param ... ignored
#'
#' @return nothing, used for the side-effect of creating or adding to a plot
#' @export
#'
#' @examples
#' mesh_plot(worldll)
#' ## crop otherwise out of bounds from PROJ
#' mesh_plot(raster::crop(worldll, raster::extent(-179, 179, -89, 89)), crs = "+proj=laea")
#' mesh_plot(worldll, crs = "+proj=moll")
#' prj <- "+proj=lcc +datum=WGS84 +lon_0=147 +lat_0=-40 +lat_1=-55 +lat_2=-20"
#' mesh_plot(etopo, crs = prj, add = FALSE, colfun = function(n = 20) grey(seq(0, 1, length = n)))
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

## still not working right, triangulating the centres works but triangulating the quads makes a mush
# # @name mesh_plot
# # @export
# # @importFrom grDevices grey
# # @examples
# # f = normalizePath("~/Git/rasterwise/extdata/get1index_64/test.nc")
# # library(stars)
# # x <- read_stars(f, curvilinear = c("lon", "lat"))
# # mesh_plot(x, qtile = 56)
# # # mesh_plot(x, colfun = palr::sstPal, qtile = 67)
# # # mesh_plot(x, colfun = palr::sstPal, qtile = 67, crs = "+proj=laea +lat_0=-30")
# mesh_plot.stars <- function(x, crs = NULL, colfun = NULL, add = FALSE, ..., qtile = FALSE) {
#   if (is.null(colfun)) colfun <- function(n) grDevices::grey(seq(0, 1, length.out = n))
#   ## whoa, we might not be curvilinear
#   if (is.null(st_dimensions(x)$x$values) || is.null(st_dimensions(x)$x$values)) {
#     stop("not a curvilinear stars object")
#   }
#   if (is.null(dim(st_dimensions(x)$x$values)) || is.null(dim(st_dimensions(x)$x$values))) {
#     ## looks rectilinear
#     coords <- as.matrix(expand.grid(st_dimensions(x)$x$values, st_dimensions(x)$y$values))
#   } else {
#     ## looks curvilinear
#     coords <- cbind(as.vector(st_dimensions(x)$x$values),
#                     as.vector(st_dimensions(x)$y$values))
#   } # else
#   ## fail, we need helpers for affine grids to values ...
#
#   if (!is.null(crs)) {
#     coords <- rgdal::project(coords, crs)  ## assume forwards
#   }
#
#  # tri1 <- RTriangle::triangulate(RTriangle::pslg(P = cbind(rep(seq_len(nrow(x)), ncol(x)),
# #                                                          rep(seq_len(ncol(x)), each = nrow(x)))))
#
#   tri <- list(T = t(triangulate_quads(quadmesh(raster::raster(extent(0, ncol(x) - 1, 0, nrow(x)-1), nrows = nrow(x)-1,
#                                                    ncols = ncol(x)-1))$ib)))
#   XY <- coords[t(tri$T), ]
#   #XY <- coords[t(tri), ]
#   ID <- rep(1:nrow(tri$T), each = 3)
#   one_slice <- function(x) {
#     xx <- x[[1]]
#     ## might need de-uniting here
#     dm <- dim(xx)
#     if (length(dm) > 2) xx <- xx[,,1, drop = TRUE]
#     xx
#   }
#   ## watch out here, we need a triangle vertex to get the colours in the right order
#   vals <- as.vector(one_slice(x))
#   if (qtile > 0) {
#     if (isTRUE(qtile)) qtile <- 12
#     vals <- findInterval(vals, quantile(vals, prob = seq(0, 1, length = qtile)))
#   }
#   COL <- colfun(27)[scales::rescale(vals[tri$T[,1]], c(1, 12))]
#   isLL <- !is.null(crs) ## assume it is
#   if (!add ) {
#     plot(cbind(range(coords[,1]), range(coords[,2])), type = "n", asp = if (isLL) 1/cos(mean(coords[,2]) * pi/180) else 1,
#          xlab = "", ylab = "")
#   }
#   vps <- gridBase::baseViewports()
#   grid::pushViewport(vps$inner, vps$figure, vps$plot)
#   grid::grid.polygon(XY[,1], XY[,2], ID,  gp = grid::gpar(col = NA, fill = COL),
#                      default.units = "native")
#   grid::popViewport(3)
#   #mm <- rnaturalearth::ne_countries(scale = "medium", returnclass="sp")
#   #if (!is.null(crs)) mm <- sp::spTransform(mm, crs)
#   #sp::plot(mm, add = TRUE, border = 'firebrick')
#
# }


# coord_plot <- function(x, coords) {
#   ## triangulate the index (rather than quadmesh)
#
#   tri <- RTriangle::triangulate(RTriangle::pslg(P = cbind(raster::colFromCell(x, seq_len(raster::ncell(x))),
#                                                           raster::rowFromCell(x, seq_len(raster::ncell(x))))))
#
#
#   o <- structure(list(vb = t(cbind(coords, raster::values(x), 1)),
#             it = t(tri$T),
#             primitivetype = "triangle",
#             material = list(),
#             normals = NULL,
#             texcoords = NULL), class = c("mesh3d", "shape3d"))
#
#   rgl::open3d()
# cols <- viridis::viridis(56)[scales::rescale(o$vb[3,o$it], c(1, 56))]
#
# #print(str(cols))
#   rgl::shade3d(o, col = cols)
#   rgl::aspect3d(1, 1, .0001)
#
#   invisible(o)
# }
