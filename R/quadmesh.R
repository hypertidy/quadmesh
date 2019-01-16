
.mkq3d <- function() {
  structure(list(vb = NULL, ib = NULL, primitivetype = "quad",
                 material = list(), normals = NULL, texcoords = NULL), .Names = c("vb",
                                                                                  "ib", "primitivetype", "material", "normals", "texcoords"), class = c("mesh3d",
                                                                                                                                                        "shape3d"))

  }

#' @importFrom raster xmin xmax ymin ymax
edgesXY <- function(x) {
  xx <- seq(xmin(x), xmax(x), length = ncol(x) + 1L)
  yy <- seq(ymax(x), ymin(x), length = nrow(x) + 1L)
  xy <- expand.grid(x = xx, y = yy)
  xy
}

#' @importFrom utils head tail
prs <- function(x) {
  cbind(head(x, -1), tail(x, -1))
}

## what was this for ...
# p4 <- function(xp, nc) {
#   (xp + c(0, 0, rep(nc, 2)))[c(1, 2, 4, 3)]
# }



#' Create a quad-type mesh for use in rgl.
#'
#' Convert an object to a `mesh3d` ([rgl::qmesh3d()]) quadrangle mesh,
#' with methods for [raster::raster()] and `matrix`.
#'
#' `quadmesh()` generates the cell-based interpretation of a raster (AREA) but applies a continuous
#' interpretation of the values of the cells to each quad corner. `dquadmesh` splits the mesh and
#' applies a discrete interpretation directly. Loosely, the quadmesh is a continuous surface and the dquadmesh
#' is free-floating cells, but it's a little more complicated and depends on the options applied. (The interpolation)
#' applied in the quadmesh case is not entirely consistent.
#'
#' The output is described as a mesh because it is a dense representation
#' of a continuous shape, in this case plane-filling quadrilaterals defined
#' by index of four of the available vertices.
#'
#' The `z` argument defaults to the input `x` argument, though may be set to `NULL`, a constant
#' numeric value, or another raster. If the coordinate system of `z` and `x` don't match the z values
#' are queried by reprojection.
#'
#' Any raster RGB object (3-layers, ranging in 0-255) may be used as
#' a _texture_ on the resulting mesh3d object.
#' It is not possible to provide rgl with an object of data for texture, it must be a PNG file and so
#' the in-memory `texture` argument is written out to PNG file (with a message). The location of the file
#' may be set explicitly with `texture_filename`.  Currently it's not possible to not use the `texture` object
#' in-memory.
#'
#'
#' @param x raster object for mesh structure
#' @param z raster object for height values
#' @param na.rm remove quads where missing values?
#' @param ... ignored
#' @param texture optional input RGB raster, 3-layers
#' @param texture_filename optional input file path for PNG texture
#' @return mesh3d
#' @export
#' @aliases dquadmesh
#' @importFrom raster extract extent values
#' @importFrom png writePNG
#' @importFrom sp SpatialPoints CRS
#' @examples
#' library(raster)
#' data(volcano)
#' r <- setExtent(raster(volcano), extent(0, 100, 0, 200))
#' qm <- quadmesh(r)
quadmesh <- function(x, z = x, na.rm = FALSE, ..., texture = NULL, texture_filename = NULL) {
  UseMethod("quadmesh")
}

#' @name quadmesh
#' @export
quadmesh.BasicRaster <- function(x, z = x, na.rm = FALSE, ..., texture = NULL, texture_filename = NULL) {
  x <- x[[1]]  ## just the oneth raster for now
  exy <- edgesXY(x)
 # ind <- apply(prs(seq(ncol(x) + 1)), 1, p4, nc = ncol(x) + 1)
  nc1 <- ncol(x) + 1
  aa <- t(prs(seq(ncol(x) + 1)))
  ind <- matrix(c(rbind(aa, aa[2:1, ])) + c(0, 0, nc1, nc1), 4)
#  ind <- matrix(unlist(purrr::map(split(aa, rep(seq(1, ncol(aa)), each = 2)), p4, nc = ncol(x) + 1)), 4)
  ## all face indexes
  ind0 <- as.integer(as.vector(ind) +
    rep(seq(0, length = nrow(x), by = ncol(x) + 1), each = 4 * ncol(x)))
  ind1 <- matrix(ind0, nrow = 4)

  if (na.rm) {
    ind1 <- ind1[,!is.na(values(x))]
  }
  ob <- .mkq3d()

  if (!is.null(z)) {
    ## wish of https://github.com/hypertidy/quadmesh/issues/17
    sp_exy <- sp::SpatialPoints(exy, proj4string = sp::CRS(raster::projection(x), doCheckCRSArgs = FALSE))
    old <- options(warn = -1)

    ## rather than add eps to the edgesXY coordinates
    ## let's extend the z raster a tiny bit
    z <- raster::setExtent(z, raster::extent(z) + raster::res(z)/1000)

    z <- zapsmall(extract(z, sp_exy, method = "bilinear"))
    options(old)
  } else {
    z <- 0
  }
  ob$vb <- t(cbind(exy, z, 1))
  ob$ib <- ind1

  if (!is.null(texture)) {
    if (!inherits(texture, "BasicRaster")) {
      stop("texture must be a 3-layer raster with RGB values (in 0-255)")
    }
    if (!raster::nlayers(texture) == 3L) {
      stop("texture must be a 3-layer raster with RGB values (in 0-255)")
    }

    exy <- target_coordinates(exy, src.proj = raster::projection(x),
                              target = texture)



  texcoords <- texture_coordinates(texture, vertices = exy)

   ob$texcoords <- t(texcoords)
   if (is.null(texture_filename)) {
     texture_filename <- tempfile(fileext = ".png")
   } else {
     if (!grepl("png$", texture_filename)) {
       warning(sprintf("'texture filename' does not look like a good PNG filename'%s'",
                       texture_filename))
     }
   }
      message(sprintf("writing texture image to %s", texture_filename))
   png::writePNG(raster::as.array(texture) / 255, texture_filename)
   ob$material$texture <- texture_filename
   ob$material$col <- "grey"
  }
  rproj <- raster::projection(x)
  ## chuck on the original structure md
  ob$raster_metadata <- list(xmn = raster::xmin(x),
                             xmx = raster::xmax(x),
                             ymn = raster::ymin(x),
                             ymx = raster::ymax(x),
                             ncols = raster::ncol(x),
                             nrows = raster::nrow(x),
                             crs = rproj)
  ob$crs <- rproj
  class(ob) <- c("quadmesh", "mesh3d", "shape3d")
  ob
}

#' @name quadmesh
#' @export
quadmesh.matrix <- function(x, z = x, na.rm = FALSE, ..., texture = NULL, texture_filename = NULL) {
  x <- raster::setExtent(raster::raster(x), raster::extent(0, ncol(x), 0, nrow(x)))
  if (is.matrix(z)) {
    #warning("z is a matrix ...")
    z <- x
  }
  quadmesh(x, z = z, na.rm = na.rm, ..., texture = texture, texture_filename = texture_filename)
}

