
.mkq3d <- function() {
  structure(list(vb = NULL,
                 material = list(), normals = NULL, texcoords = NULL,
                 meshColor = "vertices", ib = NULL),
            class = c("mesh3d", "shape3d"))
}

## edgesXY() removed: vertex generation now comes from textures::quad_vertex(),
## which reproduces the same ordering (x fastest, y from the top) via ydown = TRUE

#' @importFrom utils head tail
prs <- function(x) {
  cbind(head(x, -1), tail(x, -1))
}



#' Create a quad-type mesh for use in rgl.
#'
#' Convert an object to a `mesh3d` quadrangle mesh,
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
#' a _texture_ on the resulting mesh3d object. If `texture` is a palette raster it will be
#' auto-expanded to RGB.
#'
#' It is not possible to provide rgl with an object of data for texture, it must be a PNG file and so
#' the in-memory `texture` argument is written out to PNG file (with a message). The location of the file
#' may be set explicitly with `texture_filename`.
#'
#' Alternatively, `texture` may be the file path to an existing PNG image, in which case
#' no reprojection or file writing is done: the image is draped over the raster extent
#' with extent-normalized texture coordinates (the semantics of
#' [textures::quad_texture()]).
#'
#'
#' @param x raster object for mesh structure
#' @param z raster object for height values
#' @param na.rm remove quads where missing values?
#' @param ... ignored
#' @param texture optional input RGB raster (3-layers), or the file path to a PNG image
#' @param texture_filename optional input file path for PNG texture
#' @param maxcell default number of raster or terra cells to plot, with a default lowish-number - set to `NULL` to use native resolution
#' @return mesh3d
#' @export
#' @aliases dquadmesh
#' @importFrom raster extract extent values
#' @importFrom png writePNG

#' @examples
#' library(raster)
#' data(volcano)
#' r <- setExtent(raster(volcano), extent(0, 100, 0, 200))
#' qm <- quadmesh(r)
quadmesh <- function(x, z = x, na.rm = FALSE, ..., texture = NULL, texture_filename = NULL, maxcell = 50000) {
  UseMethod("quadmesh")
}

#' @name quadmesh
#' @export
quadmesh.SpatRaster <- function(x, z = x, na.rm = FALSE, ..., texture = NULL, texture_filename = NULL, maxcell = 50000) {
 if (!is.null(maxcell)) {
    sub <- ceiling(min(dim(x)[2:1]/sqrt(maxcell)))
    if (sub > 1) {
      x <- terra::aggregate(x, fact = sub)
    }
    maxcell <- NULL
 }
  if (inherits(z, "SpatRaster")) z <- raster::raster(z)
  if (inherits(texture, "SpatRaster")) texture <- raster::brick(texture)
  quadmesh(raster::raster(x), z = z, na.rm = na.rm, ..., texture = texture, texture_filename = texture_filename, maxcell = maxcell)
}
#' @name quadmesh
#' @export
quadmesh.BasicRaster <- function(x, z = x, na.rm = FALSE, ..., texture = NULL, texture_filename = NULL, maxcell = 50000) {
  x <- x[[1]]  ## just the oneth raster for now

   if (!is.null(maxcell)) {
    sub <- ceiling(min(dim(x)[2:1]/sqrt(maxcell)))
    if (sub > 1) {
      x <- raster::raster(terra::aggregate(x, fact = sub))
    }
    maxcell <- NULL
   }

  dm <- c(raster::ncol(x), raster::nrow(x))
  ## vertex and index generation from the textures package
  ## (ydown = TRUE matches the historical edgesXY() ordering: x fastest,
  ## vertex rows enumerated from the top of the raster, quads in cell order)
  exy <- textures::quad_vertex(dm, extent = c(raster::xmin(x), raster::xmax(x),
                                              raster::ymin(x), raster::ymax(x)),
                               ydown = TRUE)
  ## textures winds quads anti-clockwise for y-down grids, rotate the cycle
  ## back to the historical corner order (TL, TR, BR, BL) because
  ## qm_as_raster(index = ) gives specific corners public meaning
  ## (rotation by 2 is its own inverse)
  ind1 <- textures::quad_index(dm, ydown = TRUE)[c(3L, 4L, 1L, 2L), , drop = FALSE]

  if (na.rm) {
    ind1 <- ind1[,!is.na(values(x)), drop = FALSE]
  }
  ob <- .mkq3d()

  if (!is.null(z)) {
    ## wish of https://github.com/hypertidy/quadmesh/issues/17
    if (is.numeric(z) && !inherits(z, "BasicRaster")) {
      ## constant z as per the documentation
      z <- raster::setValues(x, z[1L])
    }
    old <- options(warn = -1)
    on.exit(options(old), add = TRUE)

    ## rather than add eps to the edgesXY coordinates
    ## let's extend the z raster a tiny bit
    z <- raster::setExtent(z, raster::extent(z) + raster::res(z)/1000)

    z <- zapsmall(extract(z, as.matrix(exy), method = "bilinear"))
    options(old)
  } else {
    z <- 0
  }
  ob$vb <- t(cbind(exy, z, 1))
  dimnames(ob$vb) <- NULL
  ob$ib <- ind1

  if (!is.null(texture) && is.character(texture)) {
    ## a PNG file path: quad_texture() semantics from the textures package,
    ## the image is draped over the whole mesh and the texture coordinates
    ## are the extent-normalized vertex coordinates (no reprojection, no
    ## PNG write step, the image is assumed to cover the raster extent)
    texture <- texture[1L]
    if (!file.exists(texture)) {
      warning(sprintf("texture file '%s' does not exist", texture))
    }
    ob$texcoords <- rbind((exy[, 1L] - raster::xmin(x)) / (raster::xmax(x) - raster::xmin(x)),
                          (exy[, 2L] - raster::ymin(x)) / (raster::ymax(x) - raster::ymin(x)))
    ob$material$texture <- texture
    ob$material$color <- "grey"
    texture <- NULL
  }

  if (!is.null(texture)) {
    if (!inherits(texture, "BasicRaster")) {
      stop("texture must be a 3-layer raster with RGB values (in 0-255)")
    }
    if (!raster::nlayers(texture) == 3L) {
      if (raster::nlayers(texture) == 1 && length(texture@legend@colortable) > 0) {
        texture <- raster::setValues(raster::brick(texture[[1]], texture[[1]], texture[[1]]),
                             t(grDevices::col2rgb(texture@legend@colortable[raster::values(texture) + 1])))
      } else {
        stop("texture must be a 3-layer raster with RGB values (in 0-255)")

      }
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
   ob$material$color <- "grey"
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
quadmesh.matrix <- function(x, z = x, na.rm = FALSE, ..., texture = NULL, texture_filename = NULL, maxcell = 50000) {
  x <- raster::setExtent(raster::raster(x), raster::extent(0, ncol(x), 0, nrow(x)))
  if (is.matrix(z)) {
    #warning("z is a matrix ...")
    z <- x
  }
  quadmesh(x, z = z, na.rm = na.rm, ..., texture = texture, texture_filename = texture_filename)
}

