#' Create a triangle-type mesh for use in rgl.
#'
#' Convert an object to a `mesh3d` (of rgl package) triangle mesh,
#' with methods for [raster::raster()] and `matrix`.
#'
#' `triangmesh()` generates the point-based interpretation of a raster (POINT) with the obvious continuous
#' interpretation. `dtriangmesh` splits the mesh so that each primitive is independent.  This is more coherent
#' than the analogous distinction for quadmesh, though both will appear the same on creation.
#'
#' The output is described as a mesh because it is a dense representation
#' of a continuous shape, in this case plane-filling triangles defined
#' by index of three of the available vertices.
#'
#' The `z` argument defaults to the input `x` argument, though may be set to `NULL`, a constant
#' numeric value, or another raster. If the coordinate system of `z` and `x` don't match the z values
#' are queried by reprojection.
#'
#' Any raster RGB object (3-layers, ranging in 0-255) may be used as
#' a _texture_ on the resulting mesh3d object.
#' It is not possible to provide rgl with an object of data for texture, it must be a PNG file and so
#' the in-memory `texture` argument is written out to PNG file (with a message). The location of the file
#' may be set explicitly with `texture_filename`. Alternatively, `texture` may be the
#' file path to an existing PNG image, draped over the raster extent (see [quadmesh()]).
#' @inheritParams quadmesh
#' @return mesh3d (primitive type triangle)
#' @export
#' @aliases dtriangmesh
#' @examples
#' library(raster)
#' r <- setExtent(raster(volcano), extent(0, nrow(volcano), 0, ncol(volcano)))
#' tm <- triangmesh(r)
#'
#' ## jitter the mesh just enough to show that they are distinct in the discrete case
#' a <- dtriangmesh(r)
#' a$vb[3L, ] <- jitter(a$vb[3L, ], factor = 10)
triangmesh <- function (x, z = x, na.rm = FALSE, ..., texture = NULL, texture_filename = NULL) {
  UseMethod("triangmesh")
}
#' @name triangmesh
#' @export
triangmesh.matrix <- function (x, z = x, na.rm = FALSE, ..., texture = NULL, texture_filename = NULL)  {
  x <- raster::setExtent(raster::raster(x), raster::extent(0, ncol(x), 0, nrow(x)))
  if (is.matrix(z)) {
    #warning("z is a matrix ...")
    z <- x
  }
  triangmesh(x, z = z, na.rm = na.rm, ..., texture = texture, texture_filename = texture_filename)
}

#' @name triangmesh
#' @export
triangmesh.BasicRaster <- function (x, z = x, na.rm = FALSE, ..., texture = NULL, texture_filename = NULL)  {
  x <- x[[1]]  ## just the oneth raster for now

  coords <- raster::xyFromCell(x, seq_len(raster::ncell(x)))

  nc1 <- ncol(x) + 1
  aa <- rbind(t(prs(seq_len(ncol(x)))),
              seq_len(ncol(x)-1) + ncol(x))
  bb <- rbind(seq_len(ncol(x) - 1) + 1, t(prs(seq_len(ncol(x)) + ncol(x))))
  ind <- cbind(aa, bb)  ## TODO interleave these so they paired in the expected order
  ## all face indexes
  ind0 <- as.integer(as.vector(ind) +
                       rep(seq(0, length.out = nrow(x) - 1, by = ncol(x)), each = length(ind)))
  ind1 <- matrix(ind0, nrow = 3)

  if (na.rm) {
    ## drop triangles that reference any missing vertex (vertices are cells here),
    ## note that triangle count 2 * (nc-1) * (nr-1) differs from cell count so we
    ## cannot index columns by the cell logical directly
    bad <- is.na(raster::values(x))
    ind1 <- ind1[, colSums(matrix(bad[ind1], nrow = 3L)) == 0L, drop = FALSE]
  }


  if (is.null(z)) {
    z <- 0
  } else if (inherits(z, "SpatRaster") || inherits(z, "BasicRaster")) {
    if (inherits(z, "SpatRaster")) z <- raster::raster(z)
    z <- z[[1L]]
    if (isTRUE(all.equal(raster::extent(z), raster::extent(x))) &&
        all(dim(z)[1:2] == dim(x)[1:2])) {
      z <- raster::values(z)
    } else {
      z <- raster::extract(z, coords, method = "bilinear")
    }
  } else if (is.numeric(z)) {
    z <- rep(z[1L], nrow(coords))
  } else {
    z <- raster::values(x)
  }
  ob <- structure(list( vb = t(cbind(coords, z, 1)),  material = list(), texcoords = NULL, meshColor = "vertices", it = ind1),
                  class = c("mesh3d", "shape3d"))
  dimnames(ob$vb) <- NULL

  if (!is.null(texture) && is.character(texture)) {
    ## a PNG file path: quad_texture() semantics from the textures package,
    ## the image is draped over the raster extent and the texture coordinates
    ## are the extent-normalized vertex coordinates (no reprojection, no
    ## PNG write step)
    texture <- texture[1L]
    if (!file.exists(texture)) {
      warning(sprintf("texture file '%s' does not exist", texture))
    }
    ob$texcoords <- rbind((coords[, 1L] - raster::xmin(x)) / (raster::xmax(x) - raster::xmin(x)),
                          (coords[, 2L] - raster::ymin(x)) / (raster::ymax(x) - raster::ymin(x)))
    ob$material$texture <- texture
    ob$material$color <- "grey"
    texture <- NULL
  }

  if (!is.null(texture)) {
    if (!inherits(texture, "BasicRaster")) {
      stop("texture must be a 3-layer raster with RGB values (in 0-255)")
    }
    if (!raster::nlayers(texture) == 3L) {
      stop("texture must be a 3-layer raster with RGB values (in 0-255)")
    }

    xy <- target_coordinates(t(ob$vb[1:2, ]), src.proj = raster::projection(x),
                              target = texture)



    texcoords <- texture_coordinates(texture, vertices = xy)

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
  class(ob) <- c("triangmesh", "mesh3d", "shape3d")
  ob
}
#' @name triangmesh
#' @export
dtriangmesh <- function (x, z = x, na.rm = FALSE, ..., texture = NULL, texture_filename = NULL) {
  UseMethod("dtriangmesh")
}
#' @name triangmesh
#' @export
dtriangmesh.default <- function (x, z = x, na.rm = FALSE, ..., texture = NULL, texture_filename = NULL) {
  tm <- triangmesh(x, z = NULL, na.rm = na.rm, ..., texture = texture, texture_filename = texture_filename)

  if (!is.null(z)) {
    if (inherits(z, "SpatRaster")) z <- raster::raster(z)
    if (!inherits(z, "BasicRaster")) {
      z <- raster::raster(x)
    }
    tm$vb[3L, ] <- raster::values(z[[1L]])
  }

  ## break the mesh (expands vertices, and texcoords when present)
  tm <- textures::break_mesh(tm)

  ## discrete interpretation: constant z per triangle (mean of its vertices)
  tm$vb[3, ] <- rep(.colMeans(matrix(tm$vb[3, ], 3L), 3L, ncol(tm$it)),
                    each = 3L)

  tm
}
