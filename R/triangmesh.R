#' Create a triangle-type mesh for use in rgl.
#'
#' Convert an object to a `mesh3d` ([rgl::tmesh3d()]) triangle mesh,
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
#' may be set explicitly with `texture_filename`.  Currently it's not possible to not use the `texture` object
#' in-memory.
#' @inheritParams quadmesh
#' @return mesh3d (primitivetype triangle)
#' @export
#' @aliases dtriangmesh
#' @examples
#' library(raster)
#' r <- setExtent(raster(volcano), extent(0, nrow(volcano), 0, ncol(volcano)))
#' tm <- triangmesh(r)
#' #rgl::shade3d(tm)
#'
#' ## jitter the mesh just enough to show that they are distinct in the discrete case
#' a <- dtriangmesh(r)
#' a$vb[3L, ] <- jitter(a$vb[3L, ], factor = 10)
#' ##rgl.clear(); rgl::shade3d(a, col = "grey"); aspect3d(1, 1, 0.2); rglwidget()
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

  coords <- sp::coordinates(x)

  exy <- edgesXY(x)
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
    ind1 <- ind1[,!is.na(raster::values(x))]
  }


  if (is.null(z)) z <- 0 else z <- raster::values(x)
  ob <- structure(list(it = ind1, vb = t(cbind(coords, z, 1)), primitivetype = "triangle", material = list()), class = c("mesh3d", "shape3d"))




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
    if (!inherits(z, "BasicRaster")) {
      z <- raster::raster(x)
    }
    tm$vb[3L, ] <- raster::values(z)
  }

  ## break the mesh!
  tm$vb <- tm$vb[, tm$it]
  if (!is.null(tm$texcoords)) {
    tm$texcoords <- tm$texcoords[, tm$it]

  }
  tm$it <- matrix(seq_len(ncol(tm$vb)), 3L)

  tm$vb[3, ] <- rep(apply(matrix(tm$vb[3, tm$it, drop = FALSE], 3L), 2, mean),
                    each = 3L)

  tm
}
