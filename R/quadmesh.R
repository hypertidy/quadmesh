
.mkq3d <- function() {
  structure(list(vb = NULL, ib = NULL, primitivetype = "quad",
                 material = list(), normals = NULL, texcoords = NULL), .Names = c("vb",
                                                                                  "ib", "primitivetype", "material", "normals", "texcoords"), class = c("mesh3d",
                                                                                                                                                        "shape3d"))

  }

#' @importFrom raster xmin xmax ymin ymax
edgesXY <- function(x) {
  ## report to Hijmans 2015-11-06
  #extract(r, expand.grid(c(xmin(r), xmax(r)), c(ymin(r), ymax(r))), method = "bilinear")
  #[1]   NA   NA 99.5   NA
  ## remove this eps fudge once bilinear works
  eps <- rep(sqrt(.Machine$double.eps), 2L)
  xx <- seq(xmin(x), xmax(x), length = ncol(x) + 1L)
  yy <- seq(ymax(x), ymin(x), length = nrow(x) + 1L)
  xx[c(1L, length(xx))] <- xx[c(1L, length(xx))] + c(+eps[2], -eps[2])
  yy[c(1L, length(yy))] <- yy[c(1L, length(yy))] + c(-eps[1], +eps[1])
  xy <- expand.grid(x = xx,
                        y = yy)
  xy
}

#' @importFrom utils head tail
prs <- function(x) {
  cbind(head(x, -1), tail(x, -1))
}

p4 <- function(xp, nc) {
  (xp + c(0, 0, rep(nc, 2)))[c(1, 2, 4, 3)]
}


#' Create a quad-type mesh for use in rgl.
#'
#' Convert an object to a \code{\link[rgl]{mesh3d}} quadrangle mesh,
#' currently the only available method is for \code{\link[raster]{raster}}.
#'
#' The output is described as a mesh because it is a dense representation
#' of a continuous shape, in this case plane-filling quadrilaterals defined
#' by index of four of the available vertices.
#' @param x raster object for mesh structure
#' @param z raster object for height values
#' @param na.rm remove quads where missing values?
#' @return mesh3d
#' @export
#' @importFrom raster extract extent values
#' @examples
#' library(raster)
#' data(volcano)
#' r <- setExtent(raster(volcano), extent(0, 100, 0, 200))
#' qm <- quadmesh(r)
quadmesh <- function(x, z = x, na.rm = FALSE, ..., texture = NULL) {
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
  if (!is.null(z)) z <- zapsmall(extract(z, exy, method = "bilinear")) else z <- 0
  ob$vb <- t(cbind(exy, z, 1))
  ob$ib <- ind1

  if (!is.null(texture)) {
    #vertices <- exy
    ## check projection, need to handle missing ones, and assume they the same
    #eitherNA <- is.na(projection(texture)) || is.na(projection(texture))

    # if (!eitherNA || !projection(texture) == projection(x)) {
    #   if (raster::isLonLat(x)) {
    #     vertices <- vertices * pi/180
    #   }
    #   #browser()
    #     vertices <- proj4::ptransform(vertices, src.proj = projection(x), dst.proj = projection(texture))
    #     vertices <- cbind(vertices$x, vertices$y)
    #    if (raster::isLonLat(texture)) {
    #      vertices <- vertices * 180/pi
    #    }
    # }
    #browser()

    exy <- target_coordinates(exy, src.proj = raster::projection(x),
                              target = texture)



  texcoords <- texture_coordinates(texture, vertices = exy)

   ob$texcoords <- t(texcoords)
   pngfilename <- tempfile(fileext = ".png")
   message(sprintf("writing texture image to %s", pngfilename))
   png::writePNG(raster::as.array(texture) / 255, pngfilename)
   ob$texture <- pngfilename
  }
  ob
}




# quad <- function(x, z = x, na.rm = FALSE) {
#   x <- x[[1]]  ## just the oneth raster for now
#   exy <- edgesXY(x)
#   ind <- apply(prs(seq(ncol(x) + 1)), 1, p4, nc = ncol(x) + 1)
#   ## all face indexes
#   ind0 <- as.vector(ind) +
#     rep(seq(0, length = nrow(x), by = ncol(x) + 1), each = 4 * ncol(x))
#   ind1 <- matrix(ind0, nrow = 4)
#   ## need to consider normalizing vertices here
#   if (na.rm) {
#     ind1 <- ind1[,!is.na(values(x))]
#   }
#   if (!is.null(z)) z <- extract(z, exy, method = "bilinear") else z <- 0
#   v <- data.frame(x_ = exy[,1], y_ = exy[,2], vertex_ = seq(nrow(exy)))
#   print(dim(ind1))
#   b <- data.frame(vertex_ = as.vector(ind1), quad_ = rep(seq(ncol(ind1)), each = nrow(ind1)))
#   list(v = v, b = b)
# }
#

# distinct ----------------------------------------------------------------

