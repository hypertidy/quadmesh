#' Plot mesh
#'
#' Plot mesh
#'
#' If 'color' is present on the object it is used. This can be overridden by
#' using the 'col' argument, and controlled with 'zlim' and 'breaks' in the usual
#' [graphics::image()] way.
#' @name mesh_plot
#' @param prefer_quad set to `TRUE` by default, if but may be `FALSE` to assume use of triangle rather than quad primitives -
#' this covers the case for when a mesh3d object may have quads *and* triangles in the same mesh
#' @param breaks argument passed along to [palr::image_pal()]
#' @export
mesh_plot.mesh3d <-
  function (x,
            crs = NULL, col = NULL,
            add = FALSE, zlim = NULL,
            ..., coords = NULL, prefer_quad = TRUE, breaks = NULL) {
    ## handle args, some temporary while figuring out 2020-03-20
    if (!is.null(crs)) warning("'crs' is ignored") ## for nowhttps://github.com/hypertidy/quadmesh/issues/35#issuecomment-601596052
    if (!is.null(coords)) warning("'coords' is ignored")

    if (prefer_quad) {
      prim_name <- c("ib", "it")
    } else {
      prim_name <- c("it", "ib")
    }
    ## define the second one first
    id <- x[[prim_name[2L]]]
    if (!is.null(x[[prim_name[1L]]])) {
      ## but replace it with the preference
      id <- x[[prim_name[1L]]]
    }
    if (is.null(id)) stop("no primitives index array found ('it' or 'ib')")

    ## determine colour mapping to primitives
    ## TODO: determine face or vertex colouring and
    ## expand to face (with a message that vertex not supported)
    ## if colours present, otherwise build colours from z
    if (is.null(col)) {
      ## get colours from material if present (only face will work atm)
      cols <- x$material$color[id[1L, ]]
      if (is.null(cols)) {
        ## don't pass in col = NULL (because test is for missing())
        cols <- palr::image_pal(x$vb[3L, id[1L, ]], breaks = breaks, zlim = zlim)
      }
    } else {
      cols <- palr::image_pal(x$vb[3L, id[1L, ]], col = col, breaks = breaks, zlim = zlim)
    }


    xx <- x$vb[1L, id]
    yy <- x$vb[2L, id]

    ## column of primitives array, expanded
    ID <- rep(seq_len(ncol(id)), each = nrow(id))
    xx <- list(x = xx, y = yy, id = ID, col = cols)

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


