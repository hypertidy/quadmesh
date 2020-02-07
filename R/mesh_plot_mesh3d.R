mesh_plot_mesh3d <-
  function (x,
            crs = NULL, col = NULL,
            add = FALSE, zlim = NULL,
                              ..., coords = NULL) {
    xx <- x$vb[1L, x$ib]
    yy <- x$vb[2L, x$ib]
    if (!is.null(x$ib)) id <- x$ib
    if (!is.null(x$it)) id <- x$it
    id <- rep(seq_len(ncol(id)), each = nrow(id))

    ## TODO: determine face or vertex colouring and
    ## expand to face (with a message that vertex not supported)
    ## if colours present, otherwise build colours from z
    cols <- viridis::viridis(100)[scales::rescale(x$vb[3L, x$ib[1,]], c(1, 100))]
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

