#' Title
#'
#' @return
#' @export
#'
#' @examples
#' rgl.clear()
#' wire3d(qsc())
#' rglwidget()
qsc <- function() {
 radius <- 6378137
 xy <- rbind(c(-radius, -radius, radius, radius),
             c(-radius, radius, radius, -radius))
vb <-     rbind(cbind(rbind(xy, -radius), rbind(xy, radius)),
                1)
 ## bottom, front, back, left, right, top - guide oneself with text3d(t(vb), texts = 1:ncol(vb))
  ib <- cbind(c(1, 2, 3, 4),
              c(1, 5, 8, 4),
              c(2, 6, 7, 3),
              c(1, 2, 6, 5),
              c(3, 7, 8, 4),
              c(5, 6, 7, 8))
  structure(list(vb = vb, ib = ib,
                 primitivetype = "quad",
                 material = list()), class = c("mesh3d", "shape3d"))
}

