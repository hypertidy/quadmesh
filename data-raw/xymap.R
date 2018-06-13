xymap <- do.call(cbind, maps::map(plot = FALSE)[c("x", "y")])
usethis::use_data(xymap)
