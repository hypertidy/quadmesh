#remotes::install_github("marionlouveaux/mgx2r")
filePly <- system.file("extdata", "full/mesh/mesh_meristem_full_T0.ply", package = "mgx2r", mustWork = TRUE)
fileCellGraph <- system.file("extdata",  "full/cellGraph/cellGraph_meristem_full_T0.ply", package = "mgx2r", mustWork = TRUE)
library(mgx2r)
mgx_palette <- c("#800000", "#FF0000", "#808000", "#FFFF00",
                 "#008000", "#00FF00", "#008080", "#00FFFF",
                 "#000080", "#0000FF", "#800080", "#FF00FF")

myMesh <- read_mgxPly(
  file = filePly, ShowSpecimen = FALSE, addNormals = TRUE,
  MatCol = 1, header_max = 30, my_colors = mgx_palette)


## the triangle mesh
tm <- myMesh
library(raster)
## the xy vertices of the triangles
xy <- t(tm$vb[1:2, ])
## a target grid
grid <- raster(extent(xy), nrows = 80, ncols = 80)
rxy <- sp::coordinates(grid)
## the magic barycentric index, the interpolation engine for the target grid vertices
## and where they fall in each triangle relative to triangle vertices
pid0 <- geometry::tsearch(xy[,1], xy[,2], t(tm$it), rxy[,1], rxy[, 2],
                            bary = TRUE)

ok <- !is.na(pid0$idx)
r <- setValues(grid, NA_real_)
r[ok] <- colSums(matrix(tm$vb[3, tm$it[, pid0$idx[ok]]], nrow = 3) * t(pid0$p)[, ok])
plot(r)
