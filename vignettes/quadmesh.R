## ------------------------------------------------------------------------
library(quadmesh)
library(raster)
library(rglwidget)
data(volcano)
r <- setExtent(raster(volcano), extent(0, 100, 0, 200))


qm <- quadmesh(r)

library(rgl)
scl <- function(x) (x - min(x))/diff(range(x))
shade3d(qm, col = grey(scl(qm$vb[3,qm$ib])))
subid <- currentSubscene3d()
rglwidget(elementId="quadmesh001")

## ------------------------------------------------------------------------
qm <- quadmesh(r)

qm$vb[1,] <- qm$vb[1,] * qm$vb[2,]/54
open3d()
shade3d(qm, col = grey(scl(qm$vb[3,qm$ib])))
subid <- currentSubscene3d()
rglwidget(elementId="quadmesh002")

