dquadmesh <- function (x, z = x, na.rm = FALSE, ...,
                       texture = NULL, texture_filename = NULL) {

  qm <- quadmesh(x, z = z, na.rm = na.rm, ...,
                  texture = texture, texture_filename = texture_filename)


  ## break the mesh!
  qm$vb <- qm$vb[, qm$ib]

  qm$ib <- matrix(seq_len(ncol(qm$vb)), nrow(qm$vb))
  ## this needs to be an option to quadmesh, it's prior to the continuous form in this sense
  ## and we shouldn't be rederiving this constant value here
  qm$vb[3, ] <- rep(colMeans(matrix(qm$vb[3,], 4)), each = 4)

  qm
}
