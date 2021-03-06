% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/quadmesh-package.R
\docType{data}
\name{cmip6}
\alias{cmip6}
\title{CMIP6 sample}
\format{
An object of class \code{RasterBrick} of dimension 220 x 256 x 3.
}
\usage{
cmip6
}
\description{
A small extract of model output and native grid ('gn') coordinates from CMIP6. Derived from
'CMIP6/ssp245/intpp/intpp_Omon_MPI-ESM1-2-LR_ssp245_r1i1p1f1_gn_201501-203412.nc'.
}
\details{
The \link{cmip6} object is a 'RasterBrick', defined by the raster package with three
layers: 'intpp', 'longitude', 'latitude'. The model data is primary organic carbon production 'intpp'.
}
\section{Source}{

\itemize{
\item A small extract of data and grid coordinates from CMIP 6 produced by the MPI-M.
\itemize{
\item Description: Primary Organic Carbon Production by All Types of Phytoplankton.
\item Source: Max Planck Institute for Meteorology, Hamburg 20146, Germany (MPI-M)
\item URL: https://data.ccamlr.org/dataset/small-scale-management-units
\item Reference: doi:10.1029/2017MS001217
\item License: CC BY-SA 4.0
}
}
}

\examples{
mesh_plot(cmip6[[1]])
}
\keyword{datasets}
