# quadmesh 0.2.0

* The `quadmesh` functions gains a `texture` argument, to map on a 
 3-layer RGB raster (in 0-255) via a temporary PNG file. 
 
* New function `mesh_plot` for basic vectorized raster plot with optional near near-lossless 
 projection transformation. 
 
* Index arrays are now created with integer mode.

* Added `triangulate_quads` function to convert quad index to pairs of triangle index. 

* Added supporting information to package. 

* Internal function `llh2xyz` is now exported to transform from longitude latitude coordinates to 
 geocentric xyz coordinates. 
 
# quadmesh 0.1.0

* Basic quad mesh support. 



