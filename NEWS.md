# quadmesh 0.5.5

* Fixed CRAN issues about html tag attributes, viridis unused import, and removed rgl doc links. 

# quadmesh 0.5.0

* cleaned up dependencies for CRAN policies


# quadmesh 0.4.5

* Moved large examples/ folder from inst/ to data-raw/ to avoid it being installed. 

* Remove failing test comparing unimportant differences in geocentric transformation. 

* quadmesh now works with upcoming release of reproj (> 0.4.0). 

* Fixed failing CRAN test by internalizing use of hcl.colors(). 

* `mesh_plot()` now has a method for mesh3d. Intention is
 to use mesh3d as the common basis for mesh_plot(). 
* WIP new `use_crs()` facility to set the projection in use when setting up a `mesh_plot`. Subsequent calls to `mesh_plot(, add = TRUE)` will automatically project to the coordinate system
 in use. `use_crs()` will set or get the PROJ string. 

* `mesh_plot()` now has a method for mesh3d. Intention is
 to use mesh3d as the common basis for mesh_plot(). Now using palr package for colour handling. 
 
* WIP new `use_crs()` facility to set the projection in use when setting up a `mesh_plot`. Subsequent calls to `mesh_plot(, add = TRUE)` will automatically project to the coordinate system
 in use. `use_crs()` will set or get the PROJ string. 

* New feature in `quadmesh()` to auto-expand a palette raster to the required RGB brick
 for the `texture` argument.  Motivated by 
 https://gis.stackexchange.com/questions/324552/creating-3d-image-from-hgt-files. 
 
* Simplified use of `reproj()` by relying on upcoming new version. 

* Function `mesh_plot()` now handles stars objects, only plotting the first x-y slice. 

# quadmesh 0.4.0

* Fixed bug is `mesh3d` creation, partial naming of `material$color` caused various problems. 


# quadmesh 0.3.0

* Function `mesh_plot` gains `zlim` and passes `...` to `base::plot()` (for aspect ratios with wildly disparate dwide/high grids). 

* Now using the `reproj` package for coordinate transformations. 

* New `mesh_plot()` method for `TRI` objects from silicate/anglr. 

* New function `dquadmesh` to provide the discrete interpretation of a grid. 

* New function `bary_index` to provide a basis for interpolation of a regular grid
 from triangles. 

* New functions `triangmesh()` and `dtriangmesh` to provide the triangle-interpretation of a raster (bound to  centre coordinates). 
 
* `quadmesh()` is now generic with a new method for 'matrix'. 

* Some fixes and additional features for `mesh_plot()` to allow plotting with
 curvilinear `coords`. 

* New function `qm_as_raster` to round-trip to a RasterLayer. 

* The `z` argument of `quadmesh()` may now have a different coordinate system to the 
 input data `x`.  The mesh coordinates of `x` are silently reprojected if needed and
  used to extract values in the native coordinate system of `z`. 
 
* A new `qsc` function to return a very simple six quad rendition of the Quadrilateralized Spherical Cube 
 projection family. 
 
* The `mesh_plot` function gains true curvilinear grid support by allowing input of `coords`, a two-layer
 raster of longitude and latitude values for the cell geography. This aligns with the approach used in the
 `angstroms` package. 

* The `quadmesh` function gains a new argument 'texture_filename' to control the output PNG file. 

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



