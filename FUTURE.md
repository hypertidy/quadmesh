# quadmesh: future plans (post 0.6.0)

This document collects refactor and extension plans that are out of scope for the
0.6.0 CRAN release. Items are roughly ordered from "next patch" to "structural".
The big-ticket item (merging with hypertidy/textures) is parked in a dedicated
issue, summarized at the end.

(Add FUTURE.md to .Rbuildignore.)

## 1. Consolidate mesh_plot around mesh3d (issues #35, #44)

There are currently four near-copies of the plot pipeline
(vertices -> optional reprojection -> cull non-finite -> per-face colours ->
grid.polygon): in mesh_plot.RasterLayer (partially delegating, with a large
commented-out block), mesh_plot.quadmesh, mesh_plot.TRI, and mesh_plot.mesh3d.

Target shape:

- every mesh_plot method is "coerce to mesh3d, then call the one worker"
  (this is exactly issue #44: mesh_plot for xx should be quadmesh/triangmesh
  for xx),
- one internal worker takes (vb, ib-or-it, face colours, crs metadata) and does
  reprojection, culling, and grid drawing,
- colour mapping unified on palr::image_pal() everywhere (mesh_plot.mesh3d
  already uses it; the quadmesh method still uses scales::rescale indexing,
  which behaves differently and does not support 'breaks'). This would also
  let 'scales' drop out of Imports.

The crs plumbing then lives in exactly one place, and the mesh_plot.mesh3d
limitation ("crs is ignored") goes away because mesh3d objects made by this
package carry $crs.

## 2. use_crs and global state

- The option name 'crs.in.use' is not namespaced; rename to
  'quadmesh.crs.in.use' (keep reading the old name for a deprecation cycle),
  or move to a package-internal environment.
- Document the add = TRUE contract properly: first plot sets the in-use crs,
  subsequent add = TRUE calls reproject to it.
- Consider returning the previous value invisibly on set, in the style of
  options()/par(), so callers can restore.

## 3. Decouple from raster (and terra) - matrix + extent as the core

The package's real inputs are tiny: dimension, extent, crs, cell values, and
optionally curvilinear coordinate arrays. None of that needs a raster object.

Plan:

- promote a core constructor that takes plain data, e.g.
  quadmesh_core(values, dim, extent, crs = NA, coords = NULL) - essentially
  what vaster provides grid logic for, with zero dependencies,
- corner-value interpolation already exists dependency-free in vxy()
  (NA-aware corner means); the raster path instead uses raster::extract
  bilinear with an extent fudge. Unify on the matrix path after characterising
  the edge/NA behaviour differences (vxy is also much faster),
- raster/terra/stars methods become thin shims that pull (values, extent, crs)
  and call the core; raster and terra move from Imports to Suggests,
- this also kills the current double-handling where SpatRaster input is
  converted to raster (and terra is used to aggregate raster input - each
  package currently round-trips through the other).

This is the "primitives over abstractions" move and makes the eventual merge
with textures much cleaner (see parked issue).

## 4. maxcell / decimation

- current factor: ceiling(min(dim(x)[2:1] / sqrt(maxcell))) under-decimates
  anisotropic grids (e.g. 10 x 1e6 gives factor 1); use
  ceiling(sqrt(ncell(x) / maxcell)) or per-axis factors,
- aggregate() computes means, which is wasted work for a plot preview;
  nearest-neighbour subsampling of cells (and corner coordinates) is cheaper
  and dependency-free once item 3 lands,
- support maxcell consistently across methods and the extras (issue #45).

## 5. z semantics

0.6.0 fixes z being silently ignored in dquadmesh/triangmesh/dtriangmesh, but
the documented behaviour "if the coordinate system of z and x don't match the
z values are queried by reprojection" is still not implemented - extraction
assumes a shared crs. Either implement (reproj corner coordinates into the z
crs before sampling) or soften the docs. Define clearly in one place:

- quadmesh: z sampled at corners (bilinear, continuous),
- dquadmesh: z per cell, replicated to 4 corners,
- triangmesh: z per cell centre (vertices are centres),
- dtriangmesh: z per cell, mean over the split triangles.

## 6. texture handling (issue #43)

- allow texture to be a PNG filename directly (no write step),
- allow "no texcoord mapping": spread the image over the whole mesh,
- avoid the mandatory write-to-PNG when the input is already a file,
- this is the main overlap with hypertidy/textures (quad_texture) and should
  be designed there, not grown further here.

## 7. Curvilinear / coords support (issues #37, #33, #42)

coords (longitude/latitude arrays as cell values) is currently a plot-time
concept in mesh_plot; it belongs at mesh creation: quadmesh(x, coords = cl)
should produce a mesh with the geographic vertices baked in, and mesh_plot
then needs no special casing. Accept brick, SpatRaster, matrix, or data.frame
(issue #42). This is the angstroms/ROMS use case and the strongest remaining
reason this package exists independently of terra.

## 8. mesh3d hygiene (issues #41, #32)

- revisit meshColor "vertices" default vs face colouring - mesh_plot colours
  by face while the objects declare vertex colouring,
- drop the vestigial "primitivetype" property (#32),
- align with current rgl mesh3d expectations (rgl has evolved since these
  objects were laid out).

## 9. Housekeeping

- testthat: move to edition 3 (context() calls removed in 0.6.0), rename the
  placeholder test names ("multiplication works"),
- consider tools::resaveRdaFiles("data", compress = "xz") - cmip6.rda is
  825 KB and may trigger the compression NOTE on some flavours,
- audit unused internals (scl(), .ok_ll(), commented blocks flagged in
  issue #38) - keep or delete deliberately,
- vignettes: re-render and check external URLs; the README timing comparison
  uses spex/sf which should be trimmed or guarded,
- CI matrix: R-release/R-devel with both raster-installed-from-binary and
  source, given the sp removal.

## Parked: merge with hypertidy/textures

See the draft issue (to be filed after 0.6.0 is on CRAN). Short version:
quadmesh builds mesh3d from grids, textures builds mesh3d quads with texture
mapping for rgl; dquadmesh/dtriangmesh duplicate break_mesh(), and the
texture= argument here duplicates quad_texture() there. Proposed direction is
a small shared core (grid/quad topology to mesh3d, no heavy deps) with
quadmesh keeping the raster-facing API and textures keeping the rgl-facing
texture/scene helpers.
