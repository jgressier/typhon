include(`general.m4')
dnl -------------------------------------------------------------
set_page_title([Solver features])
define([id1], [feat])
define([id2], [])
define([id3], [])

include_header

dnl -------------------------------------------------------------


[
<p>Choose an item in the heading menu to obtain the associated description of the last stable release.
<p>The features history can be found in the] hyperlink([development/changelog], [dev_chlog.html])[ section.
<p>The upcoming features are proposed in the] hyperlink([development/roadmap],  [dev_rmap.html]) [ section.
]
skip_line

section([Main Features])
item([Multi-Zone solver])
item([Finite Volume Heat Transfer solver (linear, non-linear, anisotropic) (implicit)])
item([Finite Volume Compressible Flow solver (inviscid, laminar) (explicit/implicit HLLE/HLLC)])
item([Fully unstructured mesh (2d, 3d) (tri, quad, tetra, hexa, prism, pyramid, etc)])
item([CGNS input])
item([Tecplot, VTK outputs])
skip_line

section([Upcoming Features])
item([history outputs])
item([CGNS outputs])
item([Mesh refinement])

skip_line

dnl -------------------------------------------------------------
include_footer
