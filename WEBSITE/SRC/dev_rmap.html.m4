include(`general.m4')
dnl -------------------------------------------------------------
set_page_title([Roadmap])
define([id1], [dev])
define([id2], [rmap])
define([id3], [])

include_header

dnl -------------------------------------------------------------

section([future releases])
<br>

section([core 0.2])
item([Navier-Stokes solver: Dissipative flux computation])
item([Navier-Stokes solver: Implicit solver])
item([extension of boundary conditions])
<br>

section([core 0.3])
item([AMR functions])
item([MPI communications])
<br>


skip_line

dnl -------------------------------------------------------------
include_footer
