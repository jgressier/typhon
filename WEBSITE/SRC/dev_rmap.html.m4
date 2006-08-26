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

section([core 0.3])
item([MPI communications: automatic mesh splitting and parallel computations])
item([Field initialization and boundary conditions with symbolic functions])
<br>

section([core 0.4])
item([AMR functions])
item([Multigrid computations])
<br>


skip_line

dnl -------------------------------------------------------------
include_footer
