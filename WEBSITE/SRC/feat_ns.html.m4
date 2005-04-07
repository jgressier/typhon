include(`general.m4')
dnl -------------------------------------------------------------
set_page_title([Solver features - Navier-Stokes model])
define([id1], [feat])
define([id2], [ns])

include_header

dnl -------------------------------------------------------------

section([Gas model])
item([perfect gaz])
<br>

section([Dynamics])
item([inviscid])
<br>

section([Numerical Schemes])
item([HLLE upwind scheme])
item([MUSCL second order extension])
<br>

section([Boundary Conditions])
item([symmetry])
item([subsonic or supersonic inlet])
item([subsonic or supersonic outlet])

skip_line

dnl -------------------------------------------------------------
include_footer
