include(`general.m4')
dnl -------------------------------------------------------------
set_page_title([Solver features - Navier-Stokes model])
define([id1], [feat])
define([id2], [ns])

include_header

dnl -------------------------------------------------------------

section([Gas model])
item([perfect gaz])
item([viscosity: Sutherland law, constant Prandtl number])
<br>

section([Dynamics])
item([inviscid])
item([laminar viscosity])
<br>

section([Numerical Schemes])
item([HLLE upwind scheme])
item([HLLC upwind scheme])
item([MUSCL second order extension])
<br>

section([Boundary Conditions])
item([symmetry])
item([subsonic or supersonic inlet])
item([subsonic or supersonic outlet])

skip_line

dnl -------------------------------------------------------------
include_footer
