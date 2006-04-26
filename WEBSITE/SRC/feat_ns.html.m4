include(`general.m4')
dnl -------------------------------------------------------------
set_page_title([Solver features - Navier-Stokes model])
define([id1], [feat])
define([id2], [ns])
define([id3], [])

include_header

dnl -------------------------------------------------------------

section([Gas model])
item([perfect gaz])
item([viscosity: Sutherland law, constant Prandtl number])
item([viscosity: Constant dynamic viscosity, constant Prandtl number])
<br>

section([Dynamics])
item([inviscid])
item([laminar viscosity])
<br>

section([Numerical Schemes])
item([HLLE upwind scheme])
item([HLLC upwind scheme (cf Toro, Batten)])
item([AUSM-M upwind scheme (cf Liou)])
item([MUSCL second order extension (MUSCL or FAST-MUSCL for high quality grids)])
item([MUSCL limiters: Minmod, van Albada, van Leer, Superbee, Kim (3rd order)])
item([Linear Implicit resolution (BiCG-Stab)])
<br>

section([Boundary Conditions])
item([wall (isothermal, adiabatic, coupled)])
item([symmetry])
item([subsonic or supersonic inlet])
item([subsonic or supersonic outlet])

skip_line

dnl -------------------------------------------------------------
include_footer
