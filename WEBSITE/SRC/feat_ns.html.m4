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
item([EFM/KFVS upwind scheme (cf Pullin or Deshpande)])
item([MUSCL second order extension (HIGHRES = MUSCL, MUSCL-UNS or FAST-MUSCL for high quality grids)])
[ with TVD limiters: Minmod, van Albada, van Leer, Superbee, Kim (3rd order)<br>]
item([Spectral Volume Method for high order extrapolation (only on 2D tri grids) (HIGHRES=SVM)])
[2nd order : SVM = 2 or 2QUAD<br>]
[3rd order : SVM = 3, 3WANG, 3KRIS or 3KRIS2<br>]
[4th order : SVM = 4, 4WANG, 4KRIS or 4KRIS2<br>]
item([Explicit multi-level Runge-Kutta time integration (2nd and 3rd order)])
item([Implicit backward Euler])
Linear Implicit resolution (BiCG-Stab, GMRes)<br>
Approximate upwind fluxes jacobian<br>
Viscous fluxes jacobian<br>
<br>

section([Boundary Conditions])
item([wall (isothermal, adiabatic, coupled) and moving wall])
item([symmetry])
item([subsonic or supersonic inlet])
item([subsonic or supersonic outlet])

skip_line

dnl -------------------------------------------------------------
include_footer
