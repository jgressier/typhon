include(`general.m4')
dnl -------------------------------------------------------------
set_page_title([Solver features - Heat Transfer model])
define([id1], [feat])
define([id2], [kdif])
define([id3], [])

include_header

dnl -------------------------------------------------------------

section([Material model])
item([constant properties (linear)])
item([non linear conductivity (polynomial functions)])
item([anisotropic conductivity (UDF)])
<br>

section([Numerical Schemes])
item([second order diffusive flux: stabilized (FULL) or average (AVERAGE) gradients])
item([second order diffusive flux for aligned mesh (COMPACT)])
item([Linear Implicit resolution (Jacobi, BiCG, BiCG Jacobi preconditioned, CGS)])
<br>

section([Boundary Conditions])
item([set temperature])
item([set flux])
item([add radiating flux])


skip_line

dnl -------------------------------------------------------------
include_footer
