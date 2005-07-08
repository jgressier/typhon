include(`general.m4')
dnl -------------------------------------------------------------
set_page_title([Change Log])
define([id1], [dev])
define([id2], [chlog])

include_header

define([beginlist],  [<table class=defaut border=0 cellpadding=0 cellspacing=0 style="padding-left:10pt; padding-right:10pt">])
define([endlist],    [</table><br>])
define([writefunc], [<tr class=blue><td align=center>$1</td><td width=10><td>$2</td></tr>])
define([writedev],  [<tr class=orange><td align=center>$1</td><td width=10><td>$2</td></tr>])
define([writebug],  [<tr class=red><td align=center>$1</td><td width=10><td>$2</td></tr>])

dnl -------------------------------------------------------------
sectionlr([release 0.1.7], [10/07/2005])
beginlist
writedev ([core]    [introduces User Defined Functions (UDF) through user-made fortran functions])
writefunc([KDIF]    [features radiating boundary condition])
writedev ([KDIF]    [features radiative coupling with view factors (simplified)])
writedev ([KDIF]    [features anisotropic material (only UDF)])
writedev ([KDIF]    [features boundary parameters temporal interpolation (via UDF)])
writefunc([NS],     [features HLLC upwind scheme (ability for viscous flows)])
writedev ([NS],     [features Viscous stress tensor (laminar flows)])
writedev ([OUTPUT]  [can write boundary condition face centers])
writedev ([OUTPUT]  [can write unsteady results on boundaring family])
endlist

dnl -------------------------------------------------------------
sectionlr([release 0.1.6], [10/12/2004])
beginlist
writefunc([NS],     [MUSCL interpolation for 2nd order computations])
writedev([GUI],     [Plot residuals for only one zone])
writedev([OUTPUT],  [Residuals in monres.nnn file])
endlist

dnl -------------------------------------------------------------
sectionlr([release 0.1.5], [05/08/2004])
beginlist
writefunc([NS], [Inviscid solver released, HLLE first order upwind scheme])
writefunc([NS], [boundary conditions: subsonic and supersonic inlet and oulet])
writefunc([NS], [boundary conditions: symmetry])
writedev([OUTPUT], [VTK format for scalars and vectors (NS only)])
endlist

dnl -------------------------------------------------------------
sectionlr([release 0.1.4], [01/07/2004])
beginlist
writebug([known bug], [computation of 3D meshes with QUAD face cells])
writefunc([MESH],  [CGNS files: TRI/QUAD 2D meshes, TETRA/PRISM 3D meshes])
writefunc([KDIF],  [boundary conditions: convection h(T-Tref)])
writefunc([KDIF],  [Implicit time integration (direct LU or Iterative Jacobi) ])
writedev([OUTPUT], [VTK format for scalars and vectors (NS only)])
endlist

dnl -------------------------------------------------------------
sectionlr([release 0.1.3], [08/04/2004])
beginlist
writebug([known bug], [computation of gradients])
writefunc([MESH],  [CGNS files: 2D meshes of TRI cells])
writefunc([MZONE], [KDIF/KDIF coupling])
writefunc([MZONE], [Computation of conformal connections of faces between zones])
endlist

dnl -------------------------------------------------------------
sectionlr([release 0.1.2], [15/10/2003])
beginlist
writefunc([INPUT], [Manage stationnary computations by residual criteria])
writefunc([core],  [Computation of gradients by least mean square method])
writefunc([core],  [CPU optimization])
writefunc([KDIF],  [extended flux interpolation])
writefunc([KDIF],  [Fourier based timestep computation])
endlist

dnl -------------------------------------------------------------
sectionlr([release 0.1.1], [01/08/2003])
beginlist
writefunc([OUTPUT], [TECPLOT files, vertex based solution (QUAD) and connectivity])
endlist

dnl -------------------------------------------------------------
sectionlr([release 0.1.0], [01/07/2003])
beginlist
writefunc([MESH], [CGNS files: unstructured 2D mesh of QUAD cells])
writefunc([KDIF], [Heat transfer solver released])
writefunc([KDIF], [constant conduction coefficient])
writefunc([KDIF], [explicit temporal integration with global time step])
writefunc([KDIF], [simplified flux interpolation])
writefunc([KDIF], [boundary conditions: isothermal and adiabatic])
writefunc([OUTPUT], [TECPLOT files, cell centered solution])
endlist

dnl -------------------------------------------------------------
section([june 2002: started development])



skip_line

dnl -------------------------------------------------------------
include_footer
