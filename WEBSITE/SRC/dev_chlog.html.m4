include(`general.m4')
dnl -------------------------------------------------------------
set_page_title([Change Log])
define([id1], [dev])
define([id2], [chlog])
define([id3], [])

include_header

define([beginlist], [<table class=defaut border=0 cellpadding=0 cellspacing=0 style="padding-left:10pt; padding-right:10pt">])
define([endlist],   [</table><br>])
define([writefunc], [<tr class=blue><td align=center>$1</td><td width=10><td>$2</td></tr>])
define([writedev],  [<tr class=orange><td align=center>$1</td><td width=10><td>$2</td></tr>])
define([writebug],  [<tr class=red><td align=center>$1</td><td width=10><td>$2</td></tr>])

dnl -------------------------------------------------------------
sectionlr([release 0.5.0 (r632)], [2009/03/10])
beginlist
writefunc([NS],     [Spectral Volume Method (3rd & 4th order, VTK output) (r602-603,618-619,624-625)])
writefunc([NS],     [Kinetic flux function (EFM/KFVS) (r631) and Rusanov flux (r612)])
writefunc([NS],     [Post-limitation (Barth, Monotonic) (r610,623)])
writefunc([Mesh],   [SVM-like mesh splitting (r618-619,624-625)])
writefunc([KDIF],   [Unsteady non uniform boundary conditions (symbolic functions) (r601)])
writefunc([KDIF],   [Implicitation of Flux and Thermal conditions (r612)])
writefunc([KDIF],   [Inverse boundary condition solver (r606,607,609,613-615,617,620)])
writefunc([Core],   [GMRES solver (NS or KDIF) (r627)])
writefunc([Core],   [Improvement of memory management (r621)])
writedev([Input],   [Improve CGNS reading (r601)])
writedev([Core],    [translations of comments (r627)])
writedev([Core],    [generalized element connectivity (r602)])
writebug([Ouput],   [remove NODE based Tecplot output (r602)])
endlist

dnl -------------------------------------------------------------
sectionlr([release 0.4.0 (r600)], [2008/03/21])
beginlist
writefunc([NS],     [Spectral Volume Method (2nd order) (r597-599)])
writefunc([NS],     [Runge Kutta integration (RK2, RK3-SSP) (r592,594)])
writefunc([NS],     [Miscellaneous initialization modes (r591,594)])
writefunc([Mesh],   [SVM2QUAD splitting for all methods (r589-590)])
writefunc([MPI],    [portage (r576,578)])
writefunc([Input],  [Improve CGNS reading (unused marks, volume marks) (r590-591)])
writedev([Input],   [Intel fortran 9.1 portage (CGNS reading) (r593)])
writefunc([Ouput],  [Screen outputs enhancement (r582,586)])
writefunc([Ouput],  [Miscellaneous translations (r570-575,577)])
writedev([Core],    [tree/list structure of unstructured grids (r562)])
endlist

dnl -------------------------------------------------------------
sectionlr([release 0.3.2 (r559)], [2007/02/06])
beginlist
writefunc([NS],     [Basic MUSCL method (MUSCL-UNS)])
writefunc([NS],     [Initialization with symbolic functions])
writefunc([NS],     [Initialization with optional static/total pressure or temperature, Mach/velocity])
writefunc([NS],     [Moving wall boundary condition (WALL_VELOCITY)])
writedev([NS],      [Viscous flux implicitation])
writefunc([Output], [Binary VTK (VTK-BIN)])
writefunc([Core],   [Automatic system configuration])
writefunc([Core],   [Automatic non regression test cases])
endlist

dnl -------------------------------------------------------------
sectionlr([release 0.3.1 (r479))], [2006/08/26])
beginlist
writefunc([NS],     [Kim's 3rd order limiter extended to MUSCL method])
writefunc([Output], [display CPU time])
writefunc([Output], [enhanced outputs and options (periodic, etc)])
writefunc([Core],   [external stop of computation (file stop_typhon)])
writefunc([Core],   [CPU optimization (20% for explicit second order NS computation)])
writedev([Core],    [symbolic computation library])
writefunc([MESH],   [can use long file names])
writebug([MPI],     [Bug correction in automatic splitting])
endlist

dnl -------------------------------------------------------------
sectionlr([release 0.3.0 (r406)], [2006/03/05])
beginlist
writefunc([MPI],  [automatic mesh splitting (Metis)])
writefunc([MPI],  [NS first order computation (up to 20 procs at least)])
writefunc([MPI],  [NS second order computation (slightly different results at interfaces)])
writefunc([MPI],  [Unsteady synchronization])
endlist

dnl -------------------------------------------------------------
sectionlr([release 0.2.2], [2006/01/16])
beginlist
writefunc([Opt],  [optimization of CPU cost for gradient optimization (nearly 50%)])
writefunc([NS],   [Fast MUSCL method (for high quality meshes)])
writefunc([NS],   [Kim's 3rd order limiter (only for Fast MUSCL method)])
writefunc([NS],   [test of internal energy positivity (output of bad cell index if not positive)])
writebug([Core],  [ensure local time stepping bounded to cycle time step])
endlist

dnl -------------------------------------------------------------
sectionlr([release 0.3 RC 1], [2005/11/06])
beginlist
writedev([MPI],  [automatic mesh splitting (Metis)])
writedev([MPI],  [NS first order computation (up to 20 procs)])
writedev([MPI],  [Unsteady synchronization])
endlist

dnl -------------------------------------------------------------
sectionlr([release 0.2.1], [13/10/2005])
beginlist
writefunc([MESH],  [scaling function])
writefunc([NS],    [features constant dynamic viscosity])
writefunc([NS],    [features linear dynamic viscosity])
writedev([NS],     [initialization with a file])
writedev([KDIF],   [initialization with a file])
writefunc([NS],    [improved subsonic outlet condition (enforce pi>= p)])
writebug([NS],     [features Implicit jacobian matrices for HLL fluxes (diagonal equivalent)])
writefunc([NS],    [improved MUSCL method])
writedev([NS],     [features non-uniform thermal boundary conditions])
writedev([NS],     [improved heat flux boundary condition at wall])
writedev([core],   [Navier-Stokes/Heat transfer coupling])
writebug([NS],     [CFL computation for unsteady problems])
endlist

dnl -------------------------------------------------------------
sectionlr([release 0.2.0], [04/09/2005])
beginlist
writefunc([MESH],  [scaling function])
writefunc([NS],    [features constant dynamic viscosity])
writedev([core],   [Implicitation program structure reorganised (include ghost cells)])
writefunc([KDIF],  [features BiCG, BiCG-Jacobi, CGS iterative inversion methods (DLU structure)])
writefunc([KDIF],  [improved adiabatic boundary condition])
writefunc([NS],    [features BiCG-Stab iterative inversion method (block-DLU structure)])
writefunc([NS],    [changed symmetry boundary condition (ghost cell is now really symmetric: improved hypersonic flows)])
writefunc([NS],    [features (validation) Viscous stress tensor (laminar flows), Sutherland law ])
writefunc([NS],    [features increasing CFL number with convergence (bounded by CFL_MAX parameter)])
writebug([NS],     [MUSCL interpolation for second order accuracy])
writedev([NS],     [features non-uniform thermal boundary conditions])
writedev([NS],     [improved heat flux boundary condition at wall])
writedev([core],   [Navier-Stokes/Heat transfer coupling])
endlist

dnl -------------------------------------------------------------
sectionlr([release 0.1.7], [08/07/2005])
beginlist
writedev([core],   [introduces User Defined Functions (UDF) through user-made fortran functions])
writefunc([KDIF],  [features radiating boundary condition])
writedev([KDIF],   [features radiative coupling with view factors (simplified)])
writedev([KDIF],   [features anisotropic material (only UDF)])
writedev([KDIF],   [features boundary parameters temporal interpolation (via UDF)])
writefunc([NS],    [features HLLC upwind scheme (ability for viscous flows)])
writedev([NS],     [features Viscous stress tensor (laminar flows)])
writedev([OUTPUT], [can write boundary condition face centers])
writedev([OUTPUT], [can write unsteady results on boundaring family])
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
