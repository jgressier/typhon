include(`general.m4')
dnl -------------------------------------------------------------
set_page_title([Project Presentation])
define([id1], [index])
define([id2], [pres])
define([id3], [])

include_header

<p align=justify>
[TYPHON is a project which aims to offer a development platform for many computational methods
for gas dynamics. It is structured as a multi-solver platform where it could be easily added a
new solver. For now, it provides a finite volume solver for compressible inviscid equations and
a finite volume solver for heat transfer.]
</p>

dnl -------------------------------------------------------------

section([Core structure])

<p align=justify>
[The core structure is based on a unsteady integration of user-defined cycle duration.
Each cycle integration process is independent: one can choose different mesh based structures
or different methods on different equations set. The core is currently organised with]<br>
item([a full unstructured 2D/3D mesh (CGNS input)])
item([an automatic mesh splitting and MPI based parallel computations])
item([a conjugate heat transfer/fluid process])
item([an extension to unstructured grid, patch-based, tree structure is currently under development])
</p>


dnl -------------------------------------------------------------

section([Compressible fluid solver])

<p align=justify>
[This solver is based on full compressibility Navier-Stokes equations. It has been mainly used and 
validated on high speed aerodynamics or high compressibility effects such as unsteady shock waves 
interactions. One can point out these following features]<br>
item([robust upwind schemes (HLL, HLLC, AUSM)])
item([2nd and 3rd order MUSCL type extension])
</p>

<p align=justify>
[Contributors are welcomed to add some methods to the existing solver. Some of further developments 
could be]<br>
item([high-order methods (spectral volume methods or others)])
item([turbulence modelling (RANS), Large eddy simulation (LES)])
item([Multigrid and implicit method for convergence acceleration])
item([Dual time stepping])
item([Adaptative mesh refinement (AMR)])
</p>


dnl -------------------------------------------------------------

section([Heat transfer solver])
<p align=justify>
[]
</p>

dnl -------------------------------------------------------------

section([Open source project])

<p align=justify>
[Contributors are welcomed to add a new solver or specific interface with other projects. 
It is proposed]<br>
item([graphical user interface for parameter input])
item([incompressible fluid solver])
item([lagrangian vortex solver and/or surface panel method])
item([output format to post-processing softwares])
</p>




skip_line


dnl -------------------------------------------------------------
include_footer
