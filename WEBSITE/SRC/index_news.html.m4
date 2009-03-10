include(`general.m4')
dnl -------------------------------------------------------------
set_page_title([Project News])
define([id1], [index])
define([id2], [news])
define([id3], [])

include_header

dnl -------------------------------------------------------------

section([March 2009, 9th : version 0.5.0 released])
item([3rd and 4th order Spectral Volume Method])
item([GMRES iterative solver for implicit formulations])
item([Inverse solver for Heat Transfer])
item([New inviscid numerical fluxes (EFM, Rusanov)])
item([New limiters for state extrapolation (Barth and variants)])
See hyperlink([Change log], [dev_chlog.html]) for details <br>
<br>


section([March 2008, 21st : version 0.4.0 released])
item([Runge-Kutta time integration (2nd and 3rd order)])
item([2nd order Spectral Volume Method])
item([Various initialization modes])
See hyperlink([Change log], [dev_chlog.html]) for details <br>
<br>

section([September 2007, 11th : CVS to SVN migration])
The new command to get the current developed release is<br>
<tt><big>svn export https://typhon.svn.sourceforge.net/svnroot/typhon/trunk</big></tt>
<br>

section([February 2007, 6th : version 0.3.2 released])
item([Symbolic function initialization (P/Pi, T/Ti, Mach/Velocity)])
item([New binary VTK output])
item([Automatic configuration for compilation])
item([Automatic non regression test cases])
See hyperlink([Change log], [dev_chlog.html]) for details <br>
<br>

section([August 2006, 26th : version 0.3.1 released])
item([Enhanced output options (CPU, periodic writing)])
item([CPU optimization])
item([Bug correction (MPI splitting)])
See hyperlink([Change log], [dev_chlog.html]) for details <br>
<br>

section([March 2006, 5th : version 0.3.0 released])
[It is based on a merged version of release 0.2.2 and 0.3RC1 (this release was only based on 0.2.0).]
See hyperlink([Change log], [dev_chlog.html]) for details <br>
<br>

section([January 2006, 16th : version 0.2.2 released])
item([CPU optimisation of gradient computation (MUSCL scheme, viscous flows and heat transfer)])
item([MUSCL high order variants (specific method for high quality mesh, 3rd order based limiter)])
See hyperlink([Change log], [dev_chlog.html]) for details <br>
<br>

section([November 2005, 6th : core 0.3 release candidate 1 released])
item([based on released 0.2.1 version])
item([Parallel computation: automatic splitting (METIS library) of grids])
See hyperlink([Change log], [dev_chlog.html]) for details <br>
<br>

section([October 2005, 13th : version 0.2.1 released])
item([Navier-Stokes solver: new viscosity definitions, MUSCL improvement, AUSM-M scheme, CFL evolution])
See hyperlink([Change log], [dev_chlog.html]) for details <br>
<br>

section([September 2005, 4th : version 0.2.0 released])
item([Navier-Stokes solver: laminar viscous fluxes, MUSCL interpolation, HLLC scheme, implicit integration])
item([Heat Transfer solver: new efficient implicit inversion methods (BiCG, CGS)])
See hyperlink([Change log], [dev_chlog.html]) for details <br>
<br>

section([July 2005, 10th : version 0.1.7 released])
<br>

section([April 2005, 20th : mailing lists available])

skip_line


dnl -------------------------------------------------------------
include_footer
