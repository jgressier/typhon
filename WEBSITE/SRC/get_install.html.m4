include(`general.m4')
dnl -------------------------------------------------------------
set_page_title([Installation])
define([id1], [get])
define([id2], [install])
define([id3], [])

include_header

dnl -------------------------------------------------------------

section([How to build TYPHON])
item([system])
<p>Typhon has only been tested with Unix/Linux based systems.</p>
item([Fortran compilers])
<p>Typhon uses at least Fortran 90 norm. It has been successfully compiled with Solaris SUN, 
Irix SGI, Intel Linux fortran 90 compilers. It is known to raise an internal error with
PA-Risc/Itanium HP compiler.</p>


section([Fortran compilers])
item([Intel Linux])
<br>

section([External libraries])
item([BLAS])
item([LAPACK])
item([Metis])
<br>

section([MPI])
item([MPIch])
item([LAM-MPI])
skip_line



dnl -------------------------------------------------------------
include_footer
