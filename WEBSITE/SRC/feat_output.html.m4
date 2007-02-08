include(`general.m4')
dnl -------------------------------------------------------------
set_page_title([Solver features - Output functions and files])
define([id1], [feat])
define([id2], [output])
define([id3], [])

include_header

dnl -------------------------------------------------------------

section([Output files])
item([optional mesh computation report])
item([final output or unsteady (periodic) outputs at cycles])
item([VTK])
Cell centered unstructured data, save primitive variables <br>
Binary (VTK-BIN keyword) or ASCII (VTK keyword) <br>
item([Tecplot])
Cell vertex interpolation & unstructured mesh
<br>
<br>

section([Monitoring])
item([Residuals file (monres.xxx)])

skip_line

dnl -------------------------------------------------------------
include_footer
