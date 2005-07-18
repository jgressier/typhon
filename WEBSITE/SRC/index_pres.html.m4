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

<p align=justify>
[Contributors are welcomed to add some methods or a new solver. Some of further developments 
could be]<br>
item([high-order methods (spectral volume methods or others)])
item([turbulence modelling, LES])
item([chimera grids])
item([graphical user interface])
</p>


dnl -------------------------------------------------------------

dnl section([Web site news])



skip_line


dnl -------------------------------------------------------------
include_footer
