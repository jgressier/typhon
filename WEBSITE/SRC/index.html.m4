include(`general.m4')
dnl -------------------------------------------------------------
set_page_title([TYPHON Presentation])
define([id1], [index])
define([id2], [])

include_header

dnl -------------------------------------------------------------

<p align=justify>
[TYPHON is a project which aims to offer a development platform for many computational methods
for gas dynamics. It is structured as a multi-solver platform where it could be easily added a
new solver. For now, it provides a finite volume solver for compressible inviscid equations and
a finite volume solver for heat transfer.] (see hyperlink([Presentation], [index_pres.html]))
</p>

section([Web site news])

  item([11/03/2005 : News and Change log tabs])
  item([15/03/2005 : Development tabs])

skip_line

[For all remarks about this website, please send me an email:]
hyperlink([gressier at supaero.fr], [mailto:gressier at supaero.fr]).

dnl -------------------------------------------------------------
include_footer
