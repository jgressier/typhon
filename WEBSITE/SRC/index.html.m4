include(`general.m4')
dnl -------------------------------------------------------------
set_page_title([Welcome to TYPHON Solver website])
define([id1], [index])
define([id2], [])
define([id3], [])

include_header

dnl -------------------------------------------------------------

<p align=justify>
[TYPHON is an open source project which aims to offer a development platform for many computational methods
for gas dynamics. It is structured as a multi-solver platform where it could be easily added a
new solver. For now, it provides a finite volume solver for compressible inviscid equations and
a finite volume solver for heat transfer.] (see hyperlink([Presentation], [index_pres.html]))
</p>

dnl -------------------------------------------------------------
section([Web site news])

  item([06/11/2005 : Release Candadidate 1 of core 0.3 released (based on release 0.2.1)])
  item([13/10/2005 : version 0.2.1 released])
  item([04/09/2005 : version 0.2.0 released (implicit second order compressible Navier-Stokes)])
  item([12/07/2005 : version 0.1.7 released])
  item([11/03/2005 : News and Change log tabs])
  item([15/03/2005 : Development tabs])
[Detailed news are given in section] hyperlink([News], [index_news.html])
skip_line

dnl -------------------------------------------------------------
section([TYPHON solver])
<br>

dnl -------------------------------------------------------------
section([Users])
[<p align=justify>
The current release is 0.2.0 and is available as a package. Release 0.2.1 contains some
improvements and is available on the CVS server. Release 0.2.2 will come soon with some 
CPU optimizations.
See] hyperlink([Presentation], [index_pres.html]) [to find a description of this
release or] hyperlink([[Features section]], [feat.html]) [to get more details.]

[To be regularly informed, you are encouraged to subscribe]
hyperlink([typhon-users],[http://lists.sourceforge.net/lists/listinfo/typhon-users])</p>

dnl -------------------------------------------------------------
section([Developpers])
[<p align=justify>New developpers are welcomed to join the team by either adding contributions to planned
developments or proposing other features.]skip_line

[Future developments include:]<br>
item([compressible Navier-Stokes solver enhancements])
item([dynamic refinement])
item([parallelization])
<br>

[Welcomed developments are:]<br>
item([turbulence or LES models])
item([new solvers (acoustics, incompressible flow)])
item([overset grid computations])
item([whatever you are interested in])
</p>



skip_line

[For all remarks about this website, please send me an email:]
hyperlink([gressier at supaero.fr], [mailto:gressier at supaero.fr]).

dnl -------------------------------------------------------------
include_footer
