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

  item([2009/03/10 : version 0.5.0 released (3rd and 4th order SVM, GMRES, Heat inverse solver)])
  item([2008/03/21 : version 0.4.0 released (Runge Kutta, 2nd order Spectral Volume Method)])
  item([2007/09/11 : CVS to SVN migration])
  item([2007/02/06 : version 0.3.2 released (development organization, initialization and outputs)])
dnl  item([2006/08/26 : version 0.3.1 released (bug correction, enhancement & optimization)])
dnl  item([2006/03/05 : version 0.3.0 released (development branches merged)])
dnl  item([2006/01/16 : version 0.2.2 released (MUSCL improvement)])
dnl   item([2005/11/06 : Added documentation about numerical schemes in 
dnl                     hyperlink([User guide], [doc_scheme.html]) [section]])

[Detailed news are given in section] hyperlink([News], [index_news.html])
skip_line

dnl -------------------------------------------------------------
section([TYPHON solver])


dnl -------------------------------------------------------------
dnl section([Users])

[<p align=justify>
The current release is 0.5.0. Best way to obtain it is to use subversion<br>
<tt><big>svn export https://typhon.svn.sourceforge.net/svnroot/typhon/tags/rel-0-5-0 typhon</big></tt><br>
</p>]

[<p align=justify>See] hyperlink([Presentation], [index_pres.html]) [to find a short description of this
release or] hyperlink([[Features section]], [feat.html]) [to get more details.]

[To be regularly informed, you are encouraged to subscribe]
hyperlink([typhon-users],[http://lists.sourceforge.net/lists/listinfo/typhon-users])</p>

[<p align=justify>
Please contact ]hyperlink([me], [mailto:gressier at users.sf.net])[ if you would like to provide an example
of Typhon computation (a mesh, some pictures and comments)
</p>]

dnl -------------------------------------------------------------
section([Developpers])
[<p align=justify>New developpers are welcomed to join the team by either adding contributions to planned
developments or proposing other features.]skip_line

[Future developments include:]<br>
item([compressible Navier-Stokes solver enhancements (implicit, SVM)])
item([dynamic refinement])
item([parallelization])
<br>

[Welcomed developments are:]<br>
item([turbulence or LES models])
item([new solvers (acoustics, incompressible flow)])
item([overset grid computations])
item([morphing grid computations])
item([whatever you are interested in])
</p>



skip_line

[For all remarks about this website, please send ] 
hyperlink([me], [mailto:gressier at users.sf.net])[ an email:].

dnl -------------------------------------------------------------
include_footer
