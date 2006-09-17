include(`general.m4')
dnl -------------------------------------------------------------
set_page_title([Download])
define([id1], [get])
define([id2], [download])
define([id3], [])

include_header

dnl -------------------------------------------------------------


section([TYPHON sources])
<p> As every sourceforge project, sources are available on CVS server. Let's sum up how to get them: </p>
item([choose your version as a tarball file on 
      hyperlink([sourceforge download], [http://sourceforge.net/project/showfiles.php?group_id=132321]) page])
item([get a tagged version from CVS repository using <br>
      cvs -z5 -d:pserver:anonymous@typhon.cvs.sourceforge.net:/cvsroot/typhon export -r rel-0-2-1 TYPHON])
item([get an up-to-date version from CVS repository using <br>
      cvs -z5 -d:pserver:anonymous@typhon.cvs.sourceforge.net:/cvsroot/typhon export -D today TYPHON])
skip_line

section([External libraries])
item([hyperlink([CGNS], [http://cgns.sf.net])])
item([Blas/Lapack])
item([hyperlink([Metis], [http://www-users.cs.umn.edu/~karypis/metis/]): this graph partitioning library could not be used during      computations but is needed in the building process.])
item([MPI: is only needed for the parallel version of Typhon. hyperlink([MPICH], [http://www-unix.mcs.anl.gov/mpi/mpich/])
      is usually used.])
skip_line

section([Third party tools])
item([hyperlink([CGNS tools], [http://sourceforge.net/projects/cgns/])])
item([hyperlink([Paraview], [http://www.paraview.org]): the hyperlink([VTK], [http://www.vtk.org]) based 
     visualization application])
skip_line


dnl -------------------------------------------------------------
include_footer
