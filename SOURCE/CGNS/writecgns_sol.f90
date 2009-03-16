!------------------------------------------------------------------------------!
! Procedure : writecgns_sol                      Authors : J. Gressier
!         
! Fonction 
!   Write a SOLUTION to a CGNS ZONE
!
!------------------------------------------------------------------------------!
subroutine writecgns_sol(cgnsunit, ibase, izone, defsolver, umesh, field) 

use TYPHMAKE
use OUTPUT
use VARCOM
use MGRID
use USTMESH
use MENU_SOLVER

implicit none

include 'cgnslib_f.h'

! -- INPUTS --
integer               :: cgnsunit           ! unit number for cgns file
integer               :: ibase, izone       ! base and zone index
type(mnu_solver)      :: defsolver          ! solver model
type(st_ustmesh)      :: umesh
type(st_field)        :: field

! -- OUPUTS --

! -- Internal variables --
integer               :: i, dim, ufc, ir, istart, iend, isol, ifield
integer               :: info, cgnstype, ielem, nvtex, nelem, icoord
real(4), allocatable  :: v(:)      ! temporary array for sol writing (type CGNS RealDouble)
character(len=32)     :: solname

! -- BODY --

! see http://www.grc.nasa.gov/WWW/cgns/midlevel/solution.html

!-------------------------------------------------------------------------
! write solution structure (and get isol index) 

solname = 'MyFlowSolution'
call cg_sol_write_f(cgnsunit, ibase, izone, solname, CellCenter, isol, info)
if (info /=0) call erreur("CGNS output", "writing solution structure...")


!-------------------------------------------------------------------------
! write solution primitive quantities

! -- compute total number of elements --

nelem = 0
do ielem = 1, umesh%cellvtex%ntype
  nelem = nelem +  umesh%cellvtex%elem(ielem)%nelem
enddo

allocate(v(nelem))

! -- reindex DENSITY --

istart = 0
do ielem = 1, umesh%cellvtex%ntype
   do i = 1, umesh%cellvtex%elem(ielem)%nelem
      v(istart+i) = field%etatprim%tabscal(1)%scal(umesh%cellvtex%elem(ielem)%ielem(i))
   enddo
   istart = istart +  umesh%cellvtex%elem(ielem)%nelem
enddo
print*,cgnsunit, ibase, izone, isol, nelem, size(v)

call cg_field_write_f(cgnsunit, ibase, izone, isol, RealSingle, 'Density', v(1:nelem), ifield, info)
if (info /=0) call erreur("CGNS output", "writing Density solution...")

! -- reindex PRESSURE --

istart = 0
do ielem = 1, umesh%cellvtex%ntype
   do i = 1, umesh%cellvtex%elem(ielem)%nelem
      v(istart+i) = field%etatprim%tabscal(2)%scal(umesh%cellvtex%elem(ielem)%ielem(i))
   enddo
   istart = istart +  umesh%cellvtex%elem(ielem)%nelem
enddo

call cg_field_write_f(cgnsunit, ibase, izone, isol, RealDouble, 'Pressure', v(1:nelem), ifield, info)
if (info /=0) call erreur("CGNS output", "writing Pressure solution...")

! -- reindex VELOCITY X --

istart = 0
do ielem = 1, umesh%cellvtex%ntype
   do i = 1, umesh%cellvtex%elem(ielem)%nelem
      v(istart+i) = field%etatprim%tabvect(1)%vect(umesh%cellvtex%elem(ielem)%ielem(i))%x
   enddo
   istart = istart +  umesh%cellvtex%elem(ielem)%nelem
enddo

call cg_field_write_f(cgnsunit, ibase, izone, isol, RealDouble, 'VelocityX', v, ifield, info)
if (info /=0) call erreur("CGNS output", "writing VelocityX solution...")

! -- reindex VELOCITY Y --

istart = 0
do ielem = 1, umesh%cellvtex%ntype
   do i = 1, umesh%cellvtex%elem(ielem)%nelem
      v(istart+i) = field%etatprim%tabvect(1)%vect(umesh%cellvtex%elem(ielem)%ielem(i))%y
   enddo
   istart = istart +  umesh%cellvtex%elem(ielem)%nelem
enddo

call cg_field_write_f(cgnsunit, ibase, izone, isol, RealDouble, 'VelocityY', v, ifield, info)
if (info /=0) call erreur("CGNS output", "writing VelocityY solution...")

! -- reindex VELOCITY Z --

istart = 0
do ielem = 1, umesh%cellvtex%ntype
   do i = 1, umesh%cellvtex%elem(ielem)%nelem
      v(istart+i) = field%etatprim%tabvect(1)%vect(umesh%cellvtex%elem(ielem)%ielem(i))%z
   enddo
   istart = istart +  umesh%cellvtex%elem(ielem)%nelem
enddo

call cg_field_write_f(cgnsunit, ibase, izone, isol, RealDouble, 'VelocityZ', v, ifield, info)
if (info /=0) call erreur("CGNS output", "writing VelocityZ solution...")


deallocate(v)

endsubroutine writecgns_sol
!------------------------------------------------------------------------------!
! Changes history
!
! Mar  2009 : created
!------------------------------------------------------------------------------!
