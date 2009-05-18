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
integer               :: i, dim, ufc, ir, istart, iend, isol, ifield, isca, ivec
integer               :: info, cgnstype, ielem, nvtex, nelem, icoord
real(8), allocatable  :: v(:)      ! temporary array for sol writing (type CGNS RealDouble)
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

! -- reindex and write SCALAR --

do isca = 1, field%etatprim%nscal

  istart = 0
  do ielem = 1, umesh%cellvtex%ntype
    do i = 1, umesh%cellvtex%elem(ielem)%nelem
      v(istart+i) = field%etatprim%tabscal(isca)%scal(umesh%cellvtex%elem(ielem)%ielem(i))
    enddo
    istart = istart +  umesh%cellvtex%elem(ielem)%nelem
  enddo

call cg_field_write_f(cgnsunit, ibase, izone, isol, RealDouble, trim(defsolver%namesca(isca)), v(1:nelem), ifield, info)
if (info /=0) call erreur("CGNS output", "writing "//trim(defsolver%namesca(isca))//" solution...")

enddo

! -- reindex and write VECTOR --

do ivec = 1, field%etatprim%nvect

  istart = 0
  do ielem = 1, umesh%cellvtex%ntype
    do i = 1, umesh%cellvtex%elem(ielem)%nelem
      v(istart+i) = field%etatprim%tabvect(ivec)%vect(umesh%cellvtex%elem(ielem)%ielem(i))%x
   enddo
   istart = istart +  umesh%cellvtex%elem(ielem)%nelem
 enddo

 call cg_field_write_f(cgnsunit, ibase, izone, isol, RealDouble, trim(defsolver%namesca(ivec))//'X', v, ifield, info)
 if (info /=0) call erreur("CGNS output", "writing "//trim(defsolver%namesca(ivec))//'X'//" solution...")

  istart = 0
  do ielem = 1, umesh%cellvtex%ntype
    do i = 1, umesh%cellvtex%elem(ielem)%nelem
      v(istart+i) = field%etatprim%tabvect(ivec)%vect(umesh%cellvtex%elem(ielem)%ielem(i))%y
   enddo
   istart = istart +  umesh%cellvtex%elem(ielem)%nelem
 enddo

 call cg_field_write_f(cgnsunit, ibase, izone, isol, RealDouble, trim(defsolver%namesca(ivec))//'Y', v, ifield, info)
 if (info /=0) call erreur("CGNS output", "writing "//trim(defsolver%namesca(ivec))//'Y'//" solution...")

  istart = 0
  do ielem = 1, umesh%cellvtex%ntype
    do i = 1, umesh%cellvtex%elem(ielem)%nelem
      v(istart+i) = field%etatprim%tabvect(ivec)%vect(umesh%cellvtex%elem(ielem)%ielem(i))%z
   enddo
   istart = istart +  umesh%cellvtex%elem(ielem)%nelem
 enddo

 call cg_field_write_f(cgnsunit, ibase, izone, isol, RealDouble, trim(defsolver%namesca(ivec))//'Z', v, ifield, info)
 if (info /=0) call erreur("CGNS output", "writing "//trim(defsolver%namesca(ivec))//'Z'//" solution...")

enddo


deallocate(v)

endsubroutine writecgns_sol
!------------------------------------------------------------------------------!
! Changes history
!
! Mar  2009 : created
!------------------------------------------------------------------------------!
