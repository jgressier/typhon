!------------------------------------------------------------------------------!
! Procedure : writecgns_sol                      Authors : J. Gressier
!         
! Fonction 
!   Write a SOLUTION to a CGNS ZONE
!
!------------------------------------------------------------------------------!
subroutine writecgns_sol(cgnsunit, ibase, izone, umesh, field) 

use MESHPREC
use IOCFD
use QUANTITY
use USTMESH
use GENFIELD

implicit none

include 'cgnslib_f.h'

! -- INPUTS --
integer               :: cgnsunit           ! unit number for cgns file
integer               :: ibase, izone       ! base and zone index
type(st_ustmesh)      :: umesh
type(st_genericfield) :: field

! -- OUPUTS --

! -- Internal variables --
integer               :: i, dim, ufc, ir, istart, iend, isol, ifield, isca, ivec
integer               :: info, cgnstype, ielem, nvtex, nelem, icoord
real(8), allocatable  :: v(:)      ! temporary array for sol writing (type CGNS RealDouble)
character(len=shortname) :: qname, solname

! -- BODY --

! see http://www.grc.nasa.gov/WWW/cgns/midlevel/solution.html

!-------------------------------------------------------------------------
! write solution structure (and get isol index) 

solname = 'MyFlowSolution'
call cg_sol_write_f(cgnsunit, ibase, izone, solname, CellCenter, isol, info)
if (info /=0) call cfd_error("Fatal CGNS IO writing solution structure...")


!-------------------------------------------------------------------------
! write solution primitive quantities

! -- compute total number of elements --

nelem = 0
do ielem = 1, umesh%cellvtex%ntype
  nelem = nelem +  umesh%cellvtex%elem(ielem)%nelem
enddo

allocate(v(nelem))

! -- reindex and write SCALAR --

do isca = 1, field%nscal

  istart = 0
  do ielem = 1, umesh%cellvtex%ntype
    do i = 1, umesh%cellvtex%elem(ielem)%nelem
      v(istart+i) = field%tabscal(isca)%scal(umesh%cellvtex%elem(ielem)%ielem(i))
    enddo
    istart = istart +  umesh%cellvtex%elem(ielem)%nelem
  enddo

  qname = quantity_cgnsname(field%tabscal(isca)%quantity_id)
  call cg_field_write_f(cgnsunit, ibase, izone, isol, RealDouble, trim(qname), v(1:nelem), ifield, info)
  if (info /=0) call cfd_error("Fatal CGNS IO writing "//trim(qname)//" solution...")

enddo

! -- reindex and write VECTOR --

do ivec = 1, field%nvect

  istart = 0
  do ielem = 1, umesh%cellvtex%ntype
    do i = 1, umesh%cellvtex%elem(ielem)%nelem
      v(istart+i) = field%tabvect(ivec)%vect(umesh%cellvtex%elem(ielem)%ielem(i))%x
    enddo
    istart = istart +  umesh%cellvtex%elem(ielem)%nelem
  enddo
 
  qname = quantity_cgnsname(field%tabvect(ivec)%quantity_id)
  call cg_field_write_f(cgnsunit, ibase, izone, isol, RealDouble, trim(qname)//'X', v, ifield, info)
  if (info /=0) call cfd_error("Fatal CGNS IO writing "//trim(qname)//'X'//" solution...")

  istart = 0
  do ielem = 1, umesh%cellvtex%ntype
    do i = 1, umesh%cellvtex%elem(ielem)%nelem
      v(istart+i) = field%tabvect(ivec)%vect(umesh%cellvtex%elem(ielem)%ielem(i))%y
   enddo
   istart = istart +  umesh%cellvtex%elem(ielem)%nelem
 enddo

 call cg_field_write_f(cgnsunit, ibase, izone, isol, RealDouble, trim(qname)//'Y', v, ifield, info)
 if (info /=0) call cfd_error("Fatal CGNS IO writing "//trim(qname)//'Y'//" solution...")

  istart = 0
  do ielem = 1, umesh%cellvtex%ntype
    do i = 1, umesh%cellvtex%elem(ielem)%nelem
      v(istart+i) = field%tabvect(ivec)%vect(umesh%cellvtex%elem(ielem)%ielem(i))%z
   enddo
   istart = istart +  umesh%cellvtex%elem(ielem)%nelem
 enddo

 call cg_field_write_f(cgnsunit, ibase, izone, isol, RealDouble, trim(qname)//'Z', v, ifield, info)
 if (info /=0) call cfd_error("Fatal CGNS IO writing "//trim(qname)//'Z'//" solution...")

enddo


deallocate(v)

endsubroutine writecgns_sol
!------------------------------------------------------------------------------!
! Changes history
!
! Mar  2009 : created
!------------------------------------------------------------------------------!
