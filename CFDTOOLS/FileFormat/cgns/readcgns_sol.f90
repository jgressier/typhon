!------------------------------------------------------------------------------!
! Procedure : readcgns_sol                      Authors : J. Gressier
!         
! Fonction 
!   Write a SOLUTION to a CGNS ZONE
!
!------------------------------------------------------------------------------!
subroutine readcgns_sol(cgnsunit, ibase, izone, umesh, field) 

use MESHPREC
use IOCFD
!use VARCOM
use GENFIELD
use USTMESH
use QUANTITY

implicit none

include 'cgnslib_f.h'

! -- INPUTS --
integer               :: cgnsunit           ! unit number for cgns file
integer               :: ibase, izone       ! base and zone index
type(st_ustmesh)      :: umesh

! -- OUPUTS --
type(st_genericfield) :: field

! -- Internal variables --
integer                  :: itype, isol, info, dim, i, isca, ivec
real(krp), allocatable   :: v(:)      ! temporary array for sol reading
character(len=shortname) :: qname

! -- BODY --

! see http://www.grc.nasa.gov/WWW/cgns/midlevel/solution.html

!-------------------------------------------------------------------------
! read solution primitive quantities

isol = 1
select case(krp)
case(4)
   itype = RealSingle
case(8)
   itype = RealDouble
case default
  call cfd_error("(CGNS) cannot read TYPHON real data...")
endselect

dim = umesh%ncell_int

do isca = 1, field%nscal

  qname = quantity_cgnsname(field%tabscal(isca)%quantity_id)
  call cfd_print("reading CGNS field: "//trim(qname))
  call cg_field_read_f(cgnsunit, ibase, izone, isol, trim(qname), itype, &
                       1, dim, field%tabscal(isca)%scal(1:dim), info)
  if (info /=0) call cfd_error("(CGNS) reading "//trim(qname)//" solution...")

enddo

if (field%nvect > 0) then

  allocate(v(dim))

  do ivec = 1, field%nvect

    call cfd_print("reading CGNS field: "//trim(qname))
    qname = quantity_cgnsname(field%tabvect(ivec)%quantity_id)
    call cg_field_read_f(cgnsunit, ibase, izone, isol, trim(qname)//'X', itype, &
                         1, dim, v, info)
     if (info /=0) call cfd_error("(CGNS) reading "//trim(qname)//'X'//" solution...")
     do i = 1, dim
        field%tabvect(ivec)%vect(i)%x = v(i)
     enddo

     call cg_field_read_f(cgnsunit, ibase, izone, isol, trim(qname)//'Y', itype, &
                          1, dim, v, info)
     if (info /=0) call cfd_error("(CGNS) reading "//trim(qname)//'Y'//" solution...")
     do i = 1, dim
        field%tabvect(ivec)%vect(i)%y = v(i)
     enddo

     call cg_field_read_f(cgnsunit, ibase, izone, isol, trim(qname)//'Z', itype, &
                          1, dim, v, info)
     if (info /=0) call cfd_error("(CGNS) reading "//trim(qname)//'Z'//" solution...")
     do i = 1, dim
        field%tabvect(ivec)%vect(i)%z = v(i)
     enddo

  enddo

  deallocate(v)
endif


endsubroutine readcgns_sol
!------------------------------------------------------------------------------!
! Changes history
!
! May  2009 : created
!------------------------------------------------------------------------------!
