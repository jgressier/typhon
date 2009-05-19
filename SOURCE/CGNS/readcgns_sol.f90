!------------------------------------------------------------------------------!
! Procedure : readcgns_sol                      Authors : J. Gressier
!         
! Fonction 
!   Write a SOLUTION to a CGNS ZONE
!
!------------------------------------------------------------------------------!
subroutine readcgns_sol(cgnsunit, ibase, izone, defsolver, umesh, field) 

use TYPHMAKE
use OUTPUT
use VARCOM
use GENFIELD
use USTMESH
use MENU_SOLVER

implicit none

include 'cgnslib_f.h'

! -- INPUTS --
integer               :: cgnsunit           ! unit number for cgns file
integer               :: ibase, izone       ! base and zone index
type(mnu_solver)      :: defsolver          ! solver model
type(st_ustmesh)      :: umesh

! -- OUPUTS --
type(st_genericfield) :: field

! -- Internal variables --
integer                 :: itype, isol, info, dim, i, isca, ivec
real(krp), allocatable  :: v(:)      ! temporary array for sol reading

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
  call erreur("Fatal CGNS IO", "cannot read TYPHON real data...")
endselect

dim = umesh%ncell_int

do isca = 1, field%nscal

   call cg_field_read_f(cgnsunit, ibase, izone, isol, trim(defsolver%namesca(isca)), itype, &
                        1, dim, field%tabscal(isca)%scal(1:dim), info)
   if (info /=0) call erreur("CGNS IO", "reading "//trim(defsolver%namesca(isca))//" solution...")

enddo

if (field%nvect > 0) then

  allocate(v(dim))

  do ivec = 1, field%nvect

     call cg_field_read_f(cgnsunit, ibase, izone, isol, trim(defsolver%namevec(ivec))//'X', itype, &
                          1, dim, v, info)
     if (info /=0) call erreur("CGNS IO", "reading "//trim(defsolver%namesca(isca))//'X'//" solution...")
     do i = 1, dim
        field%tabvect(ivec)%vect(i)%x = v(i)
     enddo

     call cg_field_read_f(cgnsunit, ibase, izone, isol, trim(defsolver%namevec(ivec))//'Y', itype, &
                          1, dim, v, info)
     if (info /=0) call erreur("CGNS IO", "reading "//trim(defsolver%namesca(isca))//'Y'//" solution...")
     do i = 1, dim
        field%tabvect(ivec)%vect(i)%y = v(i)
     enddo

     call cg_field_read_f(cgnsunit, ibase, izone, isol, trim(defsolver%namevec(ivec))//'Z', itype, &
                          1, dim, v, info)
     if (info /=0) call erreur("CGNS IO", "reading "//trim(defsolver%namesca(isca))//'Z'//" solution...")
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
