!------------------------------------------------------------------------------!
! MODULE : TECFMT
!
!------------------------------------------------------------------------------!
module TECFMT

use MESHPREC         ! Precision configuration
use IOCFD

implicit none

! -- Global Variables -------------------------------------------


! -- Parameters --

integer(kip), parameter :: tecbufsize = 10000

integer(kpp),    parameter :: teckip = 4
integer(kpp),    parameter :: teckrp = 8

integer(teckip), parameter :: endstring = 0
integer(teckip), parameter :: nullint   = 0
real(4),         parameter :: zoneheader = 299._4
real(4),         parameter :: dataheader = 299._4
real(4),         parameter :: eohmarker  = 357._4

integer(teckip), parameter :: tecfile_full = 0
integer(teckip), parameter :: tecfile_mesh = 1
integer(teckip), parameter :: tecfile_sol  = 2

integer(teckip), parameter :: tecloc_node  = 0
integer(teckip), parameter :: tecloc_cell  = 1

integer(teckip), parameter :: tec_ordered      = 0
integer(teckip), parameter :: tec_felineseg    = 1
integer(teckip), parameter :: tec_fetri        = 2
integer(teckip), parameter :: tec_fequad       = 3
integer(teckip), parameter :: tec_fetetra      = 4
integer(teckip), parameter :: tec_febrick      = 5
integer(teckip), parameter :: tec_fepolygon    = 6
integer(teckip), parameter :: tec_fepolyhedron = 7

!------------------------------------------------------------------------------!
! definition of TEC output
!------------------------------------------------------------------------------!
type st_deftec
  integer                  :: iunit
  character(len=longname)  :: filename
  integer(teckip)          :: nvar
  integer(teckip)          :: zonetype
  integer(teckip)          :: nnode, ncell
  integer(teckip), pointer :: location(:)
  real(teckrp),    pointer :: min(:), max(:)
endtype st_deftec

!------------------------------------------------------------------------------!
! Interfaces

contains 
!------------------------------------------------------------------------------!

!------------------------------------------------------------------------------!
! open & write TEC header definition
!------------------------------------------------------------------------------!
subroutine tec_openwrite(iunit, filename, deftec)
implicit none
! -- INPUTS --
integer               :: iunit
character(len=*)      :: filename
! -- OUTPUTS --
type(st_deftec)      :: deftec
! -- private data --
integer              :: info, ib
! -- BODY --

info = 0

deftec%iunit    = iunit 
deftec%filename = trim(filename)

open(iunit, file=filename, status='unknown', form='binary')

write(iunit) "#!TDV112"
write(iunit) 1_teckip

if (info /=0) call cfd_error("unable to open/init tecplot file")

endsubroutine tec_openwrite

!------------------------------------------------------------------------------!
! write TEC zone
!------------------------------------------------------------------------------!
subroutine tecwrite_zoneheader(deftec, ztitle, parent, id, time, info)
implicit none
! -- INPUTS --
type(st_deftec)      :: deftec
character(len=*)     :: ztitle
integer(teckip)      :: parent, id, sharezonecon
real(teckrp)         :: time
! -- OUTPUTS --
integer              :: info
! -- private data --
integer              :: i

! -- BODY --

info  = 0

write(deftec%iunit) zoneheader

call tecwrite_str(deftec%iunit, ztitle)

write(deftec%iunit) parent-1_teckip, id-1_teckip
write(deftec%iunit) time 
write(deftec%iunit) -1_teckip, deftec%zonetype
write(deftec%iunit) 1_teckip, deftec%location(1:deftec%nvar)
write(deftec%iunit) nullint, nullint    ! no face definition
write(deftec%iunit) deftec%nnode
! if polygon, write nface ...
write(deftec%iunit) deftec%ncell
write(deftec%iunit) nullint, nullint, nullint
write(deftec%iunit) nullint             ! no auxiliary var

endsubroutine tecwrite_zoneheader

!------------------------------------------------------------------------------!
! write TEC zone
!------------------------------------------------------------------------------!
subroutine tecwrite_dataheader(deftec, passivevar, sharezonevar, sharezonecon, info)
implicit none
! -- INPUTS --
type(st_deftec)      :: deftec
integer(teckip)      :: sharezonecon
integer(teckip)      :: varloc(deftec%nvar), sharezonevar(deftec%nvar), passivevar(deftec%nvar)
! -- OUTPUTS --
integer              :: info
! -- private data --
integer              :: i

! -- BODY --

info  = 0

write(deftec%iunit) dataheader    ! DATA section headder

do i = 1, deftec%nvar
  write(deftec%iunit) 2_teckip    ! data are real(8)
enddo

write(deftec%iunit) 1_teckip   ! definition of passive/active variables
write(deftec%iunit) passivevar(1:deftec%nvar)
write(deftec%iunit) 1_teckip   ! def of shared variables
write(deftec%iunit) sharezonevar(1:deftec%nvar)-1_teckip
write(deftec%iunit) sharezonecon-1_teckip

endsubroutine tecwrite_dataheader

!------------------------------------------------------------------------------!
! write TEC data
!------------------------------------------------------------------------------!
subroutine tecwrite_data(deftec, value, info)
implicit none
! -- INPUTS --
type(st_deftec)       :: deftec
real(teckrp)          :: value(:,:)    ! nvalue x nvar
! -- OUTPUTS --
integer              :: info
! -- private data --
integer               :: nvalue, nvar, i
! -- BODY --

info  = 0

nvalue = size(value, dim=1)
nvar   = size(value, dim=2)

if ((nvalue /= deftec%nnode).and.(nvalue /= deftec%ncell)) then
  call cfd_error("internal error: bad number of values when writing TECPLOT data")
endif
if (nvar > deftec%nvar) then
  call cfd_error("internal error: too many variables when writing TECPLOT data")
endif

do i = 1, nvar
  ! min and max values (always real*8)
  write(deftec%iunit) real(minval(value(1:nvalue,i)), 8), real(maxval(value(1:nvalue,i)), 8)
enddo

write(deftec%iunit) value(:,:)
!do i = 1, nvar
!  write(deftec%iunit) value(1:nvalue,i)
!enddo

endsubroutine tecwrite_data

!------------------------------------------------------------------------------!
! write TEC string
!------------------------------------------------------------------------------!
subroutine tecwrite_str(iunit, str)
implicit none
! -- INPUTS --
integer              :: iunit
character(len=*)     :: str
! -- OUTPUTS --
! -- private data --
integer              :: i
! -- BODY --

do i = 1, len(str)
  write(iunit) iachar(str(i:i))
enddo
write(iunit) endstring

endsubroutine tecwrite_str


endmodule TECFMT
!------------------------------------------------------------------------------!
! Changes
!
! May  2011: created
!------------------------------------------------------------------------------!
