!------------------------------------------------------------------------------!
! MODULE : TYFMT_PART
!
!------------------------------------------------------------------------------!
module TYFMT_PART

use IO_UNIT
use XBIN_DATA
use LIBRLE
use TYPHON_FMT
use MESHPREC         ! Precision configuration
use MESHPART

implicit none

! -- Global Variables -------------------------------------------

integer(xbinkpp), parameter :: typart_rle = 1

integer(xbinkpp), parameter :: xbinty_part = 51  ! DATA section type for PART definition

! -- DECLARATIONS -----------------------------------------------------------

!------------------------------------------------------------------------------!
! ST_DEFTYPHON
!------------------------------------------------------------------------------!
type st_deftypart
  type(st_defxbin) :: defxbin    ! data for XBIN format interface
  integer(xbinkip) :: version    ! version of partition file
  integer(xbinkpp) :: mode       ! mode of partition writing
  integer(xbinkip) :: ncell      ! size of graph
  integer(xbinkip) :: npart      ! number of parts
endtype st_deftypart

contains 
!------------------------------------------------------------------------------!

!------------------------------------------------------------------------------!
! open XBIN TYPHON
!------------------------------------------------------------------------------!
subroutine typart_openread(filename, deftypart)
implicit none
! -- INPUTS --
integer            :: iunit
character(len=*)   :: filename
! -- OUTPUTS --
type(st_deftypart) :: deftypart
! -- private data --
type(st_xbindatasection)      :: xbindata

! -- BODY --

  call xbin_openread(filename, deftypart%defxbin)
  call xbin_readdatahead(deftypart%defxbin, xbindata)

  if (xbindata%nparam >= 1) then
    deftypart%version  = xbindata%param(1)
  else
    call cfd_error("XBIN/TYPART error: expecting parameters in TYPART header, unable to read version number")
  endif

select case (deftypart%version)
case(1:)
  if (xbindata%nparam /= 4) call cfd_error("XBIN/TYPART error: bad number parameters in TYPHON header")
  deftypart%version  = xbindata%param(1)
  deftypart%mode     = xbindata%param(2)
  deftypart%ncell    = xbindata%param(3)
  deftypart%npart    = xbindata%param(4)
case default
  call cfd_error("XBIN/TYPART error: unexpected version number")
endselect

call xbin_skipdata(deftypart%defxbin, xbindata)

endsubroutine typart_openread

!------------------------------------------------------------------------------!
! open XBIN TYPHON
!------------------------------------------------------------------------------!
subroutine typart_openwrite(filename, deftypart, ncell, npart)
implicit none
! -- INPUTS --
integer(kip)       :: ncell, npart
character(len=*)   :: filename
! -- OUTPUTS --
type(st_deftypart) :: deftypart
! -- private data --
! -- BODY --

  deftypart%version = 1
  deftypart%mode    = typart_rle
  deftypart%ncell   = ncell
  deftypart%npart   = npart
  
  call xbin_openwrite(filename, deftypart%defxbin)

endsubroutine typart_openwrite

!------------------------------------------------------------------------------!
! open XBIN TYPHON
!------------------------------------------------------------------------------!
subroutine typart_close(deftypart)
implicit none
! -- INPUTS --
! -- OUTPUTS --
type(st_deftypart) :: deftypart
! -- private data --
! -- BODY --
  call xbin_close(deftypart%defxbin)
endsubroutine typart_close

!------------------------------------------------------------------------------!
! Write a partition to TYPART file
!------------------------------------------------------------------------------!
subroutine typart_writepartition(deftypart, partition)
implicit none
! -- INPUTS --
type(st_deftypart)         :: deftypart
integer(kip), intent(in)   :: partition(:)
! -- OUTPUTS --
! -- private data --
integer :: info
type(st_xbindatasection)   :: xbindata
type(st_connect)           :: rlearray
   
! -- BODY --

call xbin_defdatasection(xbindata, xbinty_part, "PARTITION", &
     (/ deftypart%ncell, deftypart%npart /) )

!------------------------------------------------------------------------------!
! encode array

call rle_encode(deftypart%ncell, partition, rlearray)

call cfd_print("RLE compression by factor "//strof(real(deftypart%ncell)/(2*rlearray%nbnodes),2))

call xbin_writedata_ordint(deftypart%defxbin, xbindata, 2, rlearray%nbnodes, rlearray%fils)

call delete_xbindata(xbindata)
call delete(rlearray)

endsubroutine typart_writepartition

!------------------------------------------------------------------------------!
! read ELEMVTEX
!------------------------------------------------------------------------------!
subroutine typhonread_elemvtex(defxbin, xbintype, elemvtex)
implicit none
! -- INPUTS --
type(st_defxbin)         :: defxbin
integer(xbinkpp)         :: xbintype
! -- OUTPUTS --
type(st_elemvtex)        :: elemvtex
! -- private data --
integer                       :: info
type(st_xbindatasection)      :: xbindata
integer(xbinkip), allocatable :: icell(:,:)
integer(xbinkip)              :: nelem, nvtex, ic

!------------------------------------------------------------------------------!
! check ELEMENT header

call xbin_readdatahead(defxbin, xbindata)

if (xbindata%usertype /= xbintype) then
  select case(xbintype)
  case(xbinty_cells)
    call cfd_error("XBIN/TYPHON error: expecting CELL data section")
  case(xbinty_faces)
    call cfd_error("XBIN/TYPHON error: expecting FACE data section")
  case default
    call cfd_error("XBIN/TYPHON error: expecting !unknown! data section")
  endselect
endif

if (xbindata%nparam >= 1) then
  elemvtex%elemtype = xbindata%param(1)
else
  call cfd_error("XBIN/TYPHON error: expecting parameters in CELL data section")
endif

!------------------------------------------------------------------------------!
! read CELL elements

nelem  = xbindata%nelem
nvtex  = xbindata%dim
allocate(elemvtex%ielem(1:nelem))
allocate(icell(1:nvtex, 1:nelem))

call xbin_readdata_indint(defxbin, xbindata, elemvtex%ielem, icell)

allocate(elemvtex%elemvtex(1:nelem,1:nvtex))
do ic = 1, nelem
  elemvtex%elemvtex(ic,1:nvtex) = icell(1:nvtex, ic)
enddo

elemvtex%nelem    = nelem
elemvtex%nvtex    = nvtex
if (nvtex_element(elemvtex%elemtype) /= elemvtex%nvtex) then
  call cfd_error("non-matching number of vertices in XBIN CELL section")
endif

call delete_xbindata(xbindata)
deallocate(icell)

endsubroutine typhonread_elemvtex


endmodule TYFMT_PART
!------------------------------------------------------------------------------!
! Changes
!
! Apr  2014: created
!------------------------------------------------------------------------------!
