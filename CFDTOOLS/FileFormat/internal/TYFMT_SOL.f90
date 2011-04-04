!------------------------------------------------------------------------------!
! MODULE : TYFMT_SOL
!
!------------------------------------------------------------------------------!

module TYFMT_SOL

use MESHPREC         ! Precision configuration
use QUANTITY
use XBIN_DATA
use TYPHON_FMT
use GENFIELD
use USTMESH

implicit none

! -- Global Variables -------------------------------------------


! -- DECLARATIONS -----------------------------------------------------------

! -- XBIN DATA section TYPE --


contains 
!------------------------------------------------------------------------------!

!------------------------------------------------------------------------------!
! write SOLUTION 
!------------------------------------------------------------------------------!
subroutine typhonwrite_sol(defxbin, umesh, gfield)
implicit none
! -- INPUTS --
type(st_defxbin)         :: defxbin
type(st_ustmesh)         :: umesh
type(st_genericfield)    :: gfield
! -- OUTPUTS --
! -- private data --
integer :: info
type(st_xbindatasection)   :: xbindata
real(xbinkrp), allocatable :: sol(:,:)
integer(xbinkip)           :: nelem, dim, i, isca, ivec

!------------------------------------------------------------------------------!
! define  header

nelem = gfield%dim

call xbin_defdatasection(xbindata, xbinty_solution, "SOLUTION", &
                         (/ nelem, gfield%nscal, gfield%nvect /) )
call xbin_writedata_nodata(defxbin, xbindata)
call delete_xbindata(xbindata)

!------------------------------------------------------------------------------!
! write SCALARS

do isca = 1, gfield%nscal
  call xbin_defdatasection(xbindata, xbinty_scalar, quantity_name(gfield%tabscal(isca)%quantity_id), &
                           (/ gfield%tabscal(isca)%quantity_id /) )
  call xbin_writedata_ordreal(defxbin, xbindata, nelem, gfield%tabscal(isca)%scal(1:nelem))
  call delete_xbindata(xbindata)
enddo

dim = 3
allocate(sol(1:dim,1:nelem))
do ivec = 1, gfield%nvect
  call xbin_defdatasection(xbindata, xbinty_vector, quantity_name(gfield%tabvect(ivec)%quantity_id), &
                           (/ gfield%tabvect(ivec)%quantity_id /) )
  call xbin_writedata_ordreal(defxbin, xbindata, nelem, dim, sol)
  call delete_xbindata(xbindata)
enddo
deallocate(sol)

endsubroutine typhonwrite_sol

!------------------------------------------------------------------------------!
! read NODES coordinates of a mesh
!------------------------------------------------------------------------------!
subroutine typhonread_nodes(defxbin, mesh)
implicit none
! -- INPUTS --
type(st_defxbin)         :: defxbin
! -- OUTPUTS --
type(st_mesh)            :: mesh
! -- private data --
integer :: info
type(st_xbindatasection)   :: xbindata
real(xbinkrp), allocatable :: vtex(:,:)
integer(xbinkip)           :: nelem, dim, i

!------------------------------------------------------------------------------!
! check header

call xbin_readdatahead(defxbin, xbindata)

if ((xbindata%name /= "NODES").or.(xbindata%usertype /= xbinty_nodes)) then
  call cfd_error("XBIN/TYPHON error: expecting NODE data section")
endif

!if (xbindata%nparam >= 1) then
!  elemvtex%elemtype = xbindata%param(1)
!else
!  call cfd_error("XBIN/TYPHON error: expecting parameters in CELL data section")
!endif

!------------------------------------------------------------------------------!
! read NODES coordinates

allocate(vtex(1:xbindata%dim,1:xbindata%nelem))

call xbin_readdata_ordreal(defxbin, xbindata, vtex)

mesh%nvtex = xbindata%nelem
allocate(mesh%vertex(xbindata%nelem, 1, 1))

do i = 1, xbindata%nelem
  mesh%vertex(i,1,1)%x = vtex(1,i) 
  mesh%vertex(i,1,1)%y = vtex(2,i) 
  mesh%vertex(i,1,1)%z = vtex(3,i) 
enddo

call delete_xbindata(xbindata)
deallocate(vtex)

endsubroutine typhonread_nodes



endmodule TYFMT_SOL
!------------------------------------------------------------------------------!
! Changes
!
! Apr  2011: created, save and read solution sections
!------------------------------------------------------------------------------!
