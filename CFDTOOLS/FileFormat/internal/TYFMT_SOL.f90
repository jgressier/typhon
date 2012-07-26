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
subroutine typhonwrite_sol(deftyphon, umesh, gfield)
implicit none
! -- INPUTS --
type(st_deftyphon)       :: deftyphon
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
                         (/ gfield%type, nelem, gfield%ncell, gfield%dof, gfield%nscal, gfield%nvect /) )
call xbin_writedata_nodata(deftyphon%defxbin, xbindata)
call delete_xbindata(xbindata)

!------------------------------------------------------------------------------!
! write SCALARS & VECTORS

do isca = 1, gfield%nscal
  call xbin_defdatasection(xbindata, xbinty_scalar, quantity_name(gfield%tabscal(isca)%quantity_id), &
                           (/ gfield%tabscal(isca)%quantity_id /) )
  call xbin_writedata_ordreal(deftyphon%defxbin, xbindata, nelem, gfield%tabscal(isca)%scal(1:nelem))
  call delete_xbindata(xbindata)
enddo

dim = 3
allocate(sol(1:dim,1:nelem))
do ivec = 1, gfield%nvect
  call xbin_defdatasection(xbindata, xbinty_vector, quantity_name(gfield%tabvect(ivec)%quantity_id), &
                           (/ gfield%tabvect(ivec)%quantity_id /) )
  do i = 1, nelem
    sol(1,i) = gfield%tabvect(ivec)%vect(i)%x
    sol(2,i) = gfield%tabvect(ivec)%vect(i)%y
    sol(3,i) = gfield%tabvect(ivec)%vect(i)%z
  enddo
  call xbin_writedata_ordreal(deftyphon%defxbin, xbindata, nelem, dim, sol)
  call delete_xbindata(xbindata)
enddo
deallocate(sol)

endsubroutine typhonwrite_sol

!------------------------------------------------------------------------------!
! read SOLUTION 
!------------------------------------------------------------------------------!
subroutine typhonread_sol(deftyphon, umesh, gfield)
implicit none
! -- INPUTS --
type(st_deftyphon)       :: deftyphon
type(st_ustmesh)         :: umesh
! -- OUTPUTS --
type(st_genericfield)    :: gfield
! -- private data --
integer :: info
type(st_xbindatasection)   :: xbindata
real(xbinkrp), allocatable :: sol(:,:)
integer(xbinkip)           :: dim, nelem, nsca, nvec, ncell, dof, fieldtype
integer                    :: i, isca, ivec

!------------------------------------------------------------------------------!
! define  header

call xbin_readdatahead(deftyphon%defxbin, xbindata)
call xbin_skipdata(deftyphon%defxbin, xbindata)

if (xbindata%usertype /= xbinty_solution) then
  call cfd_error("XBIN/TYPHON error: expecting SOLUTION data section")
endif

!print*,xbindata%nparam,'//',xbindata%param

select case (deftyphon%xty_version)
case(1:2)
  if (xbindata%nparam == 3) then
    nelem         = xbindata%param(1)
    nsca          = xbindata%param(2)
    nvec          = xbindata%param(3)
    fieldtype     = sol_cell
    dof           = 1
    ncell         = nelem
  else
    call cfd_error("XBIN/TYPHON error: expecting 3 parameters in SOLUTION data section")
  endif
case(3:xty_maxver)
  if (xbindata%nparam == 6) then
    fieldtype     = xbindata%param(1)
    nelem         = xbindata%param(2)
    ncell         = xbindata%param(3)
    dof           = xbindata%param(4)
    nsca          = xbindata%param(5)
    nvec          = xbindata%param(6)
  else
    call cfd_error("XBIN/TYPHON error: expecting 6 parameters in SOLUTION data section")
  endif
case default
  call cfd_error("XBIN/TYPHON error: unexpected version number")
endselect

call delete_xbindata(xbindata)

call new_genericfield(gfield, nelem, nsca, nvec, 0)
gfield%type  = fieldtype
gfield%ncell = ncell
gfield%dof   = dof

!------------------------------------------------------------------------------!
! read SCALARS & VECTORS

do isca = 1, gfield%nscal
  call xbin_readdatahead(deftyphon%defxbin, xbindata)
  if (xbindata%usertype /= xbinty_scalar) then
    call cfd_error("XBIN/TYPHON error: expecting SCALAR SOLUTION data section")
  endif
  gfield%tabscal(isca)%quantity_id = xbindata%param(1)
  call xbin_readdata_ordreal(deftyphon%defxbin, xbindata, gfield%tabscal(isca)%scal(1:nelem))
  call delete_xbindata(xbindata)
enddo

dim = 3
allocate(sol(1:dim,1:nelem))
do ivec = 1, gfield%nvect
  call xbin_readdatahead(deftyphon%defxbin, xbindata)
  if (xbindata%usertype /= xbinty_vector) then
    call cfd_error("XBIN/TYPHON error: expecting VECTOR SOLUTION data section")
  endif
  gfield%tabvect(ivec)%quantity_id = xbindata%param(1)
  call xbin_readdata_ordreal(deftyphon%defxbin, xbindata, sol)
  do i = 1, nelem
    gfield%tabvect(ivec)%vect(i)%x = sol(1,i)
    gfield%tabvect(ivec)%vect(i)%y = sol(2,i)
    gfield%tabvect(ivec)%vect(i)%z = sol(3,i)
  enddo
  call delete_xbindata(xbindata)
enddo
deallocate(sol)

endsubroutine typhonread_sol




endmodule TYFMT_SOL
!------------------------------------------------------------------------------!
! Changes
!
! Apr  2011: created, save and read solution sections
!------------------------------------------------------------------------------!
