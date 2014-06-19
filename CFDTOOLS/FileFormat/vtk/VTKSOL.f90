!------------------------------------------------------------------------------!
! MODULE : VTKSOL
!
!------------------------------------------------------------------------------!
module VTKSOL

use IOCFD
use PACKET
use QUANTITY
use VTKFMT
use USTMESH
use GENFIELD

implicit none

! -- Global Variables -------------------------------------------


! -- Parameters --

!------------------------------------------------------------------------------!
! 
!------------------------------------------------------------------------------!
!type st_defvtk
!endtype st_defvtk

contains 
!------------------------------------------------------------------------------!

!-----------------------------------------------------------------------------!
! writevtk_sol: Write solution structure to legacy VTK file
!------------------------------------------------------------------------------!
subroutine writevtk_sol(defvtk, umesh, gfield, isim, nsim)
implicit none
! -- INPUTS --
type(st_defvtk),       intent(in) :: defvtk        ! output parameter
type(st_ustmesh),      intent(in) :: umesh         ! unstructured mesh
type(st_genericfield), intent(in) :: gfield
integer 	  :: isim 	   ! number of current simulation
integer 	  :: nsim 	   ! Number of simulations
! -- OUTPUTS --
! -- Internal variables --
integer   :: i, ncellint, isca, ivec
integer   :: info, ntot, ielem, vtktype, nvtex
type(v3d) :: vtex
character(len=160) :: str
character(len=30)  :: qname

! -- BODY --


! -- header --

ncellint = umesh%ncell_int

select case(defvtk%type)
case(vtk_asc)
  write(defvtk%iunit,'(A,I9)') 'CELL_DATA ', ncellint
case(vtk_bin)
  write(str,'(A,I9)') 'CELL_DATA ', ncellint
  call writestr(defvtk%iunit, trim(str))
case default
  call cfd_error("Internal error (writevtk_sol): unknown output format parameter")
endselect

do isca = 1, gfield%nscal
  qname = quantity_name(gfield%tabscal(isca)%quantity_id)
  call writevtk_scal(defvtk, umesh, trim(qname),  gfield%tabscal(isca), isim, nsim)
enddo

do ivec = 1, gfield%nvect
  qname = quantity_name(gfield%tabvect(ivec)%quantity_id)
  call writevtk_vect(defvtk, umesh, trim(qname),  gfield%tabvect(ivec), isim, nsim)
enddo

endsubroutine writevtk_sol
!------------------------------------------------------------------------------!
! Changes history
! avr  2004 : created (legacy output_vtk_cell)
! juin 2004 : write CELL_DATA format
! july 2004 : extension to NS solver outputs (write scalar and vector fields)
! mar  2006 : bug correction (umesh%ncell was changed by outputs)
! July 2009 : created (from output_vtk_cell)
!------------------------------------------------------------------------------!

!-----------------------------------------------------------------------------!
! writevtk_scal: Write scalar field to legacy VTK format (ASCII or BINARY)
!------------------------------------------------------------------------------!
subroutine writevtk_scal(defvtk, umesh, name, scafld, isim, nsim)
implicit none
! -- INPUTS --
type(st_defvtk)   :: defvtk        ! VTK output parameters
type(st_ustmesh)  :: umesh         ! unstructured mesh
type(st_scafield) :: scafld        ! scalar field
character(len=*)  :: name          ! variable name
integer 	  :: isim 	   ! number of current simulation
integer 	  :: nsim 	   ! Number of simulations
! -- OUTPUTS --
! -- Private Data --
integer            :: i, q, nelem, ielem
integer            :: npaq, dimpaq, dimpaq1, dim, ista, iend
real(8)            :: value(vtkbufsize)

! -- BODY --

select case(defvtk%type)
case(vtk_asc)
  write(defvtk%iunit,'(A)')    'SCALARS '//trim(name)//' double'
  write(defvtk%iunit,'(A)')    'LOOKUP_TABLE default'
case(vtk_bin)
  call writestr(defvtk%iunit, 'SCALARS '//trim(name)//' double')
  call writestr(defvtk%iunit, 'LOOKUP_TABLE default')
case default
  call cfd_error("Internal error (writevtk_scal): unknown output format parameter")
endselect

do ielem = 1, umesh%cellvtex%nsection

  nelem = umesh%cellvtex%elem(ielem)%nelem
  call calc_buffer(nelem, vtkbufsize, npaq, dimpaq, dimpaq1)
  
  ista = 1
  dim  = dimpaq1

  do q = 1, npaq
    iend = ista-1+dim

    value(1:dim) = scafld%scal(umesh%cellvtex%elem(ielem)%ielem(ista:iend))   ! indirection & real*8

    select case(defvtk%type)
    case(vtk_asc)
      write(defvtk%iunit,'(1P,E17.8E3)') value(1:dim)
    case(vtk_bin)
      write(defvtk%iunit) value(dim*isim:dim*(isim+1))
    case default
      call cfd_error("Internal error (writevtk_vect): unknown output format parameter")
    endselect

    ista = iend+1
    dim  = dimpaq
  enddo

enddo

if (defvtk%type == vtk_bin) call writereturn(defvtk%iunit)

endsubroutine writevtk_scal
!------------------------------------------------------------------------------!
! Changes history
! April 2006 : subroutine creation, from output_vtkbin_cell
! Oct   2009 : merge VTK and VTK-BIN, write blocks of data
!------------------------------------------------------------------------------!


!-----------------------------------------------------------------------------!
! writevtk_vect: Write vector field to legacy VTK format (ASCII or BINARY)
!------------------------------------------------------------------------------!
subroutine writevtk_vect(defvtk, umesh, name, vecfld, isim, nsim)
implicit none
! -- INPUTS --
type(st_defvtk)   :: defvtk        ! VTK output parameter
type(st_ustmesh)  :: umesh         ! unstructured mesh
type(st_vecfield) :: vecfld        ! VECTOR field
character(len=*)  :: name          ! variable name
integer 	  :: isim 	   ! number of current simulation
integer 	  :: nsim 	   ! Number of simulations

! -- Private Data --
integer   :: i, ielem, q, nelem
integer   :: npaq, dimpaq, dimpaq1, dim, ista, iend
real(8)   :: value(3, vtkbufsize)

! -- BODY --

select case(defvtk%type)
case(vtk_asc)
  write(defvtk%iunit,'(A)')   'VECTORS '//trim(name)//' double' 
case(vtk_bin)
  call writestr(defvtk%iunit, 'VECTORS '//trim(name)//' double')
case default
  call cfd_error("Internal error (writevtk_vect): unknown output format parameter")
endselect

do ielem = 1, umesh%cellvtex%nsection

  nelem = umesh%cellvtex%elem(ielem)%nelem
  call calc_buffer(nelem, vtkbufsize, npaq, dimpaq, dimpaq1)
  
  ista = 1
  dim  = dimpaq1

  do q = 1, npaq
    iend = ista-1+dim
    do i = 1, dim
      value(1:3, i) = tab(vecfld%vect(umesh%cellvtex%elem(ielem)%ielem(ista-1+i)))   ! indirection & real*8
    enddo
    select case(defvtk%type)
    case(vtk_asc)
      write(defvtk%iunit, '(:,1P,3E17.8E3)') value(1:3, 1:dim)
    case(vtk_bin)
      write(defvtk%iunit) value(1:3, dim*isim:dim*(isim+1))
    case default
      call cfd_error("Internal error (writevtk_vect): unknown output format parameter")
    endselect
    ista = iend+1
    dim  = dimpaq
  enddo

enddo

if (defvtk%type == vtk_bin) call writereturn(defvtk%iunit)

endsubroutine writevtk_vect
!------------------------------------------------------------------------------!
! Changes history
! April 2006 : subroutine creation, from output_vtkbin_cell
! Oct   2009 : merge VTK and VTK-BIN, write blocks of data
!------------------------------------------------------------------------------!


endmodule VTKSOL
!------------------------------------------------------------------------------!
! Changes
!
! May  2011: created, from TYPHON/SOURCE/XIO/writevtk_sol, 
!------------------------------------------------------------------------------!
