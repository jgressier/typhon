!-----------------------------------------------------------------------------!
! Procedure : writevtk_scal                Auteur : J. Gressier
!                                               Date   : April 2006
! Function
!   Write scalar field to legacy VTK format (ASCII or BINARY)
!
!------------------------------------------------------------------------------!
subroutine writevtk_scal(defio, umesh, name, scafld)

use OUTPUT
use PACKET
use MENU_GEN
use USTMESH
use DEFFIELD

implicit none

! -- INPUTS --
type(mnu_output)  :: defio         ! output parameter
type(st_ustmesh)  :: umesh         ! unstructured mesh
type(st_scafield) :: scafld        ! champ de valeurs
character(len=*)  :: name          ! nom de la variable

! -- OUTPUTS --

! -- Private Data --
integer, parameter :: maxbuffer = 1000
integer            :: i, q, nelem, ielem
integer            :: npaq, dimpaq, dimpaq1, dim, ista, iend
real(8)            :: value(maxbuffer)

! -- BODY --

select case(defio%format)
case(fmt_VTK)
  write(defio%iunit,'(A)')    'SCALARS '//trim(name)//' double'
  write(defio%iunit,'(A)')    'LOOKUP_TABLE default'
case(fmt_VTKBIN)
  call writestr(defio%iunit, 'SCALARS '//trim(name)//' double')
  call writestr(defio%iunit, 'LOOKUP_TABLE default')
case default
  call error_stop("Internal error (writevtk_scal): unknown output format parameter")
endselect

do ielem = 1, umesh%cellvtex%nsection

  nelem = umesh%cellvtex%elem(ielem)%nelem
  call calc_buffer(nelem, maxbuffer, npaq, dimpaq, dimpaq1)
  
  ista = 1
  dim  = dimpaq1

  do q = 1, npaq
    iend = ista-1+dim

    value(1:dim) = scafld%scal(umesh%cellvtex%elem(ielem)%ielem(ista:iend))   ! indirection & real*8

    select case(defio%format)
    case(fmt_VTK)
      write(defio%iunit,'(1P,E17.8E3)') value(1:dim)
    case(fmt_VTKBIN)
      write(defio%iunit) value(1:dim)
    case default
      call error_stop("Internal error (writevtk_vect): unknown output format parameter")
    endselect

    ista = iend+1
    dim  = dimpaq
  enddo

enddo

if (defio%format == fmt_VTKBIN) call writereturn(defio%iunit)

endsubroutine writevtk_scal
!------------------------------------------------------------------------------!
! Changes history
!
! April 2006 : subroutine creation, from output_vtkbin_cell
! Oct   2009 : merge VTK and VTK-BIN, write blocks of data
!------------------------------------------------------------------------------!
