!-----------------------------------------------------------------------------!
! Procedure : writevtk_vect
!
! Function
!   Write vector field to legacy VTK format (ASCII or BINARY)
!
!------------------------------------------------------------------------------!
subroutine writevtk_vect(defio, umesh, name, vecfld)

use OUTPUT
use PACKET
use MENU_GEN
use USTMESH
use DEFFIELD

implicit none

! -- INPUTS --
type(mnu_output)  :: defio         ! output parameter
type(st_ustmesh)  :: umesh         ! unstructured mesh
type(st_vecfield) :: vecfld        ! champ de valeurs
character(len=*)  :: name          ! nom de la variable

! -- Private Data --
integer, parameter :: maxbuffer = 1000
integer   :: i, ielem, q, nelem
integer   :: npaq, dimpaq, dimpaq1, dim, ista, iend
real(8)   :: value(3, maxbuffer)

! -- BODY --

select case(defio%format)
case(fmt_VTK)
  write(defio%iunit,'(A)')   'VECTORS '//trim(name)//' double' 
case(fmt_VTKBIN)
  call writestr(defio%iunit, 'VECTORS '//trim(name)//' double')
case default
  call error_stop("Internal error (writevtk_vect): unknown output format parameter")
endselect

do ielem = 1, umesh%cellvtex%nsection

  nelem = umesh%cellvtex%elem(ielem)%nelem
  call calc_buffer(nelem, maxbuffer, npaq, dimpaq, dimpaq1)
  
  ista = 1
  dim  = dimpaq1

  do q = 1, npaq
    iend = ista-1+dim
    do i = 1, dim
      value(1:3, i) = tab(vecfld%vect(umesh%cellvtex%elem(ielem)%ielem(ista-1+i)))   ! indirection & real*8
    enddo
    select case(defio%format)
    case(fmt_VTK)
      write(defio%iunit, '(:,1P,3E17.8E3)') value(1:3, 1:dim)
    case(fmt_VTKBIN)
      write(defio%iunit) value(1:3, 1:dim)
    case default
      call error_stop("Internal error (writevtk_vect): unknown output format parameter")
    endselect
    ista = iend+1
    dim  = dimpaq
  enddo

enddo

if (defio%format == fmt_VTKBIN) call writereturn(defio%iunit)

endsubroutine writevtk_vect
!------------------------------------------------------------------------------!
! Changes history
!
! April 2006 : subroutine creation, from output_vtkbin_cell
! Oct   2009 : merge VTK and VTK-BIN, write blocks of data
!------------------------------------------------------------------------------!
