!-----------------------------------------------------------------------------!
! Procedure :writevtk_ustmesh
! 
! Function
!   Write USTMESH structure to legacy VTK file (from legacy output_vtk_cell.f90)
!
!------------------------------------------------------------------------------!
subroutine writevtk_ustmesh(defio, umesh)

use TYPHMAKE
use OUTPUT
use VARCOM
use GEO3D
use MENU_GEN
use USTMESH
use DEFFIELD

implicit none

! -- INPUTS --
type(mnu_output)             :: defio         ! output parameter
type(st_ustmesh), intent(in) :: umesh         ! unstructured mesh

! -- OUTPUTS --

! -- Private DATA --
integer, parameter :: maxbuffer = 1000
integer            :: npaq, dimpaq, dimpaq1, dim, ista, iend
integer   :: i, q, nelem, ncellint
integer   :: info, ntot, ielem, vtktype, nvtex
real(8)   :: value(3, maxbuffer)
type(v3d) :: vtex

! -- BODY --


! -- header --

select case(defio%format)
case(fmt_VTK)
  write(defio%iunit,'(A)')      'DATASET UNSTRUCTURED_GRID'
  write(defio%iunit,'(A,I9,A)') 'POINTS ', umesh%nvtex, ' double'
case(fmt_VTKBIN)
  call writestr(defio%iunit,'DATASET UNSTRUCTURED_GRID')
  write(str_w,'(A,I9,A)') 'POINTS ', umesh%nvtex, ' double'
  call writestr(defio%iunit, trim(str_w))
case default
  call error_stop("Internal error (writevtk_ustmesh): unknown output format parameter")
endselect

! -- vertices coordinates --

nelem = umesh%nvtex
call calc_buffer(nelem, maxbuffer, npaq, dimpaq, dimpaq1)
  
ista = 1
dim  = dimpaq1

do q = 1, npaq
  iend = ista-1+dim
  do i = 1, dim
    value(1:3, i) = tab(umesh%mesh%vertex(i,1,1))   ! indirection & real*8
  enddo

  select case(defio%format)
  case(fmt_VTK)
    write(defio%iunit,'(1P,3E17.8E3)') value(1:3, 1:dim)
  case(fmt_VTKBIN)
    write(defio%iunit) value(1:3, 1:dim)
  endselect

  ista = iend+1
  dim  = dimpaq
enddo
if (defio%format == fmt_VTKBIN) call writereturn(defio%iunit)

! CELLVTEX connectivity

ncellint       = umesh%ncell_int
ntot           = 0                       ! number of written integer in CELLVTEX connectivity

do ielem = 1, umesh%cellvtex%ntype
  nvtex = umesh%cellvtex%elem(ielem)%nvtex
  ntot  = ntot + (nvtex+1)*umesh%cellvtex%elem(ielem)%nelem
enddo

select case(defio%format)
case(fmt_VTK)
  write(defio%iunit,'(A,2i10)') 'CELLS ', ncellint, ntot
case(fmt_VTKBIN)
  write(str_w,'(A,2I10)') 'CELLS ', ncellint, ntot
  call writestr(defio%iunit, trim(str_w))
endselect

do ielem = 1, umesh%cellvtex%ntype

  nvtex = umesh%cellvtex%elem(ielem)%nvtex
  do i = 1, umesh%cellvtex%elem(ielem)%nelem
  select case(defio%format)
  case(fmt_VTK)
    write(defio%iunit,'(i3,'//strof(nvtex+1)//'I9)') nvtex, umesh%cellvtex%elem(ielem)%elemvtex(i,1:nvtex)-1
  case(fmt_VTKBIN)
    write(defio%iunit) nvtex, umesh%cellvtex%elem(ielem)%elemvtex(i,1:nvtex)-1
  endselect
  enddo

enddo

! -- CELL TYPES --

select case(defio%format)
case(fmt_VTK)
  write(defio%iunit,'(A,I9)') 'CELL_TYPES ', ncellint
case(fmt_VTKBIN)
  write(str_w,'(A,I9,A)') 'CELL_TYPES ', ncellint, ' int'
  call writestr(defio%iunit, trim(str_w))
endselect

do ielem = 1, umesh%cellvtex%ntype

  select case(umesh%cellvtex%elem(ielem)%elemtype)
  case(elem_bar2)
    vtktype = 3    ! VTK_LINE 
  case(elem_tri3)
    vtktype = 5    ! VTK_TRIANGLE
  case(elem_quad4)
    vtktype = 9    ! VTK_QUAD
  case(elem_ngon)
    vtktype = 7    ! VTK_POLYGON
  case(elem_tetra4)
    vtktype = 10   ! VTK_TETRA
  case(elem_pyra5)
    vtktype = 14   ! VTK_PYRAMID
  case(elem_penta6)
    vtktype = 13   ! VTK_WEDGE
  case(elem_hexa8)
    vtktype = 12   ! VTK_HEXAHEDRON
  case default
    call error_stop("VTK writer: do not known how to write this element type")
  endselect

  do i = 1, umesh%cellvtex%elem(ielem)%nelem
    select case(defio%format)
    case(fmt_VTK)
     write(defio%iunit,'(I2)') vtktype
   case(fmt_VTKBIN)
      write(defio%iunit) vtktype
    endselect
  enddo

enddo

if (defio%format == fmt_VTKBIN) call writereturn(defio%iunit)


endsubroutine writevtk_ustmesh
!------------------------------------------------------------------------------!
! Changes history
!
! avr  2004 : created (legacy output_vtk_cell)
! juin 2004 : write CELL_DATA format
! july 2004 : extension to NS solver outputs (write scalar and vector fields)
! mar  2006 : bug correction (umesh%ncell was changed by outputs)
! July 2009 : created (from output_vtk_cell)
!------------------------------------------------------------------------------!
