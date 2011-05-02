!------------------------------------------------------------------------------!
! MODULE : VTKMESH
!
!------------------------------------------------------------------------------!
module VTKMESH

use IOCFD
use PACKET
use VTKFMT
use USTMESH

implicit none

! -- Global Variables -------------------------------------------


! -- Parameters --

integer(kpp), parameter :: vtk_bar2   = 3  
integer(kpp), parameter :: vtk_tri3   = 5
integer(kpp), parameter :: vtk_quad4  = 9
integer(kpp), parameter :: vtk_ngon   = 7
integer(kpp), parameter :: vtk_tetra4 = 10
integer(kpp), parameter :: vtk_pyra5  = 14
integer(kpp), parameter :: vtk_penta6 = 13
integer(kpp), parameter :: vtk_hexa8  = 12

!------------------------------------------------------------------------------!
! 
!------------------------------------------------------------------------------!
!type st_defvtk
!endtype st_defvtk

contains 
!------------------------------------------------------------------------------!

!-----------------------------------------------------------------------------!
! writevtk_ustmesh: Write USTMESH structure to legacy VTK file
!------------------------------------------------------------------------------!
subroutine writevtk_ustmesh(defvtk, umesh)
implicit none
! -- INPUTS --
type(st_defvtk),  intent(in) :: defvtk        ! output parameter
type(st_ustmesh), intent(in) :: umesh         ! unstructured mesh

! -- OUTPUTS --

! -- Private DATA --
integer   :: npaq, dimpaq, dimpaq1, dim, ista, iend
integer   :: i, q, nelem, ncellint
integer   :: info, ntot, ielem, vtktype, nvtex
real(8)   :: value(3, vtkbufsize)
type(v3d) :: vtex
character(len=160) :: str

! -- BODY --


! -- header --

select case(defvtk%type)
case(vtk_asc)
  write(defvtk%iunit,'(A)')      'DATASET UNSTRUCTURED_GRID'
  write(defvtk%iunit,'(A,I9,A)') 'POINTS ', umesh%nvtex, ' double'
case(vtk_bin)
  call writestr(defvtk%iunit,'DATASET UNSTRUCTURED_GRID')
  write(str,'(A,I9,A)') 'POINTS ', umesh%nvtex, ' double'
  call writestr(defvtk%iunit, trim(str))
case default
  call cfd_error("Internal error (writevtk_ustmesh): unknown output format parameter")
endselect

! -- vertices coordinates --

nelem = umesh%nvtex
call calc_buffer(nelem, vtkbufsize, npaq, dimpaq, dimpaq1)
  
ista = 1
dim  = dimpaq1

do q = 1, npaq
  iend = ista-1+dim
  do i = 1, dim
    value(1:3, i) = tab(umesh%mesh%vertex(ista-1+i,1,1))   ! indirection & real*8
  enddo

  select case(defvtk%type)
  case(vtk_asc)
    write(defvtk%iunit,'(1P,3E17.8E3)') value(1:3, 1:dim)
  case(vtk_bin)
    write(defvtk%iunit) value(1:3, 1:dim)
  endselect

  ista = iend+1
  dim  = dimpaq
enddo
if (defvtk%type == vtk_bin) call writereturn(defvtk%iunit)

! CELLVTEX connectivity

ncellint       = umesh%ncell_int
ntot           = 0                       ! number of written integer in CELLVTEX connectivity

do ielem = 1, umesh%cellvtex%nsection
  nvtex = umesh%cellvtex%elem(ielem)%nvtex
  ntot  = ntot + (nvtex+1)*umesh%cellvtex%elem(ielem)%nelem
enddo

select case(defvtk%type)
case(vtk_asc)
  write(defvtk%iunit,'(A,2i10)') 'CELLS ', ncellint, ntot
case(vtk_bin)
  write(str,'(A,2I10)') 'CELLS ', ncellint, ntot
  call writestr(defvtk%iunit, trim(str))
endselect

do ielem = 1, umesh%cellvtex%nsection

  nvtex = umesh%cellvtex%elem(ielem)%nvtex
  do i = 1, umesh%cellvtex%elem(ielem)%nelem
    select case(defvtk%type)
    case(vtk_asc)
      write(defvtk%iunit,'(i3,'//strof(nvtex+1)//'I9)') nvtex, umesh%cellvtex%elem(ielem)%elemvtex(i,1:nvtex)-1
    case(vtk_bin)
      write(defvtk%iunit) nvtex, umesh%cellvtex%elem(ielem)%elemvtex(i,1:nvtex)-1
    endselect
  enddo

enddo

! -- CELL TYPES --

select case(defvtk%type)
case(vtk_asc)
  write(defvtk%iunit,'(A,I9)') 'CELL_TYPES ', ncellint
case(vtk_bin)
  write(str,'(A,I9,A)') 'CELL_TYPES ', ncellint, ' int'
  call writestr(defvtk%iunit, trim(str))
endselect

do ielem = 1, umesh%cellvtex%nsection

  vtktype = typhon2vtk_elemtype(umesh%cellvtex%elem(ielem)%elemtype)
  if (vtktype <= 0) call cfd_error("VTK writer: do not known how to write this element type")

  do i = 1, umesh%cellvtex%elem(ielem)%nelem
    select case(defvtk%type)
    case(vtk_asc)
      write(defvtk%iunit,'(I2)') vtktype
    case(vtk_bin)
      write(defvtk%iunit) vtktype
    endselect
  enddo

enddo

if (defvtk%type == vtk_bin) call writereturn(defvtk%iunit)

endsubroutine writevtk_ustmesh
!------------------------------------------------------------------------------!
! Changes history
! avr  2004 : created (legacy output_vtk_cell)
! juin 2004 : write CELL_DATA format
! july 2004 : extension to NS solver outputs (write scalar and vector fields)
! mar  2006 : bug correction (umesh%ncell was changed by outputs)
! July 2009 : created (from output_vtk_cell)
!------------------------------------------------------------------------------!

!------------------------------------------------------------------------------!
! Function : transfer element TYPE
!------------------------------------------------------------------------------!
integer function typhon2vtk_elemtype(itype)
implicit none
! -- dummy arguments --
integer(kpp),      intent(in)  :: itype

select case(itype)
case(elem_bar2)
  typhon2vtk_elemtype = vtk_bar2
case(elem_tri3)
  typhon2vtk_elemtype = vtk_tri3
case(elem_quad4)
  typhon2vtk_elemtype = vtk_quad4
case(elem_ngon)
  typhon2vtk_elemtype = vtk_ngon
case(elem_tetra4)
  typhon2vtk_elemtype = vtk_tetra4
case(elem_pyra5)
  typhon2vtk_elemtype = vtk_pyra5
case(elem_penta6)
  typhon2vtk_elemtype = vtk_penta6
case(elem_hexa8)
  typhon2vtk_elemtype = vtk_hexa8
case default
  typhon2vtk_elemtype = -1
endselect

endfunction typhon2vtk_elemtype

!------------------------------------------------------------------------------!
! Function : transfer element TYPE
!------------------------------------------------------------------------------!
!!$integer function vtk2typhon_elemtype(itype)
!!$implicit none
!!$! -- dummy arguments --
!!$integer(kpp),      intent(in)  :: itype
!!$
!!$select case(itype)
!!$case(bar_2)
!!$  vtk2typhon_elemtype = elem_BAR2
!!$case(tri_3)
!!$  vtk2typhon_elemtype = elem_TRI3
!!$case(quad_4)
!!$  vtk2typhon_elemtype = elem_QUAD4
!!$case(ngon_n)
!!$  vtk2typhon_elemtype = elem_NGON
!!$case(tetra_4)
!!$  vtk2typhon_elemtype = elem_TETRA4
!!$case(pyra_5)
!!$  vtk2typhon_elemtype = elem_PYRA5
!!$case(penta_6)
!!$  vtk2typhon_elemtype = elem_PENTA6
!!$case(hexa_8)
!!$  vtk2typhon_elemtype = elem_HEXA8
!!$case default
!!$  vtk2typhon_elemtype = -1
!!$endselect

!endfunction vtk2typhon_elemtype

endmodule VTKMESH
!------------------------------------------------------------------------------!
! Changes
!
! May  2011: created, from TYPHON/SOURCE/XIO/writevtk_ustmesh
!------------------------------------------------------------------------------!
