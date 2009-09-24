!-----------------------------------------------------------------------------!
! Procedure :writevtk_sol
! 
! Function
!   Write USTMESH structure to legacy VTK file (from legacy output_vtk_cell.f90)
!
!------------------------------------------------------------------------------!
subroutine writevtk_sol(defio, defsolver, umesh, field)

use TYPHMAKE
use OUTPUT
use VARCOM
use GEO3D
use MENU_GEN
use USTMESH
use DEFFIELD
use MENU_SOLVER

implicit none

! -- INPUTS --
type(mnu_output)             :: defio         ! output parameter
type(mnu_solver)             :: defsolver     ! solver model
type(st_ustmesh), intent(in) :: umesh         ! unstructured mesh
type(st_field)               :: field

! -- OUTPUTS --

! -- Internal variables --
integer   :: i, ncellint, isca, ivec
integer   :: info, ntot, ielem, vtktype, nvtex
type(v3d) :: vtex
character(len=shortname) :: qname

! -- BODY --


! -- header --

ncellint = umesh%ncell_int

select case(defio%format)
case(fmt_VTK)
  write(defio%iunit,'(A,I9)') 'CELL_DATA ', ncellint
case(fmt_VTKBIN)
  write(str_w,'(A,I9)') 'CELL_DATA ', ncellint
  call writestr(defio%iunit, trim(str_w))
case default
  call error_stop("Internal error (writevtk_sol): unknown output format parameter")
endselect

do isca = 1, field%etatprim%nscal

  qname = quantity_cgnsname(defsolver%idsca(isca))

  select case(defio%format)
  case(fmt_VTK)
    call output_vtk_scal(defio%iunit, umesh, trim(qname),  field%etatprim%tabscal(isca))
  case(fmt_VTKBIN)
    call output_vtkbin_scal(defio%iunit, umesh, trim(qname),  field%etatprim%tabscal(isca))
  endselect

enddo

do ivec = 1, field%etatprim%nvect

  qname = quantity_cgnsname(defsolver%idvec(ivec))

  select case(defio%format)
  case(fmt_VTK)
    call output_vtk_vect(defio%iunit, umesh, trim(qname),  field%etatprim%tabvect(ivec))
  case(fmt_VTKBIN)
    call output_vtkbin_vect(defio%iunit, umesh, trim(qname),  field%etatprim%tabvect(ivec))
  endselect

enddo

endsubroutine writevtk_sol
!------------------------------------------------------------------------------!
! Changes history
!
! avr  2004 : created (legacy output_vtk_cell)
! juin 2004 : write CELL_DATA format
! july 2004 : extension to NS solver outputs (write scalar and vector fields)
! mar  2006 : bug correction (umesh%ncell was changed by outputs)
! July 2009 : created (from output_vtk_cell)
!------------------------------------------------------------------------------!
