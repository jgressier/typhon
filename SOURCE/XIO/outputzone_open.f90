!------------------------------------------------------------------------------!
! Procedure : outputzone_open
!                         
! Function 
!   Open and write header
!
!------------------------------------------------------------------------------!
subroutine outputzone_open(defio, zone)

use TYPHMAKE
use OUTPUT
use VARCOM
use DEFZONE
use MENU_GEN

implicit none

include 'cgnslib_f.h'

! -- INPUTS --
type(mnu_output)      :: defio     ! output parameter
type(st_zone)         :: zone      ! zone

! -- OUPUTS --

! -- Internal variables --
integer               :: dim, ufc, ir
integer               :: ibase          ! CGNS base index
integer               :: iunit, info, nbmesh, nbsol
type(st_genericfield) :: vfield
character(len=10)     :: suffix

! -- BODY --

select case(defio%format)

case(fmt_TECPLOT)
  call error_stop("'Internal error) Unable to use general output with TECPLOT format")

case(fmt_TYPHON)

  suffix = ".tys"
  defio%iunit = getnew_io_unit()
  if (defio%iunit <= 0) call error_stop("IO unit management: impossible to find free unit")
  nbmesh = 1
  nbsol  = 1
  call typhon_openwrite(iunit, trim(defio%filename)//trim(suffix), defio%deftyphon, nbmesh, nbsol)

case(fmt_VTK)

  suffix = ".vtk"
  defio%iunit = getnew_io_unit()
  if (defio%iunit <= 0) call error_stop("IO unit management: impossible to find free unit")
  open(unit=defio%iunit, file=trim(defio%filename)//trim(suffix), form='formatted', iostat = info)
  write(defio%iunit,'(A)') '# vtk DataFile Version 2.0'
  write(defio%iunit,'(A)') 'TYPHON_'//trim(zone%name)
  write(defio%iunit,'(A)') 'ASCII'

case(fmt_VTKBIN)

  suffix = ".vtk"
  defio%iunit = getnew_io_unit()
  if (defio%iunit <= 0) call error_stop("IO unit management: impossible to find free unit")
  open(unit=defio%iunit, file=trim(defio%filename)//trim(suffix), form='binary', iostat = info)
  call writestr(defio%iunit, '# vtk DataFile Version 2.0')
  call writestr(defio%iunit, 'TYPHON_'//trim(zone%name))
  call writestr(defio%iunit, 'BINARY')

case(fmt_CGNS, fmt_CGNS_linked)

  suffix = '.cgns'
  call cg_open_f(trim(defio%filename)//trim(suffix), MODE_WRITE, ufc, info)
  if (info /= 0) &
    call error_stop("CGNS IO error: opening CGNS file (error "//trim(strof(info))//")")
  defio%iunit = ufc

  call cg_base_write_f(defio%iunit, "Z_"//zone%name, dimgeo(zone%gridlist%first%umesh), 3, ibase, info)
  if (info /= 0) &
    call error_stop("CGNS IO error: writing CGNS base (error "//trim(strof(info))//")")
  defio%izone = ibase

case default
  call error_stop("Internal error (outputzone_open): unknown output format parameter")
endselect

endsubroutine outputzone_open
!------------------------------------------------------------------------------!
! Changes history
!
! July  2009 : created
!------------------------------------------------------------------------------!
