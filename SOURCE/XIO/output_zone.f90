!------------------------------------------------------------------------------!
! Procedure : output_zone
!
! Function
!   Write the field of each zone
!
!------------------------------------------------------------------------------!
subroutine output_zone(nom, defio, zone)

use TYPHMAKE
use OUTPUT
use VARCOM
use DEFZONE
use MENU_GEN

implicit none

! -- INPUTS --
character(len=longname) :: nom       ! filename
type(mnu_output)      :: defio     ! output parameter
type(st_zone)         :: zone      ! zone

! -- OUPUTS --

! -- Internal variables --
integer               :: izone, i, dim, ufc, ir
integer               :: info
type(st_genericfield) :: vfield
character(len=10)     :: suffix

! -- BODY --

select case(defio%dataset)
case(dataset_node)
case(dataset_cell)
!case(dataset_bococell)
!case(dataset_bococell)
case default
  call erreur("internal error(output_zone)", "unknown dataset parameter")
endselect

select case(defio%format)

case(fmt_VTK)

  call print_info(2,"* write VTK file: " // trim(nom))
  call output_vtk(nom, defio, zone)

case(fmt_VTKBIN)

  call print_info(2,"* write VTK Binary file: " // trim(nom))
  call output_vtkbin(nom, defio, zone)

case(fmt_TECPLOT)

  call print_info(2,"* write TECPLOT file: " // trim(nom))
  call output_tecplot(nom, defio, zone)

case(fmt_VIGIE)
  call erreur("Development","VIGIE format not implemented")

case(fmt_CGNS, fmt_CGNS_linked)
  call print_info(2,"* write CGNS file: " // trim(nom))
  call output_cgns(nom, defio, zone) 


case default
  call erreur("Internal error (output_zone)","unknown output format parameter")

endselect

endsubroutine output_zone
!------------------------------------------------------------------------------!
! Changes history
!
! May  2008: creation from output_result
!------------------------------------------------------------------------------!
