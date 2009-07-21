!------------------------------------------------------------------------------!
! Procedure : output_vtkbin
!                          
! Function
!   Write the field of each zone in a VTK Binary file
!
!------------------------------------------------------------------------------!
 subroutine output_vtkbin(defio, zone)

use TYPHMAKE
use OUTPUT
use VARCOM
use DEFZONE
use MENU_GEN

implicit none

! -- INPUTS --
type(mnu_output)      :: defio     ! output parameter
type(st_zone)         :: zone      ! zone

! -- OUPUTS --

! -- Internal variables --
integer               :: dim, ufc, ir
integer               :: info
type(st_genericfield) :: vfield
character(len=10)     :: suffix

! -- BODY --

suffix = ".vtk"

open(unit=uf_chpresu, file=trim(defio%filename)//trim(suffix), form='binary', iostat = info)

select case(zone%defsolver%typ_solver)

case(solNS)
  call writestr(uf_chpresu, '# vtk DataFile Version 2.0')
  call writestr(uf_chpresu, 'TYPHON-NS')
  call writestr(uf_chpresu, 'BINARY')
  call output_vtkbin_cell(uf_chpresu, zone%defsolver, &
       zone%gridlist%first%umesh, zone%gridlist%first%info%field_loc)

case(solKDIF)

  call writestr(uf_chpresu, '# vtk DataFile Version 2.0')
  call writestr(uf_chpresu, 'TYPHON-KDIF')
  call writestr(uf_chpresu, 'BINARY')
  call output_vtkbin_cell(uf_chpresu, zone%defsolver, &
       zone%gridlist%first%umesh, zone%gridlist%first%info%field_loc)

case default
  call error_stop("Internal error: unknown solver (output_vtkbin)")
endselect

close(uf_chpresu)


endsubroutine output_vtkbin
!------------------------------------------------------------------------------!
! Changes history
!
! avr  2004 : creation de la procedure
! oct  2004 : field chained list
! Oct  2005 : add suffix with proc number
!------------------------------------------------------------------------------!
