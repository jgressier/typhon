!------------------------------------------------------------------------------!
! Procedure : output_vtk 
!                        
! Function
!   Write the field of each zone in a VTK file
!
!------------------------------------------------------------------------------!
subroutine output_vtk(nom, defio, zone)

use TYPHMAKE
use OUTPUT
use VARCOM
use DEFZONE
use MENU_GEN

implicit none

! -- INPUTS --
character(len=strlen) :: nom       ! filename
type(mnu_output)      :: defio     ! output parameter
type(st_zone)         :: zone      ! zone

! -- OUPUTS --

! -- Internal variables --
integer               :: dim, ufc, ir
integer               :: info
type(st_genericfield) :: vfield
character(len=10)     :: suffix

! -- BODY --

if (mpi_run) then
  suffix = "_p"//strof_full_int(myprocid,3)//".vtk"
else
  suffix = ".vtk"
endif

open(unit=uf_chpresu, file=trim(nom)//trim(suffix), form='formatted', iostat = info)

select case(zone%defsolver%typ_solver)

case(solNS)
  write(uf_chpresu,'(A)') '# vtk DataFile Version 2.0'
  write(uf_chpresu,'(A)') 'TYPHON-NS'
  write(uf_chpresu,'(A)') 'ASCII'
  call output_vtk_cell(uf_chpresu, zone%defsolver, &
       zone%gridlist%first%umesh, zone%gridlist%first%info%field_loc)

case(solKDIF)

  write(uf_chpresu,'(A)') '# vtk DataFile Version 2.0'
  write(uf_chpresu,'(A)') 'TYPHON-KDIF'
  write(uf_chpresu,'(A)') 'ASCII'
  call output_vtk_cell(uf_chpresu, zone%defsolver, &
       zone%gridlist%first%umesh, zone%gridlist%first%info%field_loc)

case(solVORTEX)

  call erreur("Developpement","les sorties VORTEX ne sont pas prevues dans ce format")

case default

  call erreur("Developpement","solveur inconnu (output_vtk)")

endselect

close(uf_chpresu)


endsubroutine output_vtk
!------------------------------------------------------------------------------!
! Changes history
!
! avr  2004 : creation de la procedure
! oct  2004 : field chained list
! Oct  2005 : add suffix with proc number
!------------------------------------------------------------------------------!
