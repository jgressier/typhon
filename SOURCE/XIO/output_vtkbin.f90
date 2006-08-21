!------------------------------------------------------------------------------!
! Procedure : output_vtkbin                      Authors : J. Gressier
!                                                Created : April 2006
! Function
!   Write VTK Binary file
!
!------------------------------------------------------------------------------!
 
subroutine output_vtkbin(nom, world, outp_typ, position, io) 

use TYPHMAKE
use OUTPUT
use VARCOM
use MODWORLD

implicit none

! -- INPUTS --
character(len=strlen) :: nom       ! nom du fichier
type(st_world)        :: world
integer               :: outp_typ
integer               :: position 
integer               :: io       !DEV2602

! -- OUPUTS --

! -- Internal variables --
integer               :: izone, i, dim, ufc, ir
integer               :: info
type(st_genericfield) :: vfield
character(len=10)     :: suffix

! -- BODY --

if (mpi_run) then
  suffix = "_p"//strof_full_int(myprocid,3)//".vtk"
else
  suffix = ".vtk"
endif

select case(position)
case(end_calc, end_cycle)

  if ((outp_typ == outp_NODE).or.(outp_typ == outp_CENTER)) then

    open(unit=uf_chpresu, file=trim(nom)//trim(suffix), form='binary', iostat = info)

    do izone = 1, world%prj%nzone

      select case(world%zone(izone)%defsolver%typ_solver)

      case(solNS)
        call writestr(uf_chpresu, '# vtk DataFile Version 2.0')
        call writestr(uf_chpresu, 'TYPHON-NS')
        call writestr(uf_chpresu, 'BINARY')
        call output_vtkbin_cell(uf_chpresu, world%zone(izone)%defsolver, &
                             world%zone(izone)%grid%umesh, world%zone(izone)%grid%info%field_loc)

      case(solKDIF)

        call writestr(uf_chpresu, '# vtk DataFile Version 2.0')
        call writestr(uf_chpresu, 'TYPHON-KDIF')
        call writestr(uf_chpresu, 'BINARY')
        call output_vtkbin_cell(uf_chpresu, world%zone(izone)%defsolver, &
                             world%zone(izone)%grid%umesh, world%zone(izone)%grid%info%field_loc)

      case(solVORTEX)

        call erreur("Developpement","les sorties VORTEX ne sont pas prevues dans ce format")

      case default
        call erreur("Developpement","solveur inconnu (output_vtkbin)")

      endselect

    enddo ! fin boucle : zone

    close(uf_chpresu)

  endif

case default
  print*,"!DEV! blanked output_vtkbin"
endselect

endsubroutine output_vtkbin
!------------------------------------------------------------------------------!
! Changes history
!
! avr  2004 : creation de la procedure
! oct  2004 : field chained list
! Oct  2005 : add suffix with proc number
!------------------------------------------------------------------------------!
