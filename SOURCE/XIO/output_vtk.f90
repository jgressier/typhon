!------------------------------------------------------------------------------!
! Procedure : output_vtk                  Auteur : J. Gressier
!                                         Date   : Avril 2004
! Fonction                                Modif  : (cf historique)
!   Ecriture fichier des champs de chaque zone au format VTK
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
 
subroutine output_vtk(nom, world, outp_typ, position, io) 

use TYPHMAKE
use OUTPUT
use VARCOM
use MODWORLD

implicit none

! -- Declaration des entrées --
character(len=strlen) :: nom       ! nom du fichier
type(st_world)        :: world
integer               :: outp_typ
integer               :: position 
integer               :: io       !DEV2602

! -- Declaration des sorties --

! -- Declaration des variables internes --
integer               :: izone, i, dim, ufc, ir
integer               :: info
type(st_genericfield) :: vfield

! -- Debut de la procedure --

if (position == end_calc) then 

  if ((outp_typ == outp_NODE).or.(outp_typ == outp_CENTER)) then !DEV2602

    ! DEVELOPPEMENT PROVISOIRE
    open(unit=uf_chpresu, file=trim(nom), form='formatted', iostat = info)

    do izone = 1, world%prj%nzone

      select case(world%zone(izone)%defsolver%typ_solver)

      case(solNS)
        write(uf_chpresu,'(a)') '# vtk DataFile Version 2.0'
        write(uf_chpresu,'(a)') 'TYPHON-NS'
        write(uf_chpresu,'(a)') 'ASCII'
        call output_vtk_cell(uf_chpresu, world%zone(izone)%defsolver, &
                             world%zone(izone)%grid%umesh, world%zone(izone)%grid%field)

      case(solKDIF)

        write(uf_chpresu,'(a)') '# vtk DataFile Version 2.0'
        write(uf_chpresu,'(a)') 'TYPHON-KDIF'
        write(uf_chpresu,'(a)') 'ASCII'
        call output_vtk_cell(uf_chpresu, world%zone(izone)%defsolver, &
                             world%zone(izone)%grid%umesh, world%zone(izone)%grid%field)

      case(solVORTEX)

        call erreur("Développement","les sorties VORTEX ne sont pas prévues dans ce format")

      case default
        call erreur("Développement","solveur inconnu (output_vtk)")

      endselect

    enddo ! fin boucle : zone

    close(uf_chpresu)

  endif

endif ! position = end_calc

endsubroutine output_vtk

!------------------------------------------------------------------------------!
! Historique des modifications
!
! avr  2004 : création de la procédure
!------------------------------------------------------------------------------!
