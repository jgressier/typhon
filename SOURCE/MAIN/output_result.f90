!------------------------------------------------------------------------------!
! Procedure : output_result               Auteur : J. Gressier
!                                         Date   : Decembre 2002
! Fonction                                Modif  :
!   Ecriture fichier des champs de chaque zone
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine output_result(world, position) !DEV2602 subroutine output_result(world)
 
use TYPHMAKE
use OUTPUT
use VARCOM
use MODWORLD

implicit none

! -- Declaration des entrees --
type(st_world) :: world
integer        :: position !DEV2602

! -- Declaration des sorties --

! -- Declaration des variables internes --
integer :: i

! -- Debut de la procedure --

do i = 1, world%noutput

  select case(world%output(i)%format)
  case(fmt_VTK)

    call print_info(2,"* sauvegarde au format VTK : " &
                      // trim(world%output(i)%fichier))
    call output_vtk(world%output(i)%fichier, world, world%output(i)%type, position, i) 

  case(fmt_TECPLOT)

    call print_info(2,"* sauvegarde au format TECPLOT : " &
                      // trim(world%output(i)%fichier))
    call output_tecplot(world%output(i)%fichier, world, world%output(i)%type, position, i) 
    !DEV2602 call output_tecplot(world%output(i)%fichier, world, world%output(i)%type)

  case(fmt_VIGIE)
    call erreur("Developpement","format VIGIE non implemente")

  case default
    call erreur("Sauvegarde de resultats","format de fichier inconnu")

  endselect

enddo


endsubroutine output_result
