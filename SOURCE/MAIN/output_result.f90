!------------------------------------------------------------------------------!
! Procedure : output_result               Auteur : J. Gressier
!                                         Date   : Decembre 2002
! Fonction                                Modif  :
!   Ecriture fichier des champs de chaque zone
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine output_result(world, position)

use TYPHMAKE
use OUTPUT
use VARCOM
use MODWORLD
use MODINFO
use MENU_GEN
use STRING

implicit none

! -- Inputs --
type(st_world) :: world
integer        :: position

! -- Ouputs --

! -- Internal variables --
integer               :: i, iz
character(len=strlen) :: nom       ! file name

! -- BODY --

do i = 1, world%noutput

  if ((position == end_calc).or. &
      (mod(world%info%icycle, world%output(i)%period) == 0)) then

    do iz = 1, world%prj%nzone

      nom = trim(world%output(i)%filename)//trim(world%zone(iz)%name)

      select case(position)
      case(end_calc)
        ! nothing to add
      case(end_cycle)
        nom = trim(nom)//"_cyc"//strof_full_int(world%info%icycle, 4)
      case(in_cycle)
        call print_warning("check output_result call with in_cycle parameter")
      case default
        call erreur("Internal error (output_result)","unknown position parameter in output")
      endselect

      call output_zone(nom, world%output(i), world%zone(iz))

    enddo

  endif

enddo


endsubroutine output_result
!------------------------------------------------------------------------------!
! Changes history
!
! dec  2002 : creation de la procedure
! fev  2007 : correction du nom de fichier pour TECPLOT
! May  2008 : split routine into itself & output_zone
!------------------------------------------------------------------------------!
