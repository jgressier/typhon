!------------------------------------------------------------------------------!
! Procedure : lecture_maillage            Auteur : J. Gressier
!                                         Date   : Novembre 2002
! Fonction                                Modif  :
!   Lecture des maillages
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine lecture_maillage(world)

use TYPHMAKE
use OUTPUT
use MODWORLD

implicit none

! -- Declaration des entrees/sorties --
type(st_world) :: world

! -- Declaration des entrees --

! -- Declaration des sorties --

! -- Declaration des variables internes --
integer :: izone

! -- Debut de la procedure --

do izone = 1, world%prj%nzone

  call lectzone_mesh(world%zone(izone))
  
enddo


endsubroutine lecture_maillage
