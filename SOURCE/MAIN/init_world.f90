!------------------------------------------------------------------------------!
! Procedure : init_world                  Auteur : J. Gressier
!                                         Date   : Novembre 2002
! Fonction                                Modif  :
!   Initialisation des structures et contenus
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

subroutine init_world(world)

use TYPHMAKE
use OUTPUT
use MODWORLD

implicit none

! -- Declaration des entrees/sorties --
type(st_world) :: world

! -- Declaration des sorties --

! -- Declaration des variables internes --
integer :: izone, icoupling
integer :: iz1, iz2, ncoupl1, ncoupl2, nbc1, nbc2

! -- Debut de la procedure --


!--------------------------------------------------------------------
! Initialization of zone parameters

call print_info(5,"* Initializing zones")
do izone = 1, world%prj%nzone
  call init_zone(world%zone(izone), world%prj)
enddo


!--------------------------------------------------------------------
! Lecture, transformation des maillages, calcul des parametres geometriques et connectivites

call print_info(5,"* Computing mesh properties")
do izone = 1, world%prj%nzone
  call init_maillage(world%zone(izone))
enddo

!--------------------------------------------------------------------
! Initialisation des connectivites cellules/faces/sommets des conditions aux limites

call print_info(5,"* Calcul et Initialisation des connectivites&
                  & et conditions aux limites")
do izone = 1, world%prj%nzone
  call init_connect(world%zone(izone))
enddo

!--------------------------------------------------------------------
! Split grids

if (world%info%nbproc > 1) then
  call print_info(5,"* Splitting grids")
  do izone = 1, world%prj%nzone
    call split_zone(world%zone(izone))
  enddo
endif

!--------------------------------------------------------------------
! Initialisation des champs

call print_info(5,"* Computing initial conditions")
do izone = 1, world%prj%nzone
  call init_champ(world%zone(izone))
enddo

!--------------------------------------------------------------------
! Initialisation des conditions limites

call print_info(5,"* Initializing boundary conditions")
do izone = 1, world%prj%nzone
  call init_boco(world%zone(izone))
enddo

!--------------------------------------------------------------------
! Initialisation des echanges entre zones

do icoupling = 1,  world%prj%ncoupling
  call print_info(5,"Calcul et Initialisation des echanges entre zones")
  call calcul_raccord(world, icoupling, iz1, iz2, ncoupl1, ncoupl2, nbc1, &
                      nbc2)
  call init_coupling(world%zone(iz1), world%zone(iz2), nbc1, nbc2, ncoupl1, &
                     ncoupl2, world%coupling(icoupling)%boco)
enddo

!--------------------------------------------------------------------
! Initialisation des capteurs

call print_info(5,"* Initializing probes & monitors")
do izone = 1, world%prj%nzone
  call init_capteurs(world%zone(izone))
enddo

 



endsubroutine init_world

!------------------------------------------------------------------------------!
! Historique des modifications
!
! nov   2002 : creation de la procedure
! jan   2004 : initialisation des capteurs
!------------------------------------------------------------------------------!
