!------------------------------------------------------------------------------!
! Procedure : init_world 
!
! Fonction
!   Initialisation des structures et contenus
!
!------------------------------------------------------------------------------!
subroutine init_world(world)

use TYPHMAKE
use OUTPUT
use MODWORLD

implicit none

! -- INPUTS/OUTPUTS --
type(st_world) :: world

! -- Private Data --
integer :: izone, icoupling
integer :: iz1, iz2, ncoupl1, ncoupl2, nbc1, nbc2

! -- BODY --

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
  call zone_preproc(world%zone(izone))
enddo

!--------------------------------------------------------------------
! Initialisation des champs

call print_info(5,"* Computing initial conditions")
do izone = 1, world%prj%nzone
  call initzone_field(world%zone(izone))
enddo

!--------------------------------------------------------------------
! Initialization of boundary conditions

call print_info(5,"* Initializing boundary conditions")
do izone = 1, world%prj%nzone
  call init_boco(world%zone(izone))
enddo

!--------------------------------------------------------------------
! Initialisation des echanges entre zones

do icoupling = 1,  world%prj%ncoupling
  call print_info(5,"Computing and Initializing exchanges between zones")
  call calcul_raccord(world, icoupling, iz1, iz2, ncoupl1, ncoupl2, nbc1, &
                      nbc2)
  call init_coupling(world%zone(iz1), world%zone(iz2), nbc1, nbc2, ncoupl1, &
                     ncoupl2, world%coupling(icoupling)%boco)
enddo

!--------------------------------------------------------------------
! Initialization INVERSE specific mode

if (world%prj%time_model == time_unsteady_inverse) then
  call print_info(5,"* Initializing INVERSE solver")
  call init_inverse(world)
endif

!--------------------------------------------------------------------
! Initialization BOCO history

call print_info(5,"* Initializing BOCO history")
do izone = 1, world%prj%nzone
  call init_bocohisto(world%zone(izone))
enddo

!--------------------------------------------------------------------
! Initialization of probes

call print_info(5,"* Initializing probes & monitors")
do izone = 1, world%prj%nzone
  call init_probes(world%zone(izone))
enddo

endsubroutine init_world
!------------------------------------------------------------------------------!
! Changes history
!
! nov   2002 : creation de la procedure
! jan   2004 : initialisation des capteurs
! fev   2007 : English translation
!------------------------------------------------------------------------------!
