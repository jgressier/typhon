!------------------------------------------------------------------------------!
! MODULE : MODWORLD                       Auteur : J. Gressier
!                                         Date   : Novembre 2002
! Fonction                                Modif  : Juin 2003
!   Définition des structures de données générales
!   Encapsulation de toutes les structures
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

module MODWORLD

use TYPHMAKE       ! Definition de la precision
use MENU_GEN       ! Définition des paramètres généraux
use MENU_COUPLING  ! Définition des paramètre de couplage
use MODINFO        ! Définition des informations générales
use DEFZONE        ! Définition des zones (maillages)

implicit none

! -- Variables globales du module -------------------------------------------



! -- DECLARATIONS -----------------------------------------------------------

!------------------------------------------------------------------------------!
! Définition de la structure ST_WORLD : ensemble des données
!------------------------------------------------------------------------------!
type st_world
  type(mnu_project)   :: prj        ! parametres généraux du projet
  type(st_info)       :: info       ! informations générales sur le calcul
  type(st_zone), dimension(:), pointer &
                      :: zone       ! liste de zones
  integer             :: noutput    ! nombre de définition des sorties
  type(mnu_output), dimension(:), pointer &
                      :: output     ! liste des sorties
  !integer             :: ncoupling  ! nombre de couplages entre zones
  type(mnu_coupling), dimension(:),pointer &
                      :: coupling   ! paramètres généraux de couplage
endtype st_world


! -- INTERFACES -------------------------------------------------------------

interface new
  module procedure new_world
endinterface

interface delete
  module procedure delete_world
endinterface

! -- Fonctions et Operateurs ------------------------------------------------


! -- IMPLEMENTATION ---------------------------------------------------------
contains

!------------------------------------------------------------------------------!
! Procédure : allocation d'une structure WORLD
!------------------------------------------------------------------------------!
subroutine new_world(world, nzone, noutput, ncoupling)
implicit none
type(st_world)    :: world
integer           :: nzone
integer, optional :: ncoupling
integer, optional :: noutput

  world%prj%nzone = nzone
  allocate(world%zone(nzone))

  if (present(noutput) .and. (noutput /= 0)) then
    world%noutput = noutput
    allocate(world%output(noutput))
  else
    world%noutput = 0
  endif

  if (present(ncoupling) .and. (ncoupling /= 0)) then
    world%prj%ncoupling = ncoupling
    allocate(world%coupling(ncoupling))
  else
    world%prj%ncoupling = 0
  endif

endsubroutine new_world


!------------------------------------------------------------------------------!
! Procédure : desallocation d'une structure WORLD
!------------------------------------------------------------------------------!
subroutine delete_world(world)
implicit none
type(st_world)   :: world
integer          :: i     

  do i = 1, world%prj%nzone

    print*,"destruction de zone ",i !! DEBUG
    
    call delete(world%zone(i))
  
  enddo

  deallocate(world%zone)

  if (world%noutput > 0) deallocate(world%output)

  if (world%prj%ncoupling > 0) deallocate(world%coupling)

endsubroutine delete_world



endmodule MODWORLD

!------------------------------------------------------------------------------!
! Historique des modifications
!
! Nov  2002 (v0.0.1b): création du module
!------------------------------------------------------------------------------!
