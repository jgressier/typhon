!------------------------------------------------------------------------------!
! MODULE : MODWORLD                       Auteur : J. Gressier
!                                         Date   : Novembre 2002
! Fonction                                Modif  : Juin 2003
!   Definition des structures de donnees generales
!   Encapsulation de toutes les structures
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

module MODWORLD

use TYPHMAKE       ! Definition de la precision
use MENU_GEN       ! Definition des parametres generaux
use MENU_COUPLING  ! Definition des parametre de couplage
use MODINFO        ! Definition des informations generales
use DEFZONE        ! Definition des zones (maillages)

implicit none

! -- Variables globales du module -------------------------------------------



! -- DECLARATIONS -----------------------------------------------------------

!------------------------------------------------------------------------------!
! Definition de la structure ST_WORLD : ensemble des donnees
!------------------------------------------------------------------------------!
type st_world
  type(mnu_project)   :: prj        ! parametres generaux du projet
  type(st_info)       :: info       ! informations generales sur le calcul
  type(st_zone), dimension(:), pointer &
                      :: zone       ! liste de zones
  integer             :: noutput    ! nombre de definition des sorties
  type(mnu_output), dimension(:), pointer &
                      :: output     ! liste des sorties
  !integer             :: ncoupling  ! nombre de couplages entre zones
  type(mnu_coupling), dimension(:),pointer &
                      :: coupling   ! parametres generaux de couplage
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
! Procedure : allocation d'une structure WORLD
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
! Procedure : desallocation d'une structure WORLD
!------------------------------------------------------------------------------!
subroutine delete_world(world)
implicit none
type(st_world)   :: world
integer          :: i     

  do i = 1, world%prj%nzone
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
! Nov  2002 : creation du module
!------------------------------------------------------------------------------!
