!------------------------------------------------------------------------------!
! MODULE : MGRID                          Auteur : J. Gressier
!                                         Date   : Mars 2004
! Fonction                                Modif  : (cf historique)
!   Définition des structures de données des grilles
!   maillage et champ
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

module MGRID

use TYPHMAKE      ! Definition de la precision/données informatiques
use USTMESH       ! Définition des maillages non structurés
use DEFFIELD      ! Définition des champs physiques
use GEO3D        ! module de définition des vecteurs et opérateurs associés

implicit none

! -- Variables globales du module -------------------------------------------



! -- DECLARATIONS -----------------------------------------------------------

!------------------------------------------------------------------------------!
! Définition de la structure ST_GRID : grid maillage général et champ
!------------------------------------------------------------------------------!
type st_grid
  integer                :: id         ! numéro de grid
  integer                :: mpi_cpu    ! numéro de CPU chargé du calcul
  type(st_grid), pointer :: next       ! pointeur de liste chaînée
  type(st_grid), pointer :: subgrid    ! pointeur de liste chaînée
  type(st_ustmesh)       :: umesh      ! maillage non structuré
  integer                :: nfield     ! nombre de champs
  type(st_field), pointer:: field      ! tableau des champs
  integer                :: nbocofield ! nombre de champs génériques
  type(st_genericfield), pointer &
                         :: bocofield  ! liste chaînée de champs génériques
  type(st_field), pointer:: field_loc  ! champ local pour l'intégration
  type(st_field), pointer:: field_cyclestart  ! champ de début de cycle
endtype st_grid


! -- INTERFACES -------------------------------------------------------------

interface new
  module procedure new_grid
endinterface

interface delete
  module procedure delete_grid
endinterface

interface name
  module procedure name_grid
endinterface

! -- Fonctions et Operateurs ------------------------------------------------


! -- IMPLEMENTATION ---------------------------------------------------------
contains


!------------------------------------------------------------------------------!
! Procédure : initialisation d'une structure GRID
!------------------------------------------------------------------------------!
subroutine new_grid(grid, id)
implicit none
type(st_grid)  :: grid
integer        :: id

  grid%id = id
  nullify(grid%next)
  nullify(grid%subgrid)

endsubroutine new_grid


!------------------------------------------------------------------------------!
! Procédure : création et lien chaîné d'une structure GRID
!------------------------------------------------------------------------------!
function insert_newgrid(grid, id) result(pgrid)
implicit none
type(st_grid), pointer :: pgrid
type(st_grid), target  :: grid
integer                :: id

  allocate(pgrid)
  call new(pgrid, id)
  pgrid%next => grid  

endfunction insert_newgrid


!------------------------------------------------------------------------------!
! Procédure : desallocation d'une structure GRID
!------------------------------------------------------------------------------!
subroutine delete_grid(grid)
implicit none
type(st_grid)  :: grid

  ! destruction des champs et maillage de la grille
  call delete(grid%umesh)
  call delete_chainedfield(grid%field)
  deallocate(grid%field_loc)

  ! destruction des sous-grilles
  call delete_chainedgrid(grid%subgrid)

  ! ATTENTION : pas de destruction de la grilles suivante

endsubroutine delete_grid


!------------------------------------------------------------------------------!
! Procédure : desallocation d'une liste chaînée de structure GRID
!------------------------------------------------------------------------------!
subroutine delete_chainedgrid(grid)
implicit none
type(st_grid), target  :: grid
type(st_grid), pointer :: pgrid, dgrid

  pgrid => grid
  do while(associated(pgrid))
    dgrid => pgrid
    pgrid => pgrid%next
    call delete(dgrid)
  enddo

endsubroutine delete_chainedgrid


!------------------------------------------------------------------------------!
! Fonction : nom de grille
!------------------------------------------------------------------------------!
function name_grid(grid) result(str)
implicit none
type(st_grid)         :: grid
character(len=strlen) :: str

  str = "" ! grid%umesh%name

endfunction name_grid

!------------------------------------------------------------------------------!
! Procédure : ajout avec allocation d'une structure champ générique
! (par insertion)
!------------------------------------------------------------------------------!
function newbocofield(grid,dim,nscal,nvect,ntens) result(pbocofield)
implicit none
type(st_genericfield), pointer :: pbocofield
type(st_grid)                  :: grid
integer                        :: dim, nscal, nvect, ntens

  grid%nbocofield = grid%nbocofield + 1

  if (grid%nbocofield == 1) then
   allocate(pbocofield)
   call new(pbocofield,dim,nscal,nvect,ntens)
   nullify(pbocofield%next)
   call init_genericfield(pbocofield,0._krp,v3d(0._krp,0._krp,0._krp))
  else
    pbocofield => insert_newgfield(grid%bocofield,dim,nscal,nvect,ntens)
    call init_genericfield(pbocofield,0._krp,v3d(0._krp,0._krp,0._krp))
  endif
  grid%bocofield => pbocofield

endfunction newbocofield

!------------------------------------------------------------------------------!
! Procédure : ajout avec allocation d'une structure champ (par insertion)
!------------------------------------------------------------------------------!
function newfield(grid,nscal,nvect,ncell,nface) result(pfield)
implicit none
type(st_field), pointer :: pfield
type(st_grid)           :: grid
integer                 :: dim, nscal, nvect, ncell, nface

  grid%nfield = grid%nfield + 1

  if (grid%nfield == 1) then
   allocate(pfield)
   call new(pfield,nscal,nvect,ncell,nface)
   nullify(pfield%next)
  else
    pfield => insert_newfield(grid%field,nscal,nvect,ncell,nface)
  endif
  grid%field => pfield

endfunction newfield


endmodule MGRID

!------------------------------------------------------------------------------!
! Historique des modifications
!
! mars 2004 : création du module
! juin 2004 : procédure newbocofield
! oct  2004 : field chained list
!------------------------------------------------------------------------------!
