!------------------------------------------------------------------------------!
! MODULE : MGRID                          Auteur : J. Gressier
!                                         Date   : Mars 2004
! Fonction                                Modif  : (cf historique)
!   Definition des structures de donnees des grilles
!   maillage et champ
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

module MGRID

use TYPHMAKE      ! Definition de la precision/donnees informatiques
use USTMESH       ! Definition des maillages non structures
use DEFFIELD      ! Definition des champs physiques
use GEO3D        ! module de definition des vecteurs et operateurs associes

implicit none

! -- Variables globales du module -------------------------------------------



! -- DECLARATIONS -----------------------------------------------------------

!------------------------------------------------------------------------------!
! Definition de la structure ST_GRID : grid maillage general et champ
!------------------------------------------------------------------------------!
type st_grid
  integer                :: id         ! numero de grid
  integer                :: mpi_cpu    ! numero de CPU charge du calcul
  type(st_grid), pointer :: next       ! pointeur de liste chainee
  type(st_grid), pointer :: subgrid    ! pointeur de liste chainee
  type(st_ustmesh)       :: umesh      ! maillage non structure
  integer                :: nfield     ! nombre de champs
  type(st_field), pointer:: field      ! tableau des champs
  integer                :: nbocofield ! nombre de champs generiques
  type(st_genericfield), pointer &
                         :: bocofield  ! liste chainee de champs generiques
  type(st_field), pointer:: field_loc  ! champ local pour l'integration
  type(st_field), pointer:: field_cyclestart  ! champ de debut de cycle
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
! Procedure : initialisation d'une structure GRID
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
! Procedure : creation et lien chaine d'une structure GRID
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
! Procedure : desallocation d'une structure GRID
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
! Procedure : desallocation d'une liste chainee de structure GRID
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
! Procedure : ajout avec allocation d'une structure champ generique
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
! Procedure : ajout avec allocation d'une structure champ (par insertion)
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
! mars 2004 : creation du module
! juin 2004 : procedure newbocofield
! oct  2004 : field chained list
!------------------------------------------------------------------------------!
