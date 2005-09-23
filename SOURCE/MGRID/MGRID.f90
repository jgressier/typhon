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
! Definition ST_INFOGRID : 
!------------------------------------------------------------------------------!
type st_infogrid
  integer                 :: id                ! grid index
  integer                 :: mpi_cpu           ! CPU/process  index
  type(st_field), pointer :: field_loc         ! pointer to instantaneous field
  type(st_field), pointer :: field_cyclestart  ! pointer to starting cycle field
endtype st_infogrid

!------------------------------------------------------------------------------!
! Definition ST_INFOGRID : 
!------------------------------------------------------------------------------!
type st_grd_optmem
  logical            :: gradcond_computed
  type(t3d), pointer :: gradcond(:)
endtype st_grd_optmem

!------------------------------------------------------------------------------!
! Definition de la structure ST_GRID : grid maillage general et champ
!------------------------------------------------------------------------------!
type st_grid
  type(st_infogrid)       :: info       ! grid information
  type(st_grid), pointer  :: next       ! pointeur de liste chainee
  type(st_grid), pointer  :: subgrid    ! pointeur de liste chainee
  type(st_ustmesh)        :: umesh      ! maillage non structure (geometry + connectivity)

  integer                 :: nfield     ! nombre de champs
  type(st_field), pointer :: field      ! chained list of field (cons, prim, grad, residuals)

  integer                 :: nbocofield ! nombre de champs generiques
  type(st_genericfield), pointer &
                          :: bocofield  ! chained list of generic fields (sca, vec, tens)
                                        !   for boundary conditions
  type(st_grd_optmem)     :: optmem     ! 
  real(krp), pointer      :: dtloc(:)   ! array of local timestep
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

  grid%info%id = id

  grid%optmem%gradcond_computed = .false.
  nullify(grid%optmem%gradcond)

  nullify(grid%next)
  nullify(grid%subgrid)
  nullify(grid%dtloc)

endsubroutine new_grid


!------------------------------------------------------------------------------!
! Procedure : allocation of matrix for gradient computation
!------------------------------------------------------------------------------!
subroutine grid_alloc_gradcond(grid)
implicit none
type(st_grid)  :: grid
integer        :: id

  if (.not.associated(grid%optmem%gradcond)) then
    allocate(grid%optmem%gradcond(grid%umesh%ncell_int))
  endif

endsubroutine grid_alloc_gradcond


!------------------------------------------------------------------------------!
! Procedure : deallocation of matrix for gradient computation
!------------------------------------------------------------------------------!
subroutine grid_dealloc_gradcond(grid)
implicit none
type(st_grid)  :: grid
integer        :: id

  if (associated(grid%optmem%gradcond)) then
    deallocate(grid%optmem%gradcond)
    grid%optmem%gradcond_computed = .false.
  endif

endsubroutine grid_dealloc_gradcond


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
  call grid_dealloc_gradcond(grid) 
  call delete(grid%umesh)
  call delete_chainedfield(grid%field)
  if (associated(grid%dtloc)) deallocate(grid%dtloc)

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
   call new(pfield,grid%nfield,nscal,nvect,ncell,nface)
   nullify(pfield%next)
  else
    pfield => insert_newfield(grid%field,grid%nfield,nscal,nvect,ncell,nface)
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
