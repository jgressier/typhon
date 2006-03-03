!------------------------------------------------------------------------------!
! MODULE : MGRID                                  Authors : J. Gressier
!                                                 Created : Mars 2004
! Fonction
!   Definition des structures de donnees des grilles
!   maillage et champ
!
!------------------------------------------------------------------------------!

module MGRID

use TYPHMAKE      ! Definition de la precision/donnees informatiques
use USTMESH       ! Definition des maillages non structures
use DEFFIELD      ! Definition des champs physiques
use GEO3D        ! module de definition des vecteurs et operateurs associes
use GRID_CONNECT



implicit none

! -- Variables globales du module -------------------------------------------



! -- DECLARATIONS -----------------------------------------------------------

!------------------------------------------------------------------------------!
! Definition ST_INFOGRID : 
!------------------------------------------------------------------------------!
type st_infogrid
  integer                 :: id                ! grid index
  integer                 :: mpi_cpu           ! CPU/process  index
  integer                 :: l                 ! Refinement level
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
! Definition ST_CELLEXT : liste chainee de connectivites vers cellules externes
!------------------------------------------------------------------------------!
type st_cellext
   
  integer                 :: id  ! corresponding grid for each neighbour cell
  integer                 :: icell    ! cell id in the curent grid
  integer                 :: tarcell  ! cell id in the target grid
  integer                 :: mpi_cpu  ! proc of cell id
  type(st_grid), pointer  :: targrid
  type(st_cellext), pointer :: next
endtype st_cellext


!------------------------------------------------------------------------------!
! Definition ST_RCVDATA : reception des données de l'extérieur
!------------------------------------------------------------------------------!
type st_rcvdata
  DOUBLE PRECISION , dimension(:), pointer :: ext_data
endtype st_rcvdata


!------------------------------------------------------------------------------!
! Definition ST_SNDDATA : envoi des données de l'extérieur
!------------------------------------------------------------------------------!
type st_snddata
  DOUBLE PRECISION , dimension(:), pointer :: ext_data
endtype st_snddata


!-------------------------------------------------------------------------
! Definition of st_subcell
!-------------------------------------------------------------------------
!

type st_subcell
   integer                                 :: degree  ! nb of subcell per cell
   integer                                 :: ncell_int ! nb of cells
   integer, dimension(:), pointer          :: id
   integer, dimension(:,:), pointer        :: intcell
end type st_subcell


!------------------------------------------------------------------------------!
! Definition de la structure ST_GRID : grid maillage general et champ
!------------------------------------------------------------------------------!
type st_grid
  type(st_infogrid)       :: info       ! grid information
  type(st_grid), pointer  :: gridmere   ! pointeur vers la grille mere
  type(st_grid), pointer  :: next       ! pointeur de liste chainee
  type(st_grid), pointer  :: first      ! pointeur vers 1° element liste next
  type(st_grid), pointer  :: subgrid    ! pointeur de liste chainee
  type(st_ustmesh)        :: umesh      ! maillage non structure (geometry + connectivity)

  logical, dimension(:), pointer &
                          :: need_rf
  integer, dimension(:), pointer &
                          :: tab_intcell! tableau des cellules de la grille mere 

  type(st_subcell), pointer :: subcell   ! Subcells

  type(st_cellext), pointer :: cellext   ! Connectivite de voisinage

  integer                 :: nfield     ! nombre de champs
  type(st_field), pointer :: field      ! chained list of field (cons, prim, grad, residuals)

  integer                 :: nbocofield ! nombre de champs generiques
  type(st_genericfield), pointer &
                          :: bocofield  ! chained list of generic fields (sca, vec, tens)
                                        !   for boundary conditions
  type(st_grd_optmem)     :: optmem     ! 
  real(krp), pointer      :: dtloc(:)   ! array of local timestep
  type(st_gridconnect), pointer :: gdcon_send
  type(st_gridconnect), pointer :: gdcon_recv
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
  nullify(grid%first)
  nullify(grid%subgrid)

  nullify(grid%field)

  nullify(grid%gridmere)
  nullify(grid%cellext)
  nullify(grid%subcell)

  nullify(grid%dtloc)

endsubroutine new_grid

!------------------------------------------------------------------------------!
! Procedure : initialisation d'une structure GRID fille
!------------------------------------------------------------------------------!
subroutine new_subgrid(grid, id, gridmere)
implicit none
type(st_grid), target  :: grid
type(st_grid), pointer ::  gridmere
type(st_grid), pointer ::  pg
integer        :: id


  allocate(pg)
  grid%info%id = id
  grid%info%l=gridmere%info%l+1
  grid%info%mpi_cpu=gridmere%info%mpi_cpu

  grid%optmem%gradcond_computed = .false.
  allocate(grid%first)
  allocate(grid%gridmere)

  nullify(grid%optmem%gradcond)
  nullify(grid%cellext)
  nullify(grid%next)
  nullify(grid%subgrid)
  nullify(grid%subcell)

  grid%gridmere => gridmere
  grid%next=>gridmere%subgrid
  gridmere%subgrid=>grid

  pg=>grid
  do while(associated(pg))
     pg%first=>grid
     pg=>pg%next
  end do

endsubroutine new_subgrid


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
type(st_grid), pointer :: pg
integer                :: id

  allocate(pgrid)
  allocate(pg)
  call new(pgrid, id)
  pgrid%next => grid  

  pg=>pgrid
  do while(associated(pg))
     pg%first=pgrid
     pg=>pg%next
  end do

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
  if (associated(grid%field)) call delete_chainedfield(grid%field)
  if (associated(grid%dtloc)) deallocate(grid%dtloc)

  ! destruction des sous-grilles
  if (associated(grid%subgrid)) call delete_chainedgrid(grid%subgrid)

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


!------------------------------------------------------------------------------!
! Procedure : insertion d'un type cellext dans une liste chainee
!------------------------------------------------------------------------------
subroutine insert_cellext(grid, icell, tarcell, cellext_list)
implicit none

! input
type(st_grid), target        :: grid   ! numero de la grille cible
integer                      :: icell, tarcell
! output
type(st_cellext), pointer    :: cellext_list ! pointeur de la liste chainee

! intern
type(st_cellext), pointer    :: cellext ! pointeur de la liste chainee


allocate(cellext)
allocate(cellext%targrid)
allocate(cellext%next)


! creation
cellext%icell=icell
cellext%tarcell=tarcell
cellext%id=grid%info%id
cellext%mpi_cpu=grid%info%mpi_cpu
cellext%targrid=>grid

! add to the list
if(.NOT. associated(cellext_list)) then
   allocate(cellext_list)
   cellext_list=>cellext
   nullify(cellext_list%next)
else
   cellext%next=>cellext_list
   cellext_list=>cellext
end if

endsubroutine insert_cellext

!------------------------------------------------------------------------------!
! Procedure : recherche de l'element de numero icell dans cellext_list
!             et le retourne dans cellext
!------------------------------------------------------------------------------
subroutine search_cellext(icell, cellext_list, cellext, found)
implicit none

! input
integer                      :: icell
logical                      :: found
type(st_cellext), pointer    :: cellext_list ! pointeur de la liste chainee

! output
type(st_cellext)    :: cellext ! element a trouver

! intern
type(st_cellext), pointer    :: pcellext ! element en cours

allocate(pcellext)
found=.false.	

if(associated(cellext_list)) then
   pcellext=>cellext_list
   found=.true.
   do while(pcellext%icell /= icell .AND. found)
      if(associated(pcellext%next)) then
         pcellext=>pcellext%next
      else
         found=.false.
      end if
   end do
else
   found=.false.
end if

cellext=pcellext

endsubroutine search_cellext


!-------------------------------------------------------------------------
!     new_subcell : creation of a new subcell struct
!-------------------------------------------------------------------------

subroutine new_subcell(subcell, degree, ncell_int)
  
  ! input
  integer          :: degree
  integer          :: ncell_int
  
  ! output
  type(st_subcell) :: subcell
  
  ! Procedure
  
  ! allocation
  allocate(subcell%id(ncell_int))
  !allocate(subcell%subgrid(ncell_int))
  allocate(subcell%intcell(ncell_int,degree))
  
  ! affectation
  subcell%degree=degree
  subcell%ncell_int=ncell_int
  subcell%id=0
  subcell%intcell=0
  
end subroutine new_subcell



endmodule MGRID

!------------------------------------------------------------------------------!
! Changes history
!
! mars 2004 : creation du module
! juin 2004 : procedure newbocofield
! oct  2004 : field chained list
! mai  2005 : definition et procedure cellext
! Oct  2005 : send & receive connection structures in MGRID structure
!------------------------------------------------------------------------------!
