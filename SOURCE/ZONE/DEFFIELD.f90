!------------------------------------------------------------------------------!
! MODULE : DEFFIELD                       Auteur : J. Gressier
!                                         Date   : Octobre 2002
! Fonction                                Modif  : Juin 2003 (cf historique)
!   Bibliotheque de procedures et fonctions pour la gestion des champs
!   des différents solveurs
!
! Defauts/Limitations/Divers :
! Historique :
!
!------------------------------------------------------------------------------!

module DEFFIELD

use TYPHMAKE     ! Definition de la precision
use OUTPUT
use GEO3D        ! module de définition des vecteurs et opérateurs associés
use TENSOR3      ! module de définition des tenseurs et opérateurs associés

implicit none

! -- Variables globales du module -------------------------------------------

integer, parameter :: nghostcell = 1

! -- DECLARATIONS -----------------------------------------------------------

!------------------------------------------------------------------------------!
! Définition de la structure ST_SCAFIELD : Champ physique de scalaire
!------------------------------------------------------------------------------!

type st_scafield
  integer :: dim                            ! nombre de cellules
  real(krp), dimension(:), pointer :: scal  ! champ du scalaire
endtype

!------------------------------------------------------------------------------!
! Définition de la structure ST_VECFIELD : Champ physique de vecteurs
!------------------------------------------------------------------------------!

type st_vecfield
  integer :: dim                            ! nombre de cellules
  type(v3d), dimension(:), pointer :: vect  ! champ du vecteur
endtype

!------------------------------------------------------------------------------!
! Définition de la structure ST_TENFIELD : Champ physique de tenseurs
!------------------------------------------------------------------------------!

type st_tenfield
  integer :: dim                            ! nombre de cellules
  type(t3d), dimension(:), pointer :: tens  ! champ du vecteur
endtype

!------------------------------------------------------------------------------!
! Définition de la structure ST_GENERICFIELD : Champ physique générique
!------------------------------------------------------------------------------!

type st_genericfield
  integer      :: nscal, nvect, ntens        ! dimension de champs
  integer      :: dim                        ! nombre de valeurs par champ
  type(st_scafield), dimension(:), pointer :: tabscal   ! champs des scalaires
  type(st_vecfield), dimension(:), pointer :: tabvect   ! champs des vecteurs
  type(st_tenfield), dimension(:), pointer :: tabtens   ! champs des tenseurs
endtype st_genericfield

!------------------------------------------------------------------------------!
! Définition de la structure ST_FIELD : Champ physique et champs dérivés
!------------------------------------------------------------------------------!

type st_field
  integer                :: nscal, nvect  ! dimension de base des champs
  integer                :: ncell, nface  ! nombre de cellules et faces
  logical                :: allocgrad     ! allocation  des gradients ou non
  logical                :: allocres      ! allocation  des résidus
  logical                :: allocprim     ! allocation  des valeurs primitives
  logical                :: calcgrad      ! utilisation des gradients ou non
  type(st_genericfield)  :: etatcons      ! champ des valeurs physiques, conservatives
  type(st_genericfield)  :: etatprim      ! champ des valeurs physiques, primitives
  type(st_genericfield)  :: gradient      ! champ des gradients
  type(st_genericfield)  :: residu        ! champ des residus (valeurs conservatives)
endtype st_field



! -- INTERFACES -------------------------------------------------------------

interface new
  module procedure new_field, new_genericfield, new_genericfield_st, &
                   new_scafield, new_vecfield, new_tenfield
endinterface

interface delete
  module procedure delete_field, delete_genericfield, &
                   delete_scafield, delete_vecfield, delete_tenfield
endinterface


! -- Fonctions et Operateurs ------------------------------------------------



! -- IMPLEMENTATION ---------------------------------------------------------
contains


!------------------------------------------------------------------------------!
! Procédure : allocation d'une structure SCAFIELD
!------------------------------------------------------------------------------!
subroutine new_scafield(scafield, dim)
implicit none
type(st_scafield) :: scafield          ! champ à créer
integer           :: dim               ! dimension
!print*, "DEBUG NEW SCAFIELD"

  !if (associated(scafield%scal)) then
  !  print*, "scal ASSOCIE"
  !else
  !  print*, "scal NON ASSOCIE"
  !endif

  scafield%dim = dim
  if (scafield%dim > 0) then
  allocate(scafield%scal(scafield%dim))
  endif

endsubroutine new_scafield


!------------------------------------------------------------------------------!
! Procédure : deallocation d'une structure SCAFIELD
!------------------------------------------------------------------------------!
subroutine delete_scafield(scafield)
implicit none
type(st_scafield) :: scafield     

!print*, "DEBUG DELETE SCAFIELD2"
  deallocate(scafield%scal)
!print*, "scal désalloué"

endsubroutine delete_scafield


!------------------------------------------------------------------------------!
! Procédure : allocation d'une structure VECFIELD
!------------------------------------------------------------------------------!
subroutine new_vecfield(vecfield, dim)
implicit none
type(st_vecfield) :: vecfield          ! champ à créer
integer           :: dim               ! dimension

  vecfield%dim = dim
  if (vecfield%dim > 0) then
    allocate(vecfield%vect(dim))
  endif

endsubroutine new_vecfield


!------------------------------------------------------------------------------!
! Procédure : deallocation d'une structure VECFIELD
!------------------------------------------------------------------------------!
subroutine delete_vecfield(vecfield)
implicit none
type(st_vecfield) :: vecfield        

  deallocate(vecfield%vect)

endsubroutine delete_vecfield


!------------------------------------------------------------------------------!
! Procédure : allocation d'une structure TENFIELD
!------------------------------------------------------------------------------!
subroutine new_tenfield(tenfield, dim)
implicit none
type(st_tenfield) :: tenfield          ! champ à créer
integer           :: dim               ! dimension

  tenfield%dim = dim
  if (tenfield%dim > 0) then
    allocate(tenfield%tens(dim))
  endif

endsubroutine new_tenfield


!------------------------------------------------------------------------------!
! Procédure : deallocation d'une structure TENFIELD
!------------------------------------------------------------------------------!
subroutine delete_tenfield(tenfield)
implicit none
type(st_tenfield) :: tenfield        

  deallocate(tenfield%tens)

endsubroutine delete_tenfield


!------------------------------------------------------------------------------!
! Procédure : allocation d'une structure GENERICFIELD
!------------------------------------------------------------------------------!
subroutine new_genericfield(gfield, dim, n_scal, n_vect, n_tens)
implicit none 
type(st_genericfield) :: gfield                  ! champ à créer
integer               :: dim                     ! nombre de cellules des champs
integer               :: n_scal, n_vect, n_tens  ! nombre de scalaires, vecteurs et tenseurs
integer               :: i

!print*, "DEBUG NEW GENERIC FIELD"
  gfield%dim       = dim
  gfield%nscal     = n_scal
  gfield%nvect     = n_vect
  gfield%ntens     = n_tens
  
  if (gfield%nscal > 0) then
    allocate(gfield%tabscal(n_scal))          ! allocation du tableau de champs scalaires
    do i = 1, n_scal
      
      call new(gfield%tabscal(i), gfield%dim)  ! allocation champ par champ
    enddo
  endif
  
  if (gfield%nvect > 0) then
    allocate(gfield%tabvect(n_vect))          ! allocation du tableau de champs vecteurs
    do i = 1, n_vect
      call new(gfield%tabvect(i), gfield%dim)  ! allocation champ par champ
    enddo
  endif

  if (gfield%ntens > 0) then
    allocate(gfield%tabtens(n_tens))          ! allocation du tableau de champs tenseurs
    do i = 1, n_tens
      call new(gfield%tabtens(i), gfield%dim)  ! allocation champ par champ
    enddo
  endif

endsubroutine new_genericfield


!------------------------------------------------------------------------------!
! Procédure : allocation d'une structure FIELD à partir d'une autre structure
!------------------------------------------------------------------------------!
subroutine new_genericfield_st(newfield, oldfield)
implicit none
type(st_genericfield) :: newfield, oldfield     ! champ à créer, et champ d'origine

  call new(newfield, oldfield%dim, oldfield%nscal, oldfield%nvect, oldfield%ntens)

endsubroutine new_genericfield_st


!------------------------------------------------------------------------------!
! Procédure : initialisation d'une structure GENERICFIELD
!------------------------------------------------------------------------------!
subroutine init_genericfield(gfield, scal, vect)
implicit none
type(st_genericfield) :: gfield     ! champ à créer, et champ d'origine
real(krp)             :: scal       ! scalaire pour initialisation
type(v3d)             :: vect       ! vecteur  pour initialisation
integer               :: i

  do i = 1, gfield%nscal
    gfield%tabscal(i)%scal(:) = scal
  enddo

  do i = 1, gfield%nvect
    gfield%tabvect(i)%vect(:) = vect
  enddo

  do i = 1, gfield%ntens
    gfield%tabtens(i)%tens(:) = t3d(0._krp)
  enddo

endsubroutine init_genericfield


!------------------------------------------------------------------------------!
! Procédure : desallocation d'une structure GENERICFIELD
!------------------------------------------------------------------------------!
subroutine delete_genericfield(gfield)
implicit none
type(st_genericfield) :: gfield
integer               :: i
!print*, "DEBUG DELETE GFIELD", gfield%nscal, gfield%nvect, gfield%ntens 
  if (gfield%nscal > 0) then
    do i = 1, gfield%nscal
      !!print*, "delete scalaire ",i
      call delete(gfield%tabscal(i))
    enddo
    deallocate(gfield%tabscal)
  endif
  !! print*, "fin delete scalaire "

  if (gfield%nvect > 0) then
    do i = 1, gfield%nvect
      call delete(gfield%tabvect(i))
    enddo
    deallocate(gfield%tabvect)
  endif

  if (gfield%ntens > 0) then
    do i = 1, gfield%ntens
      call delete(gfield%tabtens(i))
    enddo
    deallocate(gfield%tabtens)
  endif

endsubroutine delete_genericfield


!------------------------------------------------------------------------------!
! Procédure : allocation des gradients
!------------------------------------------------------------------------------!
subroutine alloc_grad(field)
implicit none
type(st_field) :: field
integer        :: i

  if (field%allocgrad) then
    call print_info(90,"!!! Tableau de gradients déjà alloué !!!")
  else
    field%allocgrad = .true.
    call new(field%gradient, field%etatcons%dim, 0, field%etatcons%nscal, field%etatcons%nvect)
  endif

endsubroutine alloc_grad


!------------------------------------------------------------------------------!
! Procédure : deallocation des gradients
!------------------------------------------------------------------------------!
subroutine dealloc_grad(field)
implicit none
type(st_field) :: field
integer        :: i

  if (field%allocgrad) then
    call delete(field%gradient)
    field%allocgrad = .false.
  else
    call print_info(90,"!!! désallocation impossible : Tableau de gradients non alloué !!!")
  endif

endsubroutine dealloc_grad


!------------------------------------------------------------------------------!
! Procédure : allocation des résidus
!------------------------------------------------------------------------------!
subroutine alloc_res(field)
implicit none
type(st_field) :: field
integer        :: i
!print*, "DEBUG ALLOC_RES"
  if (field%allocres) then
    call print_info(90,"!!! Tableau de résidus déjà alloué !!!")
  else
    field%allocres = .true.
    call new(field%residu, field%etatcons%dim,   field%etatcons%nscal, &
                           field%etatcons%nvect, field%etatcons%ntens)
  endif

endsubroutine alloc_res


!------------------------------------------------------------------------------!
! Procédure : deallocation des résidus
!------------------------------------------------------------------------------!
subroutine dealloc_res(field)
implicit none
type(st_field) :: field
integer        :: i
!print*, "DEBUG DEALLOC_RES"
  if (field%allocres) then
    call delete(field%residu)
    field%allocres = .false.
  else
    call print_info(90,"!!! désallocation impossible : &
                       &Tableau de résidus non alloué !!!")
  endif

endsubroutine dealloc_res


!------------------------------------------------------------------------------!
! Procédure : allocation des variables primitives
!------------------------------------------------------------------------------!
subroutine alloc_prim(field)
implicit none
type(st_field) :: field
integer       :: i

  if (field%allocprim) then
    call print_info(90,"!!! Tableau de variables primitives déjà alloué !!!")
  else
    field%allocprim = .true.
    call new(field%etatprim, field%etatcons%dim,   field%etatcons%nscal, &
                             field%etatcons%nvect, field%etatcons%ntens)
  endif

endsubroutine alloc_prim


!------------------------------------------------------------------------------!
! Procédure : deallocation des variables primitives
!------------------------------------------------------------------------------!
subroutine dealloc_prim(field)
implicit none
type(st_field) :: field
integer       :: i

  if (field%allocprim) then
    field%allocprim = .false.
    call delete(field%etatprim)
  else
    call print_info(90,"!!! désallocation impossible : &
                       &Tableau des variables primitives non alloué !!!")
  endif

endsubroutine dealloc_prim


!------------------------------------------------------------------------------!
! Procédure : allocation d'une structure FIELD
!------------------------------------------------------------------------------!
subroutine new_field(field, n_scal, n_vect, ncell, nface)
implicit none 
type(st_field) :: field             ! champ à créer
integer        :: ncell, nface      ! nombre de cellules et faces
integer        :: n_scal, n_vect    ! nombre de scalaires, vecteurs et tenseurs
integer        :: i

  field%ncell     = ncell
  field%nface     = nface
  field%nscal     = n_scal
  field%nvect     = n_vect

  call new(field%etatcons, ncell, n_scal, n_vect, 0)
  field%allocgrad = .false.
  field%allocres  = .false.
  field%allocprim = .false.

endsubroutine new_field


!------------------------------------------------------------------------------!
! Procédure : desallocation d'une structure FIELD
!------------------------------------------------------------------------------!
subroutine delete_field(field)
implicit none 
type(st_field) :: field             ! champ à créer

  !print*,"desallocation FIELD :", field%allocgrad, field%allocres, field%allocprim !! DEBUG
  call delete(field%etatcons)
 
  !print*,"  desallocation gradient ?" !! DEBUG
  if (field%allocgrad) call dealloc_grad(field)
  !print*,"  desallocation residu ?" !! DEBUG
  if (field%allocres)  call dealloc_res (field)
  !print*,"  desallocation primitive ?" !! DEBUG
  if (field%allocprim) call dealloc_prim(field)
  !print*,"fin desallocation field ?" !! DEBUG

endsubroutine delete_field



endmodule DEFFIELD


!------------------------------------------------------------------------------!
! Historique des modifications
!
! oct  2002 (v0.0.1b): création du module
! juin 2003          : structuration des champs par type (scalaire, vecteur...)
!------------------------------------------------------------------------------!

