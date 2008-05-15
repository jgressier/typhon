!------------------------------------------------------------------------------!
! MODULE : DEFFIELD 
!
! Fonction   
!   Library for managing scalar, vector, tensor fields and the derivatives
!   of variables type (conservative, primitive, residuals, gradients)
!
!------------------------------------------------------------------------------!

module DEFFIELD

use TYPHMAKE     ! Definition de la precision
use OUTPUT
use GEO3D        ! 
use TENSOR3      ! 
use GENFIELD

implicit none

! -- Variables globales du module -------------------------------------------


! -- DECLARATIONS -----------------------------------------------------------


!------------------------------------------------------------------------------!
! Definition de la structure ST_FIELD : Champ physique et champs derives
!------------------------------------------------------------------------------!

type st_field
  integer                 :: id            ! numero de champ
  type(st_field), pointer :: next          ! pointeur pour liste chaînée
  integer                 :: nscal, nvect  ! dimension de base des champs
  integer                 :: ncell, nface  ! nombre de cellules et faces
  logical                 :: allocQref     ! allocation of Qref field
  logical                 :: allocgrad     ! allocation  des gradients ou non
  logical                 :: allocres      ! allocation  des residus
  logical                 :: allocprim     ! allocation  des valeurs primitives
  logical                 :: calcgrad      ! utilisation des gradients ou non
  type(st_genericfield)   :: Qref          ! reference field
  type(st_genericfield)   :: etatcons      ! champ des valeurs physiques, conservatives
  type(st_genericfield)   :: etatprim      ! champ des valeurs physiques, primitives
  type(st_genericfield)   :: gradient      ! champ des gradients
  type(st_genericfield)   :: residu        ! champ des residus (valeurs conservatives)
endtype st_field



! -- INTERFACES -------------------------------------------------------------

interface new
  module procedure new_field
endinterface

interface delete
  module procedure delete_field
endinterface


! -- Fonctions et Operateurs ------------------------------------------------



! -- IMPLEMENTATION ---------------------------------------------------------
contains

!------------------------------------------------------------------------------!
! Procedure : allocation des gradients
!------------------------------------------------------------------------------!
subroutine alloc_grad(field)
implicit none
type(st_field) :: field
integer        :: i

  if (field%allocgrad) then
    call print_info(90,"!!! Tableau de gradients deja alloue !!!")
  else
    field%allocgrad = .true.
    call new(field%gradient, field%etatcons%dim, 0, field%etatcons%nscal, field%etatcons%nvect)
  endif

endsubroutine alloc_grad


!------------------------------------------------------------------------------!
! Procedure : deallocation des gradients
!------------------------------------------------------------------------------!
subroutine dealloc_grad(field)
implicit none
type(st_field) :: field
integer        :: i

  if (field%allocgrad) then
    call delete(field%gradient)
    field%allocgrad = .false.
  else
    call print_info(90,"!!! desallocation impossible : Tableau de gradients non alloue !!!")
  endif

endsubroutine dealloc_grad


!------------------------------------------------------------------------------!
! Procedure : allocation des residus
!------------------------------------------------------------------------------!
subroutine alloc_res(field)
implicit none
type(st_field) :: field
integer        :: i
!print*, "DEBUG ALLOC_RES"
  if (field%allocres) then
    call print_info(90,"!!! Tableau de residus deja alloue !!!")
  else
    field%allocres = .true.
    call new(field%residu, field%etatcons%dim,   field%etatcons%nscal, &
                           field%etatcons%nvect, field%etatcons%ntens)
  endif

endsubroutine alloc_res


!------------------------------------------------------------------------------!
! Procedure : deallocation des residus
!------------------------------------------------------------------------------!
subroutine dealloc_res(field)
implicit none
type(st_field) :: field
integer        :: i

  if (field%allocres) then
    call delete(field%residu)
    field%allocres = .false.
  else
    call print_info(90,"!!! desallocation impossible : &
                       &Tableau de residus non alloue !!!")
  endif

endsubroutine dealloc_res


!------------------------------------------------------------------------------!
! Procedure : allocation des variables primitives
!------------------------------------------------------------------------------!
subroutine alloc_prim(field)
implicit none
type(st_field) :: field
integer       :: i

  if (field%allocprim) then
    call print_info(90,"!!! Tableau de variables primitives deja alloue !!!")
  else
    field%allocprim = .true.
    call new(field%etatprim, field%etatcons%dim,   field%etatcons%nscal, &
                             field%etatcons%nvect, field%etatcons%ntens)
  endif

endsubroutine alloc_prim


!------------------------------------------------------------------------------!
! Procedure : deallocation des variables primitives
!------------------------------------------------------------------------------!
subroutine dealloc_prim(field)
implicit none
type(st_field) :: field
integer       :: i

  if (field%allocprim) then
    field%allocprim = .false.
    call delete(field%etatprim)
  else
    call print_info(90,"!!! desallocation impossible : &
                       &Tableau des variables primitives non alloue !!!")
  endif

endsubroutine dealloc_prim


!------------------------------------------------------------------------------!
! Procedure : allocation des variables Qrefitives
!------------------------------------------------------------------------------!
subroutine alloc_Qref(field)
implicit none
type(st_field) :: field
integer       :: i

  if (field%allocQref) then
    call print_info(90,"!!! Tableau de variables reference deja alloue !!!")
  else
    field%allocQref = .true.
    call new(field%Qref, field%etatcons%dim,   field%etatcons%nscal, &
                         field%etatcons%nvect, field%etatcons%ntens)
  endif

endsubroutine alloc_Qref


!------------------------------------------------------------------------------!
! Procedure : deallocation des variables Qrefitives
!------------------------------------------------------------------------------!
subroutine dealloc_Qref(field)
implicit none
type(st_field) :: field
integer       :: i

  if (field%allocQref) then
    field%allocQref = .false.
    call delete(field%Qref)
  else
    call print_info(90,"!!! desallocation impossible : &
                       &Tableau des variables Qrefitives non alloue !!!")
  endif

endsubroutine dealloc_Qref


!------------------------------------------------------------------------------!
! Procedure : allocation d'une structure FIELD
!------------------------------------------------------------------------------!
subroutine new_field(field, id, n_scal, n_vect, ncell, nface)
implicit none 
type(st_field) :: field             ! champ a creer
integer        :: ncell, nface      ! nombre de cellules et faces
integer        :: n_scal, n_vect    ! nombre de scalaires, vecteurs et tenseurs
integer        :: id                ! numero de champ

  field%id        = id
  field%ncell     = ncell
  field%nface     = nface
  field%nscal     = n_scal
  field%nvect     = n_vect

  call new(field%etatcons, ncell, n_scal, n_vect, 0)
  field%allocgrad = .false.
  field%allocres  = .false.
  field%allocprim = .false.
  field%allocQref = .false.

endsubroutine new_field


!------------------------------------------------------------------------------!
! Procedure : desallocation d'une structure FIELD
!------------------------------------------------------------------------------!
subroutine delete_field(field)
implicit none 
type(st_field) :: field             ! champ a creer

  call delete(field%etatcons)
 
  if (field%allocgrad) call dealloc_grad(field)
  if (field%allocres)  call dealloc_res (field)
  if (field%allocprim) call dealloc_prim(field)
  if (field%allocqref) call dealloc_qref(field)

endsubroutine delete_field


!------------------------------------------------------------------------------!
! Procedure : creation et lien chaine d'une structure FIELD
!------------------------------------------------------------------------------!
function insert_newfield(field, id, n_scal, n_vect, ncell, nface) result(pfield)
implicit none
type(st_field), pointer :: pfield
type(st_field), target  :: field
integer                 :: n_scal,n_vect,ncell,nface
integer                 :: id

  allocate(pfield)
  call new(pfield,id,n_scal,n_vect,ncell,nface)
  pfield%next => field  

endfunction insert_newfield


!------------------------------------------------------------------------------!
! Procedure : desallocation d'une liste chainee de structure FIELD
!------------------------------------------------------------------------------!
subroutine delete_chainedfield(field)
implicit none
type(st_field), target  :: field
type(st_field), pointer :: pfield, dfield

  pfield => field
  do while(associated(pfield))
    dfield => pfield
    pfield => pfield%next
    call delete(dfield)
  enddo

endsubroutine delete_chainedfield

!------------------------------------------------------------------------------
! Procédure : transfert de champ : rfield reçoit ifield
!------------------------------------------------------------------------------
subroutine transfer_field(rfield, ifield)
implicit none
type(st_field) :: ifield, rfield

rfield%nscal = ifield%nscal
rfield%nvect = ifield%nvect
rfield%ncell = ifield%ncell
rfield%nface = ifield%nface
rfield%allocgrad = ifield%allocgrad
rfield%allocres  = ifield%allocres
rfield%allocprim = ifield%allocprim
rfield%allocqref = ifield%allocqref
rfield%calcgrad  = ifield%calcgrad
call transfer_gfield(rfield%etatcons,ifield%etatcons)
call transfer_gfield(rfield%etatprim,ifield%etatprim)
call transfer_gfield(rfield%qref,ifield%qref)
call transfer_gfield(rfield%gradient,ifield%gradient)
call transfer_gfield(rfield%residu,ifield%residu)
  
endsubroutine transfer_field


!------------------------------------------------------------------------------
! field_setref : copy Qcons to Qref
!------------------------------------------------------------------------------
subroutine field_cons2ref(field)
implicit none
type(st_field) :: field

if (.not.field%allocqref) call alloc_qref(field)

call transfer_gfield(field%qref, field%etatcons)
  
endsubroutine field_cons2ref

!------------------------------------------------------------------------------
! field_setref : copy Qcons to Qref
!------------------------------------------------------------------------------
subroutine field_ref2cons(field)
implicit none
type(st_field) :: field

call transfer_gfield(field%etatcons, field%qref)
  
endsubroutine field_ref2cons


endmodule DEFFIELD


!------------------------------------------------------------------------------!
! Changes history
!
! oct  2002 : creation du module
! juin 2003 : structuration des champs par type (scalaire, vecteur...)
! DEV: interface champ/tableau
! DEV: decoupage en MGFIELD et MZFIELD pour fonctions haut et bas niveau
! juin 2004 : procedures insert_newgfield et delete_chainedgfield
! oct  2004 : field chained list
! nov  2004 : split DEFFIELD -> DEFFIELD / GENFIELD
! Apr  2008: create Qref and routines to copy to/from Qcons
!------------------------------------------------------------------------------!

