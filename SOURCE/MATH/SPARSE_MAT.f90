!------------------------------------------------------------------------------!
! MODULE : SPARSE_MAT                     Auteur : J. Gressier
!                                         Date   : Avril 2004
! Fonction                                Modif  : (cf historique)
!   Definition des structures de stockage de matrices creuses
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

module SPARSE_MAT

use TYPHMAKE       ! Definition de la precision
use CONNECTIVITY   

implicit none

! -- Variables globales du module -------------------------------------------



! -- DECLARATIONS -----------------------------------------------------------

!------------------------------------------------------------------------------!
! structure DLU : symmetrically filled matrix
!------------------------------------------------------------------------------!
type st_dlu
  logical                          :: sort
  integer(kip)                     :: dim
  integer(kip)                     :: ncouple
  real(krp), dimension(:), pointer :: diag     ! coefficient de la diagonale
  real(krp), dimension(:), pointer :: lower    ! coefficient triang. inf
  real(krp), dimension(:), pointer :: upper    ! coefficient triang. inf
  type(st_connect)                 :: couple   ! liste (dim) des couples
endtype st_dlu


!------------------------------------------------------------------------------!
! structure SDLU : symmetric matrix
!------------------------------------------------------------------------------!
type st_sdlu
  logical                          :: sort
  integer(kip)                     :: dim       ! maximal used index
  integer(kip)                     :: ncouple
  real(krp), dimension(:), pointer :: value     ! coefficient de la diagonale
  type(st_connect)                 :: couple    ! liste (dim) des couples
endtype st_sdlu


! -- INTERFACES -------------------------------------------------------------
interface new
  module procedure new_dlu, new_sdlu
endinterface

interface delete
  module procedure delete_dlu, delete_sdlu
endinterface

interface realloc
  module procedure realloc_sdlu
endinterface

! -- Fonctions et Operateurs ------------------------------------------------


! -- IMPLEMENTATION ---------------------------------------------------------
contains

!------------------------------------------------------------------------------!
! new_dlu : allocate DLU structure
!------------------------------------------------------------------------------!
subroutine new_dlu(mat, dim, ncouple)
implicit none
! - parametres
type(st_dlu) :: mat
integer(kip) :: dim, ncouple

  mat%sort    = .false.
  mat%dim     = dim
  mat%ncouple = ncouple
  allocate(mat%diag(dim))      ;   mat%diag(:)  = 0._krp
  allocate(mat%lower(ncouple)) ;   mat%lower(:) = 0._krp
  allocate(mat%upper(ncouple)) ;   mat%upper(:) = 0._krp
  call new(mat%couple, ncouple, 2)

endsubroutine new_dlu

!------------------------------------------------------------------------------!
! new_dlu : allocate SDLU structure
!------------------------------------------------------------------------------!
subroutine new_sdlu(mat, dim, ncouple)
implicit none
! - parametres
type(st_sdlu) :: mat
integer(kip) :: dim, ncouple

  mat%sort    = .false.
  mat%dim     = dim
  mat%ncouple = ncouple
  allocate(mat%value(ncouple)) ;   mat%value(:) = 0._krp
  call new(mat%couple, ncouple, 2)

endsubroutine new_sdlu

!------------------------------------------------------------------------------!
! delete_dlu : remove DLU structure
!------------------------------------------------------------------------------!
subroutine delete_dlu(mat)
implicit none
! - parametres
type(st_dlu) :: mat

  if (associated(mat%diag)) then
    deallocate(mat%diag)
    deallocate(mat%lower)
    deallocate(mat%upper)
    call delete(mat%couple)
  endif

endsubroutine delete_dlu


!------------------------------------------------------------------------------!
! delete_sdlu : remove SDLU structure
!------------------------------------------------------------------------------!
subroutine delete_sdlu(mat)
implicit none
! - parametres
type(st_sdlu) :: mat

  if (associated(mat%value)) deallocate(mat%value)
  call delete(mat%couple)

endsubroutine delete_sdlu


!------------------------------------------------------------------------------!
! realloc_sdlu : reallocate SDLU structure
!------------------------------------------------------------------------------!
subroutine realloc_sdlu(mat, dim, ncouple)
implicit none
! - parametres
type(st_sdlu)      :: mat
integer(kip)       :: dim, ncouple
real(krp), pointer :: prov(:)

  call realloc(mat%couple, ncouple, 2)
  allocate(prov(ncouple))
  prov(:)             = 0._krp
  prov(1:mat%ncouple) = mat%value(1:mat%ncouple)
  deallocate(mat%value)
  mat%value => prov
  mat%sort    = .false.
  mat%dim     = dim
  mat%ncouple = ncouple

endsubroutine realloc_sdlu


!------------------------------------------------------------------------------!
! sort_dlu : sort elements in DLU structure so that index1 < index2
!------------------------------------------------------------------------------!
subroutine sort_dlu(mat)
implicit none
! - parametres
type(st_dlu) :: mat
! - internal
real(krp)    :: x
integer      :: i, if

  if (.not.mat%sort) then
    do if = 1, mat%ncouple
      if (mat%couple%fils(if,1) > mat%couple%fils(if,2)) then
        x             = mat%lower(if) 
        mat%lower(if) = mat%upper(if)
        mat%upper(if) = x
        i                     = mat%couple%fils(if,2)
        mat%couple%fils(if,2) = mat%couple%fils(if,1)
        mat%couple%fils(if,1) = i
      endif
    enddo
    mat%sort = .true.
  endif

endsubroutine sort_dlu



endmodule SPARSE_MAT


!------------------------------------------------------------------------------!
! Historique des modifications
!
! avr  2004 : creation du module
!------------------------------------------------------------------------------!
