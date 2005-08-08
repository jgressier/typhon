!------------------------------------------------------------------------------!
! MODULE : SPMAT_BDLU                                 Authors : J. Gressier
!                                                     Created : June 2005
! Fonction
!   Definition des structures de stockage de matrices creuses
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

module SPMAT_BDLU

use TYPHMAKE       ! Definition de la precision
use CONNECTIVITY   

implicit none

! -- Variables globales du module -------------------------------------------



! -- DECLARATIONS -----------------------------------------------------------

!------------------------------------------------------------------------------!
! structure BDLU : matrice a REMPLISSAGE symetrique
!------------------------------------------------------------------------------!
type st_bdlu
  logical                          :: sort
  logical                          :: diaginvert
  integer(kip)                     :: dim
  integer(kip)                     :: dimblock
  integer(kip)                     :: ncouple
  real(krp), dimension(:,:,:), pointer :: diag     ! coefficient de la diagonale
  real(krp), dimension(:,:,:), pointer :: lower    ! coefficient triang. inf
  real(krp), dimension(:,:,:), pointer :: upper    ! coefficient triang. inf
  type(st_connect)                     :: couple   ! liste (dim) des couples
endtype st_bdlu


! -- INTERFACES -------------------------------------------------------------
interface new
  module procedure new_bdlu
endinterface

interface delete
  module procedure delete_bdlu
endinterface

! -- Fonctions et Operateurs ------------------------------------------------


! -- IMPLEMENTATION ---------------------------------------------------------
contains

!------------------------------------------------------------------------------!
! new_bdlu : allocate BDLU structure
!------------------------------------------------------------------------------!
subroutine new_bdlu(mat, dimblock, dim, ncouple)
implicit none
! - parametres
type(st_bdlu) :: mat
integer(kip) :: dimblock, dim, ncouple

  mat%sort       = .false.
  mat%diaginvert = .false.
  mat%dimblock = dimblock
  mat%dim      = dim
  mat%ncouple = ncouple
  allocate(mat% diag(dim,     dimblock, dimblock)) ;   mat% diag(:,:,:) = 0._krp
  allocate(mat%lower(ncouple, dimblock, dimblock)) ;   mat%lower(:,:,:) = 0._krp
  allocate(mat%upper(ncouple, dimblock, dimblock)) ;   mat%upper(:,:,:) = 0._krp
  call new(mat%couple, ncouple, 2)

endsubroutine new_bdlu

!------------------------------------------------------------------------------!
! delete_bdlu : remove BDLU structure
!------------------------------------------------------------------------------!
subroutine delete_bdlu(mat)
implicit none
! - parametres
type(st_bdlu) :: mat

  deallocate(mat%diag)
  deallocate(mat%lower)
  deallocate(mat%upper)
  call delete(mat%couple)

endsubroutine delete_bdlu


!------------------------------------------------------------------------------!
! sort_bdlu : sort elements in BDLU structure so that index1 < index2
!------------------------------------------------------------------------------!
subroutine sort_bdlu(mat)
implicit none
! - parametres
type(st_bdlu) :: mat
! - internal
real(krp)    :: x(mat%dimblock, mat%dimblock)
integer      :: i, if

  if (.not.mat%sort) then
    do if = 1, mat%ncouple
      if (mat%couple%fils(if,1) > mat%couple%fils(if,2)) then
        x(:,:)            = mat%lower(if,:,:) 
        mat%lower(if,:,:) = mat%upper(if,:,:)
        mat%upper(if,:,:) = x(:,:)
        i                     = mat%couple%fils(if,2)
        mat%couple%fils(if,2) = mat%couple%fils(if,1)
        mat%couple%fils(if,1) = i
      endif
    enddo
    mat%sort = .true.
  endif

endsubroutine sort_bdlu


!------------------------------------------------------------------------------!
! invertdiag_bdlu : invert diagonal block elements
!------------------------------------------------------------------------------!
subroutine invertdiag_bdlu(mat)
implicit none
! - parametres
type(st_bdlu) :: mat
! - internal
real(krp)    :: x(mat%dimblock, mat%dimblock)
integer      :: i, if



endsubroutine invertdiag_bdlu



endmodule SPMAT_BDLU


!------------------------------------------------------------------------------!
! Historique des modifications
!
! avr  2004 : created, scalar terms
! dec  2004 : from SPMATH_DLU, extension to block terms
!------------------------------------------------------------------------------!
