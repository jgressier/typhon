!------------------------------------------------------------------------------!
! MODULE : SPARSE_MAT                     Auteur : J. Gressier
!                                         Date   : Avril 2004
! Fonction                                Modif  : (cf historique)
!   Définition des structures de stockage de matrices creuses
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
! structure DLU : matrice à REMPLISSAGE symétrique
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


! -- INTERFACES -------------------------------------------------------------
interface new
  module procedure new_dlu
endinterface

interface delete
  module procedure delete_dlu
endinterface

! -- Fonctions et Operateurs ------------------------------------------------


! -- IMPLEMENTATION ---------------------------------------------------------
contains

!------------------------------------------------------------------------------!
! new_dlu : allocate DLU structure
!------------------------------------------------------------------------------!
subroutine new_dlu(mat, dim, ncouple)
implicit none
! - paramètres
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
! delete_dlu : remove DLU structure
!------------------------------------------------------------------------------!
subroutine delete_dlu(mat)
implicit none
! - paramètres
type(st_dlu) :: mat

  deallocate(mat%diag)
  deallocate(mat%lower)
  deallocate(mat%upper)
  call delete(mat%couple)

endsubroutine delete_dlu


!------------------------------------------------------------------------------!
! sort_dlu : sort elements in DLU structure so that index1 < index2
!------------------------------------------------------------------------------!
subroutine sort_dlu(mat)
implicit none
! - paramètres
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
! avr  2004 : création du module
!------------------------------------------------------------------------------!
