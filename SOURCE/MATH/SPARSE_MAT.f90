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
! new_dlu : allocation d'une structure LDU
!------------------------------------------------------------------------------!
subroutine new_dlu(mat, dim, ncouple)
implicit none
! - paramètres
type(st_dlu) :: mat
integer(kip) :: dim, ncouple

  mat%dim     = dim
  mat%ncouple = ncouple
  allocate(mat%diag(dim))      ;   mat%diag(:)  = 0._krp
  allocate(mat%lower(ncouple)) ;   mat%lower(:) = 0._krp
  allocate(mat%upper(ncouple)) ;   mat%upper(:) = 0._krp
  call new(mat%couple, ncouple, 2)

endsubroutine new_dlu

!------------------------------------------------------------------------------!
! structure DLU : matrice à REMPLISSAGE symétrique
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




endmodule SPARSE_MAT


!------------------------------------------------------------------------------!
! Historique des modifications
!
! avr  2004 : création du module
!------------------------------------------------------------------------------!
