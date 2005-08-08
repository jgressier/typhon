!------------------------------------------------------------------------------!
! MODULE : SPMAT_SDLU                                Authors : J. Gressier
!                                                   Created : Avril 2004
! Fonction                                
!   Definition des structures de stockage de matrices creuses
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

module SPMAT_SDLU

use TYPHMAKE       ! Definition de la precision
use CONNECTIVITY   

implicit none

! -- Variables globales du module -------------------------------------------



! -- DECLARATIONS -----------------------------------------------------------

!------------------------------------------------------------------------------!
! structure SDLU : symmetrical matrix
!------------------------------------------------------------------------------!
type st_sdlu
  logical            :: sort
  logical            :: diaginvert
  integer(kip)       :: dim         ! maximal used index (for information only)
  integer(kip)       :: ncouple     ! size of value & couple
  real(krp), pointer :: value(:)   
  type(st_connect)   :: couple      ! list of couples
endtype st_sdlu


! -- INTERFACES -------------------------------------------------------------
interface new
  module procedure new_sdlu
endinterface

interface delete
  module procedure delete_sdlu
endinterface

interface realloc
  module procedure realloc_sdlu
endinterface

! -- Fonctions et Operateurs ------------------------------------------------


! -- IMPLEMENTATION ---------------------------------------------------------
contains

!------------------------------------------------------------------------------!
! new_sdlu : allocate SDLU structure
!------------------------------------------------------------------------------!
subroutine new_sdlu(mat, dim, ncouple)
implicit none
! - parametres
type(st_sdlu) :: mat
integer(kip) :: dim, ncouple

  mat%sort       = .false.
  mat%diaginvert = .false.
  mat%dim      = dim
  mat%ncouple = ncouple
  allocate(mat%value(ncouple)) ;   mat%value(:) = 0._krp
  call new(mat%couple, ncouple, 2)

endsubroutine new_sdlu

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
! sort_sdlu : sort elements in SDLU structure so that index1 < index2
!------------------------------------------------------------------------------!
subroutine sort_sdlu(mat)
implicit none
! - parametres
type(st_sdlu) :: mat
! - internal
real(krp)    :: x
integer      :: i, if

  call erreur("Internal error", "not supposed to be used")

endsubroutine sort_sdlu


!------------------------------------------------------------------------------!
! invertdiag_sdlu : invert diagonal block elements
!------------------------------------------------------------------------------!
subroutine invertdiag_sdlu(mat)
implicit none
! - parametres
type(st_sdlu) :: mat
! - internal
real(krp)    :: x
integer      :: i, if

endsubroutine invertdiag_sdlu


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


endmodule SPMAT_SDLU
!------------------------------------------------------------------------------!
! Historique des modifications
!
! avr  2004 : created, scalar terms
! dec  2004 : extension to block terms
!------------------------------------------------------------------------------!
