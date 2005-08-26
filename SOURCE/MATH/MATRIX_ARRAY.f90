!------------------------------------------------------------------------------!
! MODULE : MATRIX_ARRAY                        Authors : J. Gressier
!                                              Date    : July 2005
! Fonction
!   Bibliotheque de procedures et fonctions pour le calcul geometrique 3D
!
!------------------------------------------------------------------------------!

module MATRIX_ARRAY

use TYPHMAKE

! -- DECLARATIONS -----------------------------------------------------------

type st_mattab
  integer            :: dim, nb        ! dim of each matrix, and number of matrices
  real(krp), pointer :: mat(:,:,:)    
endtype

! -- INTERFACES -------------------------------------------------------------

interface new
  module procedure new_mattab
end interface

interface delete
  module procedure delete_mattab
end interface

! -- Fonctions et Operateurs ------------------------------------------------


! -- IMPLEMENTATION ---------------------------------------------------------
contains

!------------------------------------------------------------------------------!
! Constructor : new mattab
!------------------------------------------------------------------------------!
subroutine new_mattab(mattab, n, dim)
implicit none
type(st_mattab) :: mattab
integer(kip)    :: n, dim

  mattab%dim = dim
  mattab%nb  = n
  allocate(mattab%mat(dim, dim, n))

endsubroutine new_mattab

!------------------------------------------------------------------------------!
! Destructor : delete mattab
!------------------------------------------------------------------------------!
subroutine delete_mattab(mattab)
implicit none
type(st_mattab) :: mattab

  mattab%dim = 0
  mattab%nb  = 0
  deallocate(mattab%mat)
  
endsubroutine delete_mattab


endmodule MATRIX_ARRAY
!------------------------------------------------------------------------------!
! Changes history
!
! july 2005 : created, structure definition
!------------------------------------------------------------------------------!

