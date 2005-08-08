!------------------------------------------------------------------------------!
! MODULE : SPMAT_CRS                     Auteur : J. Gressier
!                                         Date   : February 2005
! Fonction                                Modif  : (cf history)
!   Definition des structures de stockage de matrices creuses
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

module SPMAT_CRS

use TYPHMAKE       ! Definition de la precision

implicit none

! -- Variables globales du module -------------------------------------------



! -- DECLARATIONS -----------------------------------------------------------

!------------------------------------------------------------------------------!
! structure CRS : Compressed Row Storage
!------------------------------------------------------------------------------!
type st_crs
  logical               :: invertdiag ! original or inverted diagonal
  integer(kip)          :: nval       ! number of non zero values
  integer(kip)          :: dim        ! dimension of original matrix
  ! integer(kip)        :: dimblock
  real(krp),    pointer :: val(:)     ! value in matrix
  integer(kip), pointer :: jcol(:)    ! column index of k-th value (1:nval)
  integer(kip), pointer :: kval(:)    ! k index in val(:) of first value of row i-th (1:dim+1)
endtype st_crs

! -- INTERFACES -------------------------------------------------------------

interface new
  module procedure new_crs
endinterface

interface delete
  module procedure delete_crs
endinterface

! -- Fonctions et Operateurs ------------------------------------------------


! -- IMPLEMENTATION ---------------------------------------------------------
contains

!------------------------------------------------------------------------------!
! new_crs : allocate CRS structure
!------------------------------------------------------------------------------!
subroutine new_crs(mat, dim, nval)
implicit none
! - parametres
type(st_crs) :: mat
integer(kip) :: dim, nval

  mat%invertdiag = .false.
  mat%dim        = dim
  mat%nval       = nval
  allocate(mat%val (nval))  ; mat%val (:) = 0._krp
  allocate(mat%jcol(nval))  ; mat%jcol(:) = 0_kip
  allocate(mat%kval(dim+1)) ; mat%kval(:) = 0_kip

endsubroutine new_crs

!------------------------------------------------------------------------------!
! delete_crs : remove CRS structure
!------------------------------------------------------------------------------!
subroutine delete_crs(mat)
implicit none
! - parametres
type(st_crs) :: mat

  deallocate(mat%val)
  deallocate(mat%jcol)
  deallocate(mat%kval)

endsubroutine delete_crs


!------------------------------------------------------------------------------!
! sort_crs : sort elements in CRS structure so that index1 < index2
!------------------------------------------------------------------------------!
subroutine sort_crs(mat)
implicit none
! - parametres
type(st_crs) :: mat
! - internal
real(krp)    :: x
integer      :: i, if


endsubroutine sort_crs


!------------------------------------------------------------------------------!
! invertdiag_crs : invert diagonal block elements
!------------------------------------------------------------------------------!
subroutine invertdiag_crs(mat)
implicit none
! - parametres
type(st_crs) :: mat
! - internal
real(krp)    :: x
integer      :: i, if



endsubroutine invertdiag_crs



endmodule SPMAT_CRS

!------------------------------------------------------------------------------!
! Changes history
!
! fev  2005 : created
!------------------------------------------------------------------------------!
