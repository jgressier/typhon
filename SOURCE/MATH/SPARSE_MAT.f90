!------------------------------------------------------------------------------!
! MODULE : SPARSE_MAT                             Authors : J. Gressier
!
! Function
!   Definition of sparse matrix storage structures
!
! Defaults/Limitations/Misc :
!
!------------------------------------------------------------------------------!

module SPARSE_MAT

use TYPHMAKE       ! Definition de la precision
use OUTPUT
use SPMAT_DLU
use SPMAT_SDLU
use SPMAT_CRS
use SPMAT_BDLU

implicit none

! -- Global variables -------------------------------------------------------

integer(kpp), parameter :: mat_undef = -1
integer(kpp), parameter :: mat_none  =  1
integer(kpp), parameter :: mat_dlu   = 10
integer(kpp), parameter :: mat_bdlu  = 11
integer(kpp), parameter :: mat_sdlu  = 15
integer(kpp), parameter :: mat_bsdlu = 16
integer(kpp), parameter :: mat_crs   = 20
integer(kpp), parameter :: mat_bcrs  = 21


! -- DECLARATIONS -----------------------------------------------------------

!------------------------------------------------------------------------------!
! structure SPMAT : general sparse matrix
!------------------------------------------------------------------------------!
type st_spmat
  integer(kpp)   :: type
  type(st_dlu)   :: dlu
  type(st_sdlu)  :: sdlu
  type(st_crs)   :: crs
  type(st_bdlu)  :: bdlu
endtype st_spmat

! -- INTERFACES -------------------------------------------------------------

!interface new
!  module procedure new_
!endinterface

interface delete
  module procedure delete_spmat
endinterface

! -- Functions and Operators ------------------------------------------------


! -- IMPLEMENTATION ---------------------------------------------------------
contains

!------------------------------------------------------------------------------!
! delete_spmat : remove SPMAT structure
!------------------------------------------------------------------------------!
subroutine delete_spmat(mat)
implicit none
! - parametres
type(st_spmat) :: mat

  select case(mat%type)
  case(mat_dlu)
    call delete(mat%dlu)
  case(mat_bdlu)
    call delete(mat%bdlu)
  case(mat_sdlu)
    call delete(mat%sdlu)
  case(mat_crs)
    call delete(mat%crs)
  case default
    call erreur("Internal error", "unexpected internal parameter (delete_spmat)")
  endselect

endsubroutine delete_spmat


endmodule SPARSE_MAT

!------------------------------------------------------------------------------!
! Changes history
!
! Apr 2004 : creation, scalar terms
! Dec 2004 : extension to block terms
! Feb 2005 : transfered to SPMATH_DLU/BDLU
!            encapsulation of all types (DLU, BDLU, CRS, BCRS)
!------------------------------------------------------------------------------!
