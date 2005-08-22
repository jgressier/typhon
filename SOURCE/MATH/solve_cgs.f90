!------------------------------------------------------------------------------!
! Procedure : solve_cgs                               Authors : J. Gressier
!                                                     Created : August 2005
! Fonction
!   Solve implicit system with Conjugate Gradient Squared iterative method
!
!------------------------------------------------------------------------------!
subroutine solve_cgs(deftime, mat, rhs, info)

use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_NUM
use GENFIELD
use MATRIX_ARRAY
use SPARSE_MAT

implicit none

! -- Inputs --
type(mnu_time)        :: deftime       ! time integration parameter
type(st_spmat)        :: mat

! -- Input/output --
real(krp)             :: rhs(*)        ! rhs vector

! -- Ouputs --
integer(kpp)          :: info

! -- Internal variables --


! -- BODY --

select case(mat%type)
case(mat_dlu)
  call dlu_cgs(deftime%implicite, mat%dlu, rhs, rhs, info)

case(mat_bdlu)
  call erreur("development","CGS method not implemented with BDLU storage")

case(mat_crs)
  call erreur("development","CGS method not implemented with CRS storage")

case(mat_bcrs)
  call erreur("development","CGS method not implemented with BCRS storage")

case default
  call erreur("Internal error","unknown storage method for sparse matrix")
endselect


endsubroutine solve_cgs
!------------------------------------------------------------------------------!
! Change History
!
! Aug  2005 : creation 
!------------------------------------------------------------------------------!
