!------------------------------------------------------------------------------!
! Procedure : solve_bicg                              Authors : J. Gressier
!                                                     Created : August 2005
! Fonction
!   Solve implicit system with Bi-Conjugate Gradient iterative method
!
!------------------------------------------------------------------------------!
subroutine solve_bicg(deftime, mat, rhs, info)

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
  call dlu_bicg(deftime%implicite, mat%dlu, rhs, info)

case(mat_bdlu)
  call bdlu_bicg(deftime%implicite, mat%bdlu, rhs, info)

case(mat_crs)
  call erreur("development","BICG method not implemented with CRS storage")

case(mat_bcrs)
  call erreur("development","BICG method not implemented with BCRS storage")

case default
  call erreur("Internal error","unknown storage method for sparse matrix")
endselect


endsubroutine solve_bicg
!------------------------------------------------------------------------------!
! Change History
!
! Aug  2005 : creation 
!------------------------------------------------------------------------------!
