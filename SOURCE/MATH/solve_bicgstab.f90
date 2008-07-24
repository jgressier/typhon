!------------------------------------------------------------------------------!
! Procedure : solve_bicgstab                          Authors : J. Gressier
!                                                     Created : August 2005
! Fonction
!   Solve implicit system with Bi-Conjugate Gradient Stabilized iterative method
!
!------------------------------------------------------------------------------!
subroutine solve_bicgstab(deftime, mat, rhs, info)

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
  call erreur("development","BICG-Stabilized method not implemented with DLU storage")

case(mat_bdlu)
  call bdlu_bicgstab(deftime%implicite, mat%bdlu, rhs, info)

case(mat_crs)
  call erreur("development","BICG-Stabilized method not implemented with CRS storage")

case(mat_bcrs)
  call erreur("development","BICG-Stabilized method not implemented with BCRS storage")

case default
  call erreur("Internal error","unknown storage method for sparse matrix")
endselect


endsubroutine solve_bicgstab
!------------------------------------------------------------------------------!
! Change History
!
! Aug  2005 : creation
!------------------------------------------------------------------------------!
