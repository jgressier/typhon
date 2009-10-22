!------------------------------------------------------------------------------!
! Procedure : init_implicit_bdlu                        Authors : G. Grondin
!                                                       Created : Jun 2009
! Function
!   Build implicit system with jacobian matrices to BDLU structure (SPARSE MAT)
!
!------------------------------------------------------------------------------!
subroutine init_implicit_bdlu(dtloc, defsolver, umesh, matbdlu)

use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_SOLVER
use MENU_NUM
use USTMESH
use DEFFIELD
use SPARSE_MAT

implicit none

! -- Inputs --
type(st_ustmesh) :: umesh        ! unstructured mesh
real(krp)        :: dtloc(1:umesh%ncell)
type(mnu_solver) :: defsolver    ! solver parameters

! -- Outputs --
type(st_bdlu)    :: matbdlu

! -- Internal variables --
integer(kip)          :: ic
integer(kip)          :: i, dimb

! -- Body --

dimb = defsolver%nequat   ! dimension of a block

call new(matbdlu, dimb, umesh%ncell, umesh%nface)  ! alloc of sparse matrix

matbdlu%couple%fils(1:matbdlu%ncouple, 1:2) = umesh%facecell%fils(1:matbdlu%ncouple, 1:2)

!-------------------------------------------------------
! Init diagonal

do ic = 1, umesh%ncell_int
  matbdlu%diag (1:dimb, 1:dimb, ic) = 0._krp
  do i = 1, dimb
    matbdlu%diag(i, i, ic) = 1._krp ! umesh%mesh%volume(ic,1,1) / dtloc(ic)
  enddo
enddo


endsubroutine init_implicit_bdlu

!------------------------------------------------------------------------------!
! Change history
!
! Jun 2009 : creation
!------------------------------------------------------------------------------!
