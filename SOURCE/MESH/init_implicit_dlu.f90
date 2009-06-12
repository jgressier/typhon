!------------------------------------------------------------------------------!
! Procedure : init_implicit_dlu                         Authors : G. Grondin
!                                                       Created : Jun 2009
! Function
!   Build implicit system with jacobian matrices to DLU structure (SPARSE MAT)
!
!------------------------------------------------------------------------------!
subroutine init_implicit_dlu(dtloc, defsolver, umesh, matdlu)

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
type(st_dlu)     :: matdlu

! -- Internal variables --
integer(kip)          :: ic

! -- Body --

if (defsolver%nequat > 1) call erreur("internal error", "impossible to use DLU storage for this system of equations")

call new(matdlu, umesh%ncell, umesh%nface)  ! alloc of sparse matrix

matdlu%couple%fils(1:matdlu%ncouple, 1:2) = umesh%facecell%fils(1:matdlu%ncouple, 1:2) 

!-------------------------------------------------------
! Init diagonal

do ic = 1, umesh%ncell_int
  matdlu%diag(ic) = umesh%mesh%volume(ic,1,1) / dtloc(ic)
enddo


endsubroutine init_implicit_dlu

!------------------------------------------------------------------------------!
! Change history
!
! Jun 2009 : creation
!------------------------------------------------------------------------------!
