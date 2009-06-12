!------------------------------------------------------------------------------!
! Procedure : build_implicit_dlu                Authors : J. Gressier
!
! Function
!   Build implicit system with jacobian matrices to DLU structure (SPARSE MAT)
!
! Defaults/Limitations/Misc :
!
!------------------------------------------------------------------------------!
subroutine build_implicit_dlu(dtloc, umesh, jacL, jacR, matdlu)

use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_SOLVER
use MENU_NUM
use USTMESH
use DEFFIELD
use MATRIX_ARRAY
use SPARSE_MAT

implicit none

! -- Inputs --
type(st_ustmesh) :: umesh        ! unstructured mesh
real(krp)        :: dtloc(1:umesh%ncell)
type(mnu_time)   :: deftime      ! time integration parameter
type(st_mattab)  :: jacL, jacR   ! flux jacobian matrices

! -- Outputs --
type(st_dlu)     :: matdlu

! -- Internal variables --
integer(kip)          :: if, ic1, ic2, ic, info, dim

! -- Body --

!-----------------------
! compatibility test
!-----------------------
if (jacL%dim > 1) call erreur("internal error", "impossible to use DLU storage for this system of equations")

!-------------------------------------------------------
! matrix construction - internal faces only

do if = 1, umesh%nface_int

  ic1 = matdlu%couple%fils(if,1)     ! ic1 cell is supposed to the lowest index
  ic2 = matdlu%couple%fils(if,2)     ! ic2 cell is supposed to the highest index

  ! contribution of the face to left cell

  matdlu%diag(ic1) = matdlu%diag(ic1) + jacL%mat(1,1,if)
  matdlu%upper(if) = + jacR%mat(1,1,if)  

  ! contribution of the face to right cell

  matdlu%diag(ic2) = matdlu%diag(ic2) - jacR%mat(1,1,if)
  matdlu%lower(if) = - jacL%mat(1,1,if)  

enddo

!-------------------------------------------------------
! matrix construction - boundary faces only

do if = umesh%nface_int+1, umesh%nface

  ic1 = matdlu%couple%fils(if,1)     ! ic1 cell is supposed to the lowest index  (internal cell)
  ic2 = matdlu%couple%fils(if,2)     ! ic2 cell is supposed to the highest index (ghost    cell)

  ! contribution of the face to left cell (internal cell)

  matdlu%diag(ic1) = matdlu%diag(ic1) + jacL%mat(1,1,if)
  matdlu%upper(if) = + jacR%mat(1,1,if) 

  ! contribution of the face to right cell

  matdlu%diag(ic2) = 1._krp
  matdlu%lower(if) = 0._krp

enddo


endsubroutine build_implicit_dlu

!------------------------------------------------------------------------------!
! Changes history
!
! Aug 2005 : creation
! Aug 2005 : ghost cells have been added to the DLU system
!------------------------------------------------------------------------------!
