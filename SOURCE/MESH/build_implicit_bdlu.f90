!------------------------------------------------------------------------------!
! Procedure : build_implicit_bdlu                       Authors : J. Gressier
!                                                       Created : Aug 2005
! Function
!   Build implicit system with jacobian matrices to BDLU structure (SPARSE MAT)
!
!------------------------------------------------------------------------------!
subroutine build_implicit_bdlu(dtloc, umesh, jacL, jacR, matbdlu)

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
type(st_ustmesh) :: umesh        ! domaine non structure a integrer
real(krp)        :: dtloc(1:umesh%ncell)
type(mnu_time)   :: deftime      ! parametres d'integration spatiale
type(st_mattab)  :: jacL, jacR   ! tableaux de jacobiennes des flux

! -- Outputs --
type(st_bdlu)    :: matbdlu

! -- Internal variables --
integer(kip)          :: i, if, ic1, ic2, ic, info, dim, dimb

! -- Body --

dimb = jacL%dim   ! dimension of a block

call new(matbdlu, dimb, umesh%ncell, umesh%nface)  ! alloc & init of sparse matrix

matbdlu%couple%fils(1:matbdlu%ncouple, 1:2) = umesh%facecell%fils(1:matbdlu%ncouple, 1:2) 

!-------------------------------------------------------
! Init diagonal

do ic = 1, umesh%ncell_int
  matbdlu%diag (1:dimb, 1:dimb, ic) = 0._krp
  do i = 1, dimb
    matbdlu%diag(i, i, ic) = umesh%mesh%volume(ic,1,1) / dtloc(ic)
  enddo
enddo

!-------------------------------------------------------
! matrix construction - internal faces only

do if = 1, umesh%nface_int

  ic1 = matbdlu%couple%fils(if,1)     ! ic1 cell is supposed to the lowest index
  ic2 = matbdlu%couple%fils(if,2)     ! ic2 cell is supposed to the highest index

  ! contribution of the face to left cell

  matbdlu%diag (1:dimb, 1:dimb, ic1) = matbdlu%diag(1:dimb, 1:dimb, ic1) + jacL%mat(1:dimb, 1:dimb, if)
  matbdlu%upper(1:dimb, 1:dimb, if)  = + jacR%mat(1:dimb, 1:dimb, if)  

  ! contribution of the face to right cell

  matbdlu%diag (1:dimb, 1:dimb, ic2) = matbdlu%diag(1:dimb, 1:dimb, ic2) - jacR%mat(1:dimb, 1:dimb, if)
  matbdlu%lower(1:dimb, 1:dimb, if)  = - jacL%mat(1:dimb, 1:dimb, if)  

enddo

!-------------------------------------------------------
! matrix construction - boundary faces only

do if = umesh%nface_int+1, umesh%nface

  ic1 = matbdlu%couple%fils(if,1)     ! ic1 cell is supposed to the lowest index  (internal cell)
  ic2 = matbdlu%couple%fils(if,2)     ! ic2 cell is supposed to the highest index (ghost    cell)

  ! contribution of the face to left cell (internal cell)

  matbdlu%diag (1:dimb, 1:dimb, ic1) = matbdlu%diag(1:dimb, 1:dimb, ic1) + jacL%mat(1:dimb, 1:dimb, if)
  matbdlu%upper(1:dimb, 1:dimb, if)  = + jacR%mat(1:dimb, 1:dimb, if) 

  ! contribution of the face to right cell

  matbdlu%diag (1:dimb, 1:dimb, ic2) = 0._krp
  do i = 1, dimb
    matbdlu%diag (i, i, ic2) = 1._krp
  enddo
  matbdlu%lower(1:dimb, 1:dimb, if)  = 0._krp

enddo


endsubroutine build_implicit_bdlu

!------------------------------------------------------------------------------!
! Change history
!
! Aug  2005 : creation
!------------------------------------------------------------------------------!
