!------------------------------------------------------------------------------!
! Procedure : build_implicit_dlu                        Authors : J. Gressier
!                                                       Created : Aug 2005
! Function
!   Build implicit system with jacobian matrices to DLU structure (SPARSE MAT)
!
!------------------------------------------------------------------------------!
subroutine build_implicit_dlu(dt, umesh, jacL, jacR, matdlu)

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
real(krp)        :: dt
type(mnu_time)   :: deftime      ! parametres d'integration spatiale
type(st_ustmesh) :: umesh        ! domaine non structure a integrer
type(st_mattab)  :: jacL, jacR   ! tableaux de jacobiennes des flux

! -- Outputs --
type(st_dlu)     :: matdlu

! -- Internal variables --
integer(kip)          :: if, ic1, ic2, ic, info, dim

! -- Body --

!-----------------------
! compatibility test
!-----------------------
if (jacL%dim > 1) call erreur("internal error", "impossible to use DLU storage for this system of equations")

!-----------------------

! DEV : on ne devrait allouer que les faces internes

call new(matdlu, umesh%ncell_int, umesh%nface)  ! alloc & init of sparse matrix

matdlu%couple%fils(1:matdlu%ncouple, 1:2) = umesh%facecell%fils(1:matdlu%ncouple, 1:2) 

! construction de la matrice

do if = 1, matdlu%ncouple
  ic1 = matdlu%couple%fils(if,1)    
  ic2 = matdlu%couple%fils(if,2)    
  ! bilan cellule a gauche de la face
  matdlu%diag(ic1) = matdlu%diag(ic1) + jacL%mat(1,1,if)
  matdlu%upper(if) = + jacR%mat(1,1,if)    ! ic1 cell is supposed to the lowest index
  ! bilan cellule a droite de la face
  if (ic2 <= matdlu%dim) then
    matdlu%diag(ic2) = matdlu%diag(ic2) - jacR%mat(1,1,if)
    matdlu%lower(if) = - jacL%mat(1,1,if)  ! ic2 cell is supposed to the highest index
  endif
enddo

do ic = 1, matdlu%dim
  matdlu%diag(ic) = matdlu%diag(ic) + umesh%mesh%volume(ic,1,1) / dt
  !matdlu%diag(ic) = umesh%mesh%volume(ic,1,1) / dt
enddo


endsubroutine build_implicit_dlu

!------------------------------------------------------------------------------!
! Change history
!
! Aug  2005 : creation
!------------------------------------------------------------------------------!
