!------------------------------------------------------------------------------!
! Procedure : build_implicit                            Authors : J. Gressier
!                                                       Created : Aug 2005
! Function
!   Build implicit system with jacobian matrices to different structures
!
!------------------------------------------------------------------------------!
subroutine build_implicit(dt, deftime, umesh, jacL, jacR, mat, residu)

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
type(mnu_time)   :: deftime      ! time integration parameters
type(st_ustmesh) :: umesh        ! unstructured mesh
type(st_mattab)  :: jacL, jacR   ! jacobian matrices related to faces


! -- Inputs/Outputs --
type(st_genericfield) :: residu  ! residuals

! -- Outputs --
type(st_spmat)   :: mat

! -- Internal variables --
integer(kip)     :: iv, ic, info, dim

! -- Body --

!!!-----------------------------------------------------
!!! suppression de l'influence des cellules limites 
!!! l'eventuelle dependance de la cellule gauche via la cellule limite
!!! doit deja etre dans jacL
!!
!!do if = 1, umesh%nface
!!  if (umesh%facecell%fils(if,2) > umesh%ncell_int) jacR%mat(:,:,if) = 0._krp
!!enddo

!-----------------------------------------------------
! build SPARSE MATRIX
!-----------------------------------------------------

mat%type = deftime%implicite%storage

select case(mat%type)

case(mat_dlu)
  call build_implicit_dlu(dt, umesh, jacL, jacR, mat%dlu)

case(mat_bdlu)
  call build_implicit_bdlu(dt, umesh, jacL, jacR, mat%bdlu)

case(mat_crs, mat_bcrs)
  call erreur("development","unexpected matrix structure (not yet implemented)")

case default
  call erreur("internal error","unknown matrix structure to build implicit system")
endselect

!-----------------------------------------------------
! RHS correction
!-----------------------------------------------------

do iv = 1, residu%nscal
  residu%tabscal(iv)%scal(umesh%ncell_int+1:umesh%ncell) = 0._krp
enddo
do iv = 1, residu%nvect
  do ic = umesh%ncell_int+1, umesh%ncell
    residu%tabvect(iv)%vect(ic) = v3d(0._krp, 0._krp, 0._krp)
  enddo
enddo

endsubroutine build_implicit
!------------------------------------------------------------------------------!
! Change history
!
! Aug  2005 : creation / add bdlu construction
!------------------------------------------------------------------------------!
