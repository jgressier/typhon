!------------------------------------------------------------------------------!
! MODULE : BASEFIELD                            Authors : J. Gressier
!
! Function
!   Library of scalar, vector and tensor fields operations for all solvers
!
! Defaults/Limitations/Misc :
!
!------------------------------------------------------------------------------!

module BASEFIELD

use MESHPREC     ! Precision and string size
!use OUTPUT
use VEC3D        !
use TENSOR3      !

implicit none

! -- Module global variables ------------------------------------------------

integer, parameter :: nghostcell = 1

! -- DECLARATIONS -----------------------------------------------------------

!------------------------------------------------------------------------------!
! ST_SCAFIELD structure definition : physical scalar field
!------------------------------------------------------------------------------!

type st_scafield
  integer :: dim                            ! cell number
  integer :: quantity_id
  real(krp), dimension(:), pointer :: scal  ! scalar field
endtype st_scafield

!------------------------------------------------------------------------------!
! ST_VECFIELD structure definition : physical vector field
!------------------------------------------------------------------------------!

type st_vecfield
  integer :: dim                            ! cell number
  integer :: quantity_id
  type(v3d), dimension(:), pointer :: vect  ! vector field
endtype st_vecfield

!------------------------------------------------------------------------------!
! ST_TENFIELD structure definition : physical tensor field
!------------------------------------------------------------------------------!

type st_tenfield
  integer :: dim                            ! cell number
  integer :: quantity_id
  type(t3d), dimension(:), pointer :: tens  ! tensor field
endtype st_tenfield


! -- INTERFACES -------------------------------------------------------------

interface new
  module procedure new_scafield, new_vecfield, new_tenfield
endinterface

interface delete
  module procedure delete_scafield, delete_vecfield, delete_tenfield
endinterface

interface transfer
  module procedure transfer_scafield, transfer_vecfield, transfer_tenfield
endinterface

interface scale
  module procedure scaf_scale, vecf_scale, tenf_scale
endinterface

interface xeqxpy
  module procedure scaf_xeqxpy, vecf_xeqxpy, tenf_xeqxpy
endinterface

interface xeqxpay
  module procedure scaf_xeqxpay, vecf_xeqxpay, tenf_xeqxpay
endinterface

interface l1norm
  module procedure scaf_l1norm, vecf_l1norm, tenf_l1norm
endinterface

interface l2sqnorm
  module procedure scaf_l2sqnorm, vecf_l2sqnorm, tenf_l2sqnorm
endinterface

interface l2norm
  module procedure scaf_l2norm, vecf_l2norm, tenf_l2norm
endinterface

interface dot_prod
  module procedure scaf_dot_prod, vecf_dot_prod, tenf_dot_prod
endinterface

! -- Functions and Operators ------------------------------------------------


! -- IMPLEMENTATION ---------------------------------------------------------
contains


!------------------------------------------------------------------------------!
! Procedure : ST_SCAFIELD structure allocation
!------------------------------------------------------------------------------!
subroutine new_scafield(scafield, dim)
implicit none
type(st_scafield) :: scafield          ! scalar field
integer           :: dim               ! dimension

scafield%dim = dim
if (scafield%dim > 0) then
  allocate(scafield%scal(scafield%dim))
endif

endsubroutine new_scafield


!------------------------------------------------------------------------------!
! Procedure : ST_VECFIELD structure allocation
!------------------------------------------------------------------------------!
subroutine new_vecfield(vecfield, dim)
implicit none
type(st_vecfield) :: vecfield          ! vector field
integer           :: dim               ! dimension

vecfield%dim = dim
if (vecfield%dim > 0) then
  allocate(vecfield%vect(dim))
endif

endsubroutine new_vecfield


!------------------------------------------------------------------------------!
! Procedure : ST_TENFIELD structure allocation
!------------------------------------------------------------------------------!
subroutine new_tenfield(tenfield, dim)
implicit none
type(st_tenfield) :: tenfield          ! tensor field
integer           :: dim               ! dimension

tenfield%dim = dim
if (tenfield%dim > 0) then
  allocate(tenfield%tens(dim))
endif

endsubroutine new_tenfield


!------------------------------------------------------------------------------!
! Procedure : ST_SCAFIELD structure deallocation
!------------------------------------------------------------------------------!
subroutine delete_scafield(scafield)
implicit none
type(st_scafield) :: scafield

deallocate(scafield%scal)

endsubroutine delete_scafield


!------------------------------------------------------------------------------!
! Procedure : ST_VECFIELD structure deallocation
!------------------------------------------------------------------------------!
subroutine delete_vecfield(vecfield)
implicit none
type(st_vecfield) :: vecfield

deallocate(vecfield%vect)

endsubroutine delete_vecfield


!------------------------------------------------------------------------------!
! Procedure : ST_TENFIELD structure deallocation
!------------------------------------------------------------------------------!
subroutine delete_tenfield(tenfield)
implicit none
type(st_tenfield) :: tenfield

deallocate(tenfield%tens)

endsubroutine delete_tenfield


!------------------------------------------------------------------------------
!  Procedure : scalar field transfer : rscafield receives iscafield
!------------------------------------------------------------------------------
subroutine transfer_scafield(rscafield,iscafield)
implicit none
type(st_scafield) :: iscafield, rscafield
integer           :: i

rscafield%dim= iscafield%dim
do i = 1, iscafield%dim
  rscafield%scal(i) = iscafield%scal(i)
enddo

endsubroutine transfer_scafield


!------------------------------------------------------------------------------
! Procedure : vector field transfer : rvecfield receives ivecfield
!------------------------------------------------------------------------------
subroutine transfer_vecfield(rvecfield,ivecfield)
implicit none
type(st_vecfield) :: ivecfield, rvecfield
integer           :: i

rvecfield%dim= ivecfield%dim
do i = 1, ivecfield%dim
  rvecfield%vect(i)= ivecfield%vect(i)
enddo

endsubroutine transfer_vecfield


!------------------------------------------------------------------------------
! Procedure : tensor field transfer : rtenfield receives itenfield
!------------------------------------------------------------------------------
subroutine transfer_tenfield(rtenfield,itenfield)
implicit none
type(st_tenfield) :: itenfield, rtenfield
integer           :: i

rtenfield%dim= itenfield%dim
do i = 1, itenfield%dim
  rtenfield%tens(i)= itenfield%tens(i)
enddo

endsubroutine transfer_tenfield


!------------------------------------------------------------------------------
! Computing routine : scaf_scale : X = a*X
!------------------------------------------------------------------------------
subroutine scaf_scale(x, a)
implicit none
type(st_scafield) :: x
real(krp)         :: a
integer           :: i

do i = 1, x%dim
  x%scal(i) = a*x%scal(i)
enddo

endsubroutine scaf_scale


!------------------------------------------------------------------------------
! Computing routine : vecf_scale : X = a*X
!------------------------------------------------------------------------------
subroutine vecf_scale(x, a)
implicit none
type(st_vecfield) :: x
real(krp)         :: a

call scale(x%vect(1:x%dim), a)

endsubroutine vecf_scale


!------------------------------------------------------------------------------
! Computing routine : tenf_scale : X = a*X
!------------------------------------------------------------------------------
subroutine tenf_scale(x, a)
implicit none
type(st_tenfield) :: x
integer           :: i
real(krp)         :: a

do i = 1, x%dim
  x%tens(i)%mat = a*x%tens(i)%mat
enddo

endsubroutine tenf_scale


!------------------------------------------------------------------------------
! Computing routine : scaf_xeqxpy : X = X + Y
!------------------------------------------------------------------------------
subroutine scaf_xeqxpy(x, y)
implicit none
type(st_scafield) :: x, y
integer           :: i

do i = 1, x%dim
  x%scal(i) = x%scal(i) + y%scal(i)
enddo

endsubroutine scaf_xeqxpy


!------------------------------------------------------------------------------
! Computing routine : vecf_xeqxpy : X = X + Y
!------------------------------------------------------------------------------
subroutine vecf_xeqxpy(x, y)
implicit none
type(st_vecfield) :: x, y

call shift_add(x%vect(1:x%dim), y%vect(1:x%dim))

endsubroutine vecf_xeqxpy


!------------------------------------------------------------------------------
! Computing routine : tenf_xeqxpy : X = X + Y
!------------------------------------------------------------------------------
subroutine tenf_xeqxpy(x, y)
implicit none
type(st_tenfield) :: x, y
integer           :: i

do i = 1, x%dim
  x%tens(i)%mat = x%tens(i)%mat + y%tens(i)%mat
enddo

endsubroutine tenf_xeqxpy


!------------------------------------------------------------------------------
! Computing routine : scaf_xeqxpay : X = X + a*Y
!------------------------------------------------------------------------------
subroutine scaf_xeqxpay(x, a, y)
implicit none
type(st_scafield) :: x, y
real(krp)         :: a
integer           :: i

do i = 1, x%dim
  x%scal(i) = x%scal(i) + a*y%scal(i)
enddo

endsubroutine scaf_xeqxpay


!------------------------------------------------------------------------------
! Computing routine : vecf_xeqxpay : X = X + a*Y
!------------------------------------------------------------------------------
subroutine vecf_xeqxpay(x, a, y)
implicit none
type(st_vecfield) :: x, y
real(krp)         :: a
integer           :: i

do i = 1, x%dim
  x%vect(i) = x%vect(i) + a*y%vect(i)
enddo

endsubroutine vecf_xeqxpay


!------------------------------------------------------------------------------
! Computing routine : tenf_xeqxpay : X = X + a*Y
!------------------------------------------------------------------------------
subroutine tenf_xeqxpay(x, a, y)
implicit none
type(st_tenfield) :: x, y
real(krp)         :: a
integer           :: i

do i = 1, x%dim
  x%tens(i)%mat = x%tens(i)%mat + a*y%tens(i)%mat
enddo

endsubroutine tenf_xeqxpay


!------------------------------------------------------------------------------
! Computing routine : scaf_l1norm : n = sum(abs(x(i)))
!------------------------------------------------------------------------------
real(krp) function scaf_l1norm(x)
implicit none
type(st_scafield) :: x
integer           :: i

scaf_l1norm = 0.0_krp
do i = 1, x%dim
  scaf_l1norm = scaf_l1norm + abs(x%scal(i))
enddo

endfunction scaf_l1norm

!------------------------------------------------------------------------------
! Computing routine : vecf_l1norm : n = sum(abs(x(i)))
!------------------------------------------------------------------------------
real(krp) function vecf_l1norm(x)
implicit none
type(st_vecfield) :: x
integer           :: i

vecf_l1norm = 0.0_krp
do i = 1, x%dim
  vecf_l1norm = vecf_l1norm + abs(x%vect(i))
enddo

endfunction vecf_l1norm

!------------------------------------------------------------------------------
! Computing routine : tenf_l1norm : n = sum(abs(x(i)))
!------------------------------------------------------------------------------
real(krp) function tenf_l1norm(x)
implicit none
type(st_tenfield) :: x
integer           :: i

tenf_l1norm = 0.0_krp
do i = 1, x%dim
  tenf_l1norm = tenf_l1norm + sum(abs(x%tens(i)%mat(1:3,1:3)))
enddo

endfunction tenf_l1norm

!------------------------------------------------------------------------------
! Computing routine : scaf_l2sqnorm : n = sqrt(sum(x(i)^2))
!------------------------------------------------------------------------------
real(krp) function scaf_l2sqnorm(x)
implicit none
type(st_scafield) :: x
integer           :: i

scaf_l2sqnorm = 0.0_krp
do i = 1, x%dim
  scaf_l2sqnorm = scaf_l2sqnorm + x%scal(i)*x%scal(i)
enddo

endfunction scaf_l2sqnorm

!------------------------------------------------------------------------------
! Computing routine : vecf_l2sqnorm : n = sqrt(sum(x(i).x(i)))
!------------------------------------------------------------------------------
real(krp) function vecf_l2sqnorm(x)
implicit none
type(st_vecfield) :: x
integer           :: i

vecf_l2sqnorm = 0.0_krp
do i = 1, x%dim
  vecf_l2sqnorm = vecf_l2sqnorm + sqrabs(x%vect(i))
enddo

endfunction vecf_l2sqnorm

!------------------------------------------------------------------------------
! Computing routine : tenf_l2sqnorm : n = sqrt(sum(x(i)::x(i)))
!------------------------------------------------------------------------------
real(krp) function tenf_l2sqnorm(x)
implicit none
type(st_tenfield) :: x
integer           :: i, ii, jj

tenf_l2sqnorm = 0.0_krp
do i = 1, x%dim
  do ii = 1,3
    do jj = 1,3
      tenf_l2sqnorm = tenf_l2sqnorm + x%tens(i)%mat(ii,jj)*x%tens(i)%mat(ii,jj)
    enddo
  enddo
enddo

endfunction tenf_l2sqnorm

!------------------------------------------------------------------------------
! Computing routine : scaf_l2norm : n = sqrt(sum(x(i)^2))
!------------------------------------------------------------------------------
real(krp) function scaf_l2norm(x)
implicit none
type(st_scafield) :: x
integer           :: i

scaf_l2norm = 0.0_krp
do i = 1, x%dim
  scaf_l2norm = scaf_l2norm + x%scal(i)*x%scal(i)
enddo
scaf_l2norm = sqrt(scaf_l2norm)

endfunction scaf_l2norm

!------------------------------------------------------------------------------
! Computing routine : vecf_l2norm : n = sqrt(sum(x(i).x(i)))
!------------------------------------------------------------------------------
real(krp) function vecf_l2norm(x)
implicit none
type(st_vecfield) :: x
integer           :: i

vecf_l2norm = 0.0_krp
do i = 1, x%dim
  vecf_l2norm = vecf_l2norm + sqrabs(x%vect(i))
enddo
vecf_l2norm = sqrt(vecf_l2norm)

endfunction vecf_l2norm

!------------------------------------------------------------------------------
! Computing routine : tenf_l2norm : n = sqrt(sum(x(i)::x(i)))
!------------------------------------------------------------------------------
real(krp) function tenf_l2norm(x)
implicit none
type(st_tenfield) :: x
integer           :: i, ii, jj

tenf_l2norm = 0.0_krp
do i = 1, x%dim
  do ii = 1,3
    do jj = 1,3
      tenf_l2norm = tenf_l2norm + x%tens(i)%mat(ii,jj)*x%tens(i)%mat(ii,jj)
    enddo
  enddo
enddo
tenf_l2norm = sqrt(tenf_l2norm)

endfunction tenf_l2norm

!------------------------------------------------------------------------------
! Computing routine : scaf_dot_prod : n = sum(x(i)*y(i))
!------------------------------------------------------------------------------
real(krp) function scaf_dot_prod(x, y)
implicit none
type(st_scafield) :: x, y
integer           :: i

scaf_dot_prod = 0.0_krp
do i = 1, x%dim
  scaf_dot_prod = scaf_dot_prod + x%scal(i)*y%scal(i)
enddo

endfunction scaf_dot_prod

!------------------------------------------------------------------------------
! Computing routine : vecf_dot_prod : n = sum(x(i).y(i))
!------------------------------------------------------------------------------
real(krp) function vecf_dot_prod(x, y)
implicit none
type(st_vecfield) :: x, y
type(v3d) :: vv
integer           :: i

vecf_dot_prod = 0.0_krp
do i = 1, x%dim
  vecf_dot_prod = vecf_dot_prod + (x%vect(i).scal.y%vect(i))
enddo

endfunction vecf_dot_prod

!------------------------------------------------------------------------------
! Computing routine : tenf_dot_prod : n = sum(x(i):y(i))
!------------------------------------------------------------------------------
real(krp) function tenf_dot_prod(x, y)
implicit none
type(st_tenfield) :: x, y
integer           :: i, ii, jj

tenf_dot_prod = 0.0_krp
do i = 1, x%dim
  do ii = 1,3
    do jj = 1,3
      tenf_dot_prod = tenf_dot_prod + x%tens(i)%mat(ii,jj)*y%tens(i)%mat(ii,jj)
    enddo
  enddo
enddo

endfunction tenf_dot_prod


endmodule BASEFIELD

!------------------------------------------------------------------------------!
! Changes history
!
! Oct 2002 : creation
! Jun 2003 : field structure by type (scalar, vector, tensor)
! DEV: field/array interface
! DEV: split in MGFIELD and MZFIELD pour low- and high-level functions haut
! Jun 2004 : procedures insert_newgfield and delete_chainedgfield
! Nov 2004 : split GENFIELD -> GENFIELD/BASEFIELD
! Nov 2007 : X = X + Y ; X = X + A*Y routines
!------------------------------------------------------------------------------!
