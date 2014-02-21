!------------------------------------------------------------------------------!
! MODULE : MATRIX                                   Author : J. Gressier
!                                                   Created: October 2006
! Fonction
!
!
!------------------------------------------------------------------------------!
module MATRIX

use TYPHMAKE

implicit none

! -- Global variables--------------- -------------------------------------------


!------------------------------------------------------------------------------!
!    DECLARATIONS
!------------------------------------------------------------------------------!


! -- INTERFACES -------------------------------------------------------------



! -- Procedures, Fonctions et Operateurs ------------------------------------
!


!------------------------------------------------------------------------------!
!    IMPLEMENTATION 
!------------------------------------------------------------------------------!
contains

!------------------------------------------------------------------------------!
! cholesky_decomp()
!   transform the matrix into its lower triangular Cholesky decompostion
!   A = L.Lt         A(n,n) supposed to be positive definite symmetric
!   only upper part of A is used
!   only lower part of A is overwritten
!------------------------------------------------------------------------------!
subroutine cholesky_decomp(a, n)
implicit none 
! --- INPUT ---
integer :: n                           ! size of matrix

! --- INPUT/OUTPUT ---
real(krp), intent(inout) :: a(n,n)     ! matrix input and output

! --- internal variables ---
integer   :: i, j, k
real(krp) :: sum

! --- BODY ---

do i = 1, n
  do j = i, n
    sum = a(i,j)
    do k = i-1, 1, -1
      sum = sum - a(i,k)*a(j,k)
    enddo
    if ( i == j ) then
      a(i,i) = sqrt(sum)
    else
      a(j,i) = sum/a(i,i)
    endif
  enddo
enddo
  
endsubroutine cholesky_decomp


!------------------------------------------------------------------------------!
! cholesky_solve()
!   computes solution of L.Lt.X=B 
!   A = L.Lt         A(n,n) contains lower part L (from cholesky_decomp)
!   B(n, p) are "p" RHS members as inputs, and "p" solutions as outputs
!------------------------------------------------------------------------------!
subroutine cholesky_solve(a, n, b, p)
implicit none 
! --- INPUTS ---
integer               :: n, p         ! size of matrix and number of RHS
real(krp), intent(in) :: a(n,n)       ! matrix input (lower part)

! --- INPUTS/OUTPUTS ---
real(krp), intent(inout) :: b(n, p)   ! "p" RHS members (dim n), solutions on output  

! --- internal variables ---
integer   :: i, j

! --- BODY ---

do i = 1, n              ! ------ solve L.y = b : y result stored in b
  if (i > 1) then
    do j = 1, i-1
      b(i,1:p) = b(i,1:p) - a(i,j)*b(j,1:p)      ! j < i : b(j) is already a "y" result
    enddo
  endif
  b(i,1:p) = b(i,1:p)/a(i,i)
enddo
  
do i = n, 1, -1          ! ------ solve Lt.x = y : x result stored in b
  if (i < n) then
    do j = i+1, n
      b(i,1:p) = b(i,1:p) - a(j,i)*b(j,1:p)      ! j > i : b(j) is already a "X" result
    enddo
  endif
  b(i,1:p) = b(i,1:p)/a(i,i)
enddo
  
endsubroutine cholesky_solve

endmodule MATRIX
!------------------------------------------------------------------------------!
! Changes history
!
! Oct   2006 : module creation
!------------------------------------------------------------------------------!
