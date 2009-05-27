!------------------------------------------------------------------------------!
! MODULE : SPMAT_DLU                                Authors : J. Gressier
!                                                   Created : Avril 2004
! Fonction
!   Definition des structures de stockage de matrices creuses
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

module SPMAT_DLU

use TYPHMAKE       ! Definition de la precision
use CONNECTIVITY   

implicit none

! -- Variables globales du module -------------------------------------------



! -- DECLARATIONS -----------------------------------------------------------

!------------------------------------------------------------------------------!
! structure DLU : matrice a REMPLISSAGE symetrique
!------------------------------------------------------------------------------!
type st_dlu
  logical            :: sort
  logical            :: diaginvert
  integer(kip)       :: dim
  integer(kip)       :: ncouple
  real(krp), dimension(:), pointer :: diag     ! coefficient de la diagonale
  real(krp), dimension(:), pointer :: lower    ! coefficient triang. inf
  real(krp), dimension(:), pointer :: upper    ! coefficient triang. inf
  type(st_connect)   :: couple   ! liste (dim) des couples
endtype st_dlu


! -- INTERFACES -------------------------------------------------------------
interface new
  module procedure new_dlu
endinterface

interface delete
  module procedure delete_dlu
endinterface

! -- Fonctions et Operateurs ------------------------------------------------


! -- IMPLEMENTATION ---------------------------------------------------------
contains

!------------------------------------------------------------------------------!
! new_dlu : allocate DLU structure
!------------------------------------------------------------------------------!
subroutine new_dlu(mat, dim, ncouple)
implicit none
! - parametres
type(st_dlu) :: mat
integer(kip) :: dim, ncouple

  mat%sort       = .false.
  mat%diaginvert = .false.
  mat%dim      = dim
  mat%ncouple = ncouple
  allocate(mat% diag(dim    )) ;   mat% diag(:) = 0._krp
  allocate(mat%lower(ncouple)) ;   mat%lower(:) = 0._krp
  allocate(mat%upper(ncouple)) ;   mat%upper(:) = 0._krp
  call new(mat%couple, ncouple, 2)

endsubroutine new_dlu

!------------------------------------------------------------------------------!
! delete_dlu : remove DLU structure
!------------------------------------------------------------------------------!
subroutine delete_dlu(mat)
implicit none
! - parametres
type(st_dlu) :: mat

  if (associated(mat%diag)) then
    deallocate(mat%diag)
    deallocate(mat%lower)
    deallocate(mat%upper)
    call delete(mat%couple)
  endif

endsubroutine delete_dlu


!------------------------------------------------------------------------------!
! sort_dlu : sort elements in DLU structure so that index1 < index2
!------------------------------------------------------------------------------!
subroutine sort_dlu(mat)
implicit none
! - parametres
type(st_dlu) :: mat
! - internal
real(krp)    :: x
integer      :: i, if

  if (.not.mat%sort) then
    do if = 1, mat%ncouple
      if (mat%couple%fils(if,1) > mat%couple%fils(if,2)) then
        x             = mat%lower(if) 
        mat%lower(if) = mat%upper(if)
        mat%upper(if) = x
        i                     = mat%couple%fils(if,2)
        mat%couple%fils(if,2) = mat%couple%fils(if,1)
        mat%couple%fils(if,1) = i
      endif
    enddo
    mat%sort = .true.
  endif

endsubroutine sort_dlu


!------------------------------------------------------------------------------!
! invertdiag_dlu : invert diagonal block elements
!------------------------------------------------------------------------------!
subroutine invertdiag_dlu(mat)
implicit none
! - parametres
type(st_dlu) :: mat
! - internal
real(krp)    :: x
integer      :: i, if


endsubroutine invertdiag_dlu


!------------------------------------------------------------------------------!
! dlu_yeqax : y = A.x
!------------------------------------------------------------------------------!
subroutine dlu_yeqax(y, mat, x)
implicit none
! -- parameters --
type(st_dlu)                 , intent(in)  :: mat
real(krp), dimension(mat%dim), intent(in)  :: x
real(krp), dimension(mat%dim), intent(out) :: y
! -- internal --
integer(kip) :: if, imin, imax

y(1:mat%dim) = mat%diag(1:mat%dim)*x(1:mat%dim)

do if = 1, mat%ncouple
  imin = mat%couple%fils(if,1)    ! ic1 cell is supposed to be the lowest index
  imax = mat%couple%fils(if,2)    ! ic2 cell is supposed to be the highest index
  !!! no test that the index is lower than dim !!!
  y(imax) = y(imax) + mat%lower(if)*x(imin)
  y(imin) = y(imin) + mat%upper(if)*x(imax)
enddo

endsubroutine dlu_yeqax


!------------------------------------------------------------------------------!
! dlu_yeqatx : y = At.x
!------------------------------------------------------------------------------!
subroutine dlu_yeqatx(y, mat, x)
implicit none
! -- parameters --
type(st_dlu)                 , intent(in)  :: mat
real(krp), dimension(mat%dim), intent(in)  :: x
real(krp), dimension(mat%dim), intent(out) :: y
! -- internal --
integer(kip) :: if, imin, imax

! -- body --

y(1:mat%dim) = mat%diag(1:mat%dim)*x(1:mat%dim)

do if = 1, mat%ncouple
  imin = mat%couple%fils(if,1)    ! ic1 cell is supposed to be the lowest index
  imax = mat%couple%fils(if,2)    ! ic2 cell is supposed to be the highest index
  !!! no test that the index is lower than dim !!!
  y(imax) = y(imax) + mat%upper(if)*x(imin)
  y(imin) = y(imin) + mat%lower(if)*x(imax)
enddo

endsubroutine dlu_yeqatx


!------------------------------------------------------------------------------!
! dlu_xeqaxpy : x = A.x + y
!------------------------------------------------------------------------------!
subroutine dlu_xeqaxpy(x, mat, y, p)
implicit none
! -- parameters --
type(st_dlu)                 , intent(in)    :: mat
real(krp), dimension(mat%dim), intent(in)    :: y
real(krp), dimension(mat%dim), intent(out)   :: p  ! p is a transient variable
real(krp), dimension(mat%dim), intent(inout) :: x
! -- internal --
integer(kip) :: if, imin, imax

! -- body --

p(1:mat%dim) = x(1:mat%dim)
x(1:mat%dim) = y(1:mat%dim) + mat%diag(1:mat%dim)*p(1:mat%dim) 
do if = 1, mat%ncouple
  imin = mat%couple%fils(if,1)    ! ic1 cell is supposed to be the lowest index
  imax = mat%couple%fils(if,2)    ! ic2 cell is supposed to be the highest index
  !!! no test that the index is lower than dim !!!
  x(imax) = x(imax) + mat%lower(if)*p(imin)
  x(imin) = x(imin) + mat%upper(if)*p(imax)
enddo

endsubroutine dlu_xeqaxpy


!------------------------------------------------------------------------------!
! dlu_yeqmaxpz : y = - A.x + z
!------------------------------------------------------------------------------!
subroutine dlu_yeqmaxpz(y, mat, x, z)
implicit none
! -- parameters --
type(st_dlu)                 , intent(in)  :: mat
real(krp), dimension(mat%dim), intent(in)  :: x, z
real(krp), dimension(mat%dim), intent(out) :: y
! -- internal --
integer(kip) :: if, imin, imax

! -- body --

y(1:mat%dim) = z(1:mat%dim) - mat%diag(1:mat%dim)*x(1:mat%dim) 
do if = 1, mat%ncouple
  imin = mat%couple%fils(if,1)    ! ic1 cell is supposed to be the lowest index
  imax = mat%couple%fils(if,2)    ! ic2 cell is supposed to be the highest index
  !!! no test that the index is lower than dim !!!
  y(imax) = y(imax) - mat%lower(if)*x(imin)
  y(imin) = y(imin) - mat%upper(if)*x(imax)
enddo

endsubroutine dlu_yeqmaxpz


endmodule SPMAT_DLU


!------------------------------------------------------------------------------!
! Change History
!
! Apr 2004 : created, scalar terms
! Dec 2004 : extension to block terms
! Jul 2005 : internal operations
!------------------------------------------------------------------------------!
