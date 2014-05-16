!------------------------------------------------------------------------------!
! MODULE : LIBSORT                                  Authors: J. Gressier
!                                                   Created: October 2005
! Fonction
!   Compressed Row Storage format (definition & conversion)
!
!------------------------------------------------------------------------------!
module LIBSORT

use CONNECTIVITY 
use IOCFD 

implicit none

! -- Variables globales du module -------------------------------------------


! -- DECLARATIONS -----------------------------------------------------------


! -- INTERFACES -------------------------------------------------------------

interface sort_insertion
  module procedure sort_insertion_real, sort_insertion_integer
endinterface

interface sort_shell
  module procedure sort_shell_real
endinterface

interface sort_heap
  module procedure sort_heap_real, sort_heap_integer
endinterface


! -- Fonctions et Operateurs ------------------------------------------------



! -- IMPLEMENTATION ---------------------------------------------------------
contains


!------------------------------------------------------------------------------!
! Procedure : sort_insertion_real
!   Straight Insertion Method 
!   (N^2 method should only be used for relatively small arrays (N<100))
!------------------------------------------------------------------------------!
SUBROUTINE sort_insertion_real(ARR, n)
  implicit none
  ! -- inputs --
  integer(kip) :: n        ! size of array to sort
  real(krp)    :: arr(N)   ! array to sort
  ! -- internal variables --
  integer(kip) :: i, j
  real(krp)    :: a
  ! -- BODY --
  do j = 2, N
    a = ARR(j)
    do i = j-1, 1, -1
      if (ARR(i) <= a) goto 10
      ARR(i+1) = ARR(i)
    end do
    i=0
10  ARR(i+1)=a
  end do
END SUBROUTINE sort_insertion_real

!------------------------------------------------------------------------------!
! Procedure : sort_insertion_integer
!   Straight Insertion Method 
!   (N^2 method should only be used for relatively small arrays (N<100))
!------------------------------------------------------------------------------!
SUBROUTINE sort_insertion_integer(ARR, n)
  implicit none
  ! -- inputs --
  integer(kip) :: n        ! size of array to sort
  integer(kip) :: arr(N)   ! array to sort
  ! -- internal variables --
  integer(kip) :: i, j
  integer(kip) :: a
  ! -- BODY --
  do j = 2, N
    a = ARR(j)
    do i = j-1, 1, -1
      if (ARR(i) <= a) goto 10
      ARR(i+1) = ARR(i)
    end do
    i=0
10  ARR(i+1)=a
  end do
END SUBROUTINE sort_insertion_integer


!------------------------------------------------------------------------------!
! Procedure : sort_shell_real
!   SHELL Method (N^3/2 method can be used for relatively large arrays)
!------------------------------------------------------------------------------!
SUBROUTINE sort_shell_real(ARR, n)
  implicit none
  ! -- inputs --
  integer(kip) :: n        ! size of array to sort
  real(krp)    :: arr(N)   ! array to sort
  ! -- internal variables --
  integer(kip) :: m, k, i, j, lognb2, nn, l
  real(krp)    :: t
  real(krp), parameter :: ALN2I = 1./0.69314718
  real(krp), parameter :: RTINY = 1.E-5
  ! -- BODY --
  LOGNB2 = INT(ALOG(FLOAT(N))*ALN2I+RTINY)
  m      = n
  do nn = 1, LOGNB2
    m = m/2; k = n-m
    do j = 1, k
      i = j
10    continue
      l = i+m
      if (ARR(l) < ARR(i)) then
        t = ARR(i)
        ARR(i) = ARR(l)
        ARR(l) = t
        i = i-m
        if (i >= 1) GOTO 10
      end if
    end do
  end do
END SUBROUTINE sort_shell_real


!------------------------------------------------------------------------------!
! Procedure : sort_heap_real
!   HEAPSORT method (N Log2 N method can be used for very large arrays)
!------------------------------------------------------------------------------!
SUBROUTINE sort_heap_real(RA, n)
  implicit none
  ! -- inputs --
  integer(kip) :: n        ! size of array to sort
  real(krp)    :: RA(N)    ! array to sort
  ! -- internal variables --
  integer(kip) :: l, ir, i, j
  real(krp)    :: rra
  ! -- BODY --
  L  = N/2+1
  IR = N
  !The index L will be decremented from its initial value during the
  !"hiring" (heap creation) phase. Once it reaches 1, the index IR 
  !will be decremented from its initial value down to 1 during the
  !"retirement-and-promotion" (heap selection) phase.
  do while (.true.)
    if (L > 1) then
      L   = L-1
      RRA = RA(L)
    else
      RRA    = RA(IR)
      RA(IR) = RA(1)
      IR     = IR-1
      if (IR == 1) then
        RA(1) = RRA
        exit
      endif
    endif
    I = L
    J = L+L
    do while (J <= IR)
      if (J < IR) then
        if (RA(J) < RA(J+1)) J = J+1
      endif
      if (RRA < RA(J)) then
        RA(I) = RA(J)
        I=J; J=J+J
      else
        J = IR+1
      endif
    enddo
    RA(I) = RRA
  enddo

endsubroutine sort_heap_real


!------------------------------------------------------------------------------!
! Procedure : sort_heap_integer
!   HEAPSORT method (N Log2 N method can be used for very large arrays)
!------------------------------------------------------------------------------!
SUBROUTINE sort_heap_integer(RA, n)
  implicit none
  ! -- inputs --
  integer(kip) :: n        ! size of array to sort
  integer(kip) :: RA(N)    ! array to sort
  ! -- internal variables --
  integer(kip) :: l, ir, i, j
  integer(kip) :: rra
  ! -- BODY --
  L  = N/2+1
  IR = N
  !The index L will be decremented from its initial value during the
  !"hiring" (heap creation) phase. Once it reaches 1, the index IR 
  !will be decremented from its initial value down to 1 during the
  !"retirement-and-promotion" (heap selection) phase.
  do while (.true.)
    if (L > 1) then
      L   = L-1
      RRA = RA(L)
    else
      RRA    = RA(IR)
      RA(IR) = RA(1)
      IR     = IR-1
      if (IR == 1) then
        RA(1) = RRA
        exit
      endif
    endif
    I = L
    J = L+L
    do while (J <= IR)
      if (J < IR) then
        if (RA(J) < RA(J+1)) J = J+1
      endif
      if (RRA < RA(J)) then
        RA(I) = RA(J)
        I=J; J=J+J
      else
        J = IR+1
      endif
    enddo
    RA(I) = RRA
  enddo

endsubroutine sort_heap_integer



endmodule LIBSORT

!------------------------------------------------------------------------------!
! Changes history
!
! Oct  2005: Created
! May  2013: moved to CFDTOOLS
!------------------------------------------------------------------------------!



