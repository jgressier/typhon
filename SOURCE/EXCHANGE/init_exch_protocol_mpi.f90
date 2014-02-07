!------------------------------------------------------------------------------!
! Procedure : init_exch_protocol                  Authors : J. Gressier
!                                                 Created : July 2004
! Fonction  
!   Initialization of exchange protocol
!
!------------------------------------------------------------------------------!

subroutine init_exch_protocol(winfo)

use TYPHMAKE
use OUTPUT
use MODINFO
use VARCOM

implicit none

#ifdef MPICOMPIL
include 'mpif.h'
#endif /*MPICOMPIL*/

! -- Declaration des entrees/sorties --
type(st_info) :: winfo

! -- Declaration des sorties --

! -- Declaration des variables internes --
integer :: ierr

! -- Debut de la procedure --

#ifdef MPICOMPIL
call print_info(5,"initialization MPI exchanges")

call MPI_Init(ierr)
call MPI_Comm_rank(MPI_COMM_WORLD, myprocid,     ierr)
call MPI_Comm_size(MPI_COMM_WORLD, winfo%nbproc, ierr)

myprocid = myprocid+1    ! mpi_comm_rank starts from 0
mpi_run  = .true.

print*,'I am ',myprocid,'among',winfo%nbproc,' procs'

tympi_real = MPI_REAL8
tympi_int  = MPI_INTEGER4
#else  /*NO MPICOMPIL*/
call print_info(5,"initialization sequential exchanges")

winfo%nbproc = 1
myprocid     = 0

mpi_run = .false.
#endif /*MPICOMPIL*/

endsubroutine init_exch_protocol

!------------------------------------------------------------------------------!
! Changes history
!
! july 2004 : creation de la procedure
!------------------------------------------------------------------------------!
