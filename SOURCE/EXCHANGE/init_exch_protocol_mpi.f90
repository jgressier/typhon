!------------------------------------------------------------------------------!
! Procedure : init_exch_protocol                  Auteur : J. Gressier
!                                                 Date   : July 2004
! Fonction                                        Modif  : see history
!   Initialization of exchange protocol
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

subroutine init_exch_protocol(winfo)

use TYPHMAKE
use OUTPUT
use MODINFO

implicit none

include 'mpif.h'

! -- Declaration des entrées/sorties --
type(st_info) :: winfo

! -- Declaration des sorties --

! -- Declaration des variables internes --
integer :: ierr

! -- Debut de la procedure --


call print_info(5,"initialization MPI exchanges")

call MPI_Init(ierr)
call MPI_Comm_rank(MPI_COMM_WORLD, winfo%my_id,  ierr)
call MPI_Comm_size(MPI_COMM_WORLD, winfo%nbproc, ierr)

print*,'I am ',winfo%my_id,'among',winfo%nbproc

endsubroutine init_exch_protocol

!------------------------------------------------------------------------------!
! Changes history
!
! july 2004 : création de la procédure
!------------------------------------------------------------------------------!
