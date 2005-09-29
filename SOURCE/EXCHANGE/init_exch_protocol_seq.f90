!------------------------------------------------------------------------------!
! Procedure : init_exch_protocol                  Auteur : J. Gressier
!                                                 Date   : July 2004
! Fonction
!   Initialization of exchange protocol
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

subroutine init_exch_protocol(winfo)

use TYPHMAKE
use OUTPUT
use MODINFO
use VARCOM

implicit none

! -- Declaration des entrees/sorties --
type(st_info) :: winfo

! -- Declaration des sorties --

! -- Declaration des variables internes --

! -- Debut de la procedure --


call print_info(5,"initialization sequential exchanges")

winfo%nbproc = 1
myprocid     = 0

mpi_run = .false.

endsubroutine init_exch_protocol

!------------------------------------------------------------------------------!
! Changes history
!
! july 2004 : creation de la procedure
!------------------------------------------------------------------------------!
