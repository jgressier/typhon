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

! -- Declaration des entrées/sorties --
type(st_info) :: winfo

! -- Declaration des sorties --

! -- Declaration des variables internes --

! -- Debut de la procedure --


call print_info(5,"initialization sequential exchanges")

winfo%nbproc = 1
winfo%my_id  = 1


endsubroutine init_exch_protocol

!------------------------------------------------------------------------------!
! Changes history
!
! july 2004 : création de la procédure
!------------------------------------------------------------------------------!
