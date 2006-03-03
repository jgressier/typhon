!------------------------------------------------------------------------------!
! Procedure : def_mpi                     Auteur : J. Gressier
!                                         Date   : July 2004
! Fonction                                Modif  : (cf historique)
!   Traitement des parametres du fichier menu principal
!   Parametres MPI
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine def_mpi(block, isolver, defmpi)

use RPM
use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_MPI

implicit none

! -- Declaration des entrees --
type(rpmblock), target :: block
integer                :: isolver

! -- Declaration des sorties --
type(mnu_mpi) :: defmpi

! -- Declaration des variables internes --
type(rpmblock), pointer  :: pblock, pcour  ! pointeur de bloc RPM
integer                  :: nkey           ! nombre de clefs
integer                  :: i
character(len=dimrpmlig) :: str            ! chaine RPM intermediaire

! -- Debut de la procedure --

call print_info(5,"- Definition of MPI parameters and strategy")

! -- Initialisation --


! -- Recherche du BLOCK:MPI --

pblock => block
call seekrpmblock(pblock, "MPI", 0, pcour, nkey)

! DEV : est-ce que la presence du bloc est obligatoire ?
if (nkey > 1) call erreur("lecture de menu", &
                           "bloc MPI surnumeraire")

defmpi%exchange_grad = .false.

if (nkey == 1) then


endif


endsubroutine def_mpi
!------------------------------------------------------------------------------!
! Changes history
!
! Feb  2006 : creation, reading basic parameters
!------------------------------------------------------------------------------!
