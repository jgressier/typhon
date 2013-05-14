!------------------------------------------------------------------------------!
! MODULE : DEFCAPTEURS                    Auteur : J. Gressier
!                                         Date   : Juillet 2003
! Fonction                                Modif  : (cf historique)
!   Definition des structures de donnees pour capteurs dans les zones
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

module DEFCAPTEURS

use TYPHMAKE      ! machine accuracy
use DEFFIELD      ! physical data & fields

implicit none

! -- Variables globales du module -------------------------------------------



! -- DECLARATIONS -----------------------------------------------------------

!------------------------------------------------------------------------------!
! Definition de la structure ST_CAPTEUR : capteur maillage general et champ
!------------------------------------------------------------------------------!

type st_capteur
  integer      :: idef            ! index de definition de capteur (mnu_capteur)
  integer      :: iunit           ! numero d'unite pour la sauvegarde
  integer      :: dim             ! taille du champ a sauvegarder
  integer      :: nbuf            ! taille du buffer
  real(krp), dimension(:,:), pointer &
               :: tab             ! tableau de valeurs (dim,nbuf)
endtype st_capteur


! -- INTERFACES -------------------------------------------------------------

interface delete
  module procedure delete_capteur
endinterface

! -- Fonctions et Operateurs ------------------------------------------------


! -- IMPLEMENTATION ---------------------------------------------------------
contains


!------------------------------------------------------------------------------!
! Procedure : desallocation d'une structure CAPTEUR
!------------------------------------------------------------------------------!
subroutine delete_capteur(capteur)
implicit none
type(st_capteur)  :: capteur

  deallocate(capteur%tab)

endsubroutine delete_capteur


!------------------------------------------------------------------------------!
! Function : numero d'unite pour sauvegarde
!------------------------------------------------------------------------------!
integer function capteur_get_unit(iz, ic)
implicit none
integer :: iz, ic   ! numero de zone et de capteur

  capteur_get_unit = iz*100 + 20 + ic

endfunction capteur_get_unit




endmodule DEFCAPTEURS

!------------------------------------------------------------------------------!
! Historique des modifications
!
! juil 2003 : creation du module
! nov  2003 : creation de numero d'unite
!------------------------------------------------------------------------------!
