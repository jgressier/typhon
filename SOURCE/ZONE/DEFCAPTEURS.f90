!------------------------------------------------------------------------------!
! MODULE : DEFCAPTEURS                    Auteur : J. Gressier
!                                         Date   : Juillet 2003
! Fonction                                Modif  : (cf historique)
!   Définition des structures de données pour capteurs dans les zones
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
! Définition de la structure ST_CAPTEUR : capteur maillage général et champ
!------------------------------------------------------------------------------!

type st_capteur
  integer      :: idef            ! index de définition de capteur (mnu_capteur)
  integer      :: iunit           ! numéro d'unité pour la sauvegarde
  integer      :: dim             ! taille du champ à sauvegarder
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
! Procédure : desallocation d'une structure CAPTEUR
!------------------------------------------------------------------------------!
subroutine delete_capteur(capteur)
implicit none
type(st_capteur)  :: capteur
integer           :: i     

  deallocate(capteur%tab)

endsubroutine delete_capteur


!------------------------------------------------------------------------------!
! Function : numéro d'unité pour sauvegarde
!------------------------------------------------------------------------------!
integer function capteur_get_unit(iz, ic)
implicit none
integer :: iz, ic   ! numéro de zone et de capteur

  capteur_get_unit = iz*100 + 20 + ic

endfunction capteur_get_unit




endmodule DEFCAPTEURS

!------------------------------------------------------------------------------!
! Historique des modifications
!
! juil 2003 : création du module
! nov  2003 : création de numéro d'unité
!------------------------------------------------------------------------------!
