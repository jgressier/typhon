!------------------------------------------------------------------------------!
! MODULE : MENU_INTEG                     Auteur : J. Gressier
!                                         Date   : Novembre 2002
! Fonction                                Modif  : 
!   Définition des structures pour les entrées du programme TYPHON
!   Structures pour les paramètres de l'intégration temporelle entre zones
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

module MENU_INTEG

use TYPHMAKE   ! Definition de la precision

implicit none

! -- Variables globales du module -------------------------------------------


! -- DECLARATIONS -----------------------------------------------------------


!------------------------------------------------------------------------------!
! structure MNU_TEMP : options numériques pour l'intégration temporelle
!------------------------------------------------------------------------------!
type mnu_integ
  character       :: temps      ! (S)tationnaire, (I)nstationnaire, (P)ériodique
endtype mnu_integ



! -- INTERFACES -------------------------------------------------------------


! -- Fonctions et Operateurs ------------------------------------------------


! -- IMPLEMENTATION ---------------------------------------------------------
!contains


endmodule MENU_INTEG




