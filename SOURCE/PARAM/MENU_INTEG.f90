!------------------------------------------------------------------------------!
! MODULE : MENU_INTEG                     Auteur : J. Gressier
!                                         Date   : Novembre 2002
! Fonction                                Modif  : 
!   Definition des structures pour les entrees du programme TYPHON
!   Structures pour les parametres de l'integration temporelle entre zones
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
! structure MNU_TEMP : options numeriques pour l'integration temporelle
!------------------------------------------------------------------------------!
type mnu_integ
  character       :: temps      ! (S)tationnaire, (I)nstationnaire, (P)eriodique
endtype mnu_integ



! -- INTERFACES -------------------------------------------------------------


! -- Fonctions et Operateurs ------------------------------------------------


! -- IMPLEMENTATION ---------------------------------------------------------
!contains


endmodule MENU_INTEG




