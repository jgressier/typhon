!------------------------------------------------------------------------------!
! MODULE : MENU_GEN                       Auteur : J. Gressier
!                                         Date   : Novembre 2002
! Fonction                                Modif  : Juin 2003
!   Définition des structures pour les entrées du programme TYPHON
!   Structures pour les options générales
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

module MENU_GEN

use TYPHMAKE   ! Definition de la precision

implicit none

! -- Variables globales du module -------------------------------------------


! -- DECLARATIONS -----------------------------------------------------------


!------------------------------------------------------------------------------!
! structure MNU_PROJECT : Paramètres du projet
!------------------------------------------------------------------------------!
type mnu_project
  integer         :: nzone      ! nombre de zones
  integer         :: ncoupling  ! nombre de couplages entre zones
  character       :: typ_coord  ! type de repère
  character       :: typ_temps  ! (S)tationnaire, (I)nstationnaire, (P)ériodique
  real(krp)       :: duree      ! durée de l'intégration ou de la période
  real(krp)       :: tpsbase    ! pas de temps de base pour le couplage
  integer         :: ncycle     ! nombre de cycle (en stationnaire ou périodique)
  real(krp)       :: residumax  ! valeur maximale du résidu admise (stationnaire)
  real(krp)       :: dtbase     ! pas de temps de base d'un cycle
endtype mnu_project


!------------------------------------------------------------------------------!
! structure MNU_OUTPUT : Paramètres du projet
!------------------------------------------------------------------------------!
type mnu_OUTPUT
  character       :: format     ! format de la sortie
  character(len=strlen) &
                  :: fichier    ! nom du fichier de sortie
  character       :: type       ! type de sortie (cf. VARCOM)
endtype mnu_OUTPUT

! -- INTERFACES -------------------------------------------------------------


! -- Fonctions et Operateurs ------------------------------------------------


! -- IMPLEMENTATION ---------------------------------------------------------
!contains


endmodule MENU_GEN

