!------------------------------------------------------------------------------!
! MODULE : MENU_GEN                       Auteur : J. Gressier
!                                         Date   : Novembre 2002
! Fonction                                Modif  : cf historique
!   Definition des structures pour les entrees du programme TYPHON
!   Structures pour les options generales
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
! structure MNU_PROJECT : Parametres du projet
!------------------------------------------------------------------------------!
type mnu_project
  integer         :: nzone      ! nombre de zones
  integer         :: ncoupling  ! nombre de couplages entre zones
  character       :: typ_coord  ! type de repere
  character       :: typ_temps  ! (S)tationnaire, (I)nstationnaire, (P)eriodique
  real(krp)       :: duree      ! duree de l'integration ou de la periode
  real(krp)       :: tpsbase    ! pas de temps de base du couplage
  integer         :: ncycle     ! nombre de cycle (en stationnaire ou periodique)
  real(krp)       :: residumax  ! valeur maximale du residu admise (stationnaire)
  real(krp)       :: dtbase     ! pas de temps de base d'un cycle 
endtype mnu_project


!------------------------------------------------------------------------------!
! structure MNU_OUTPUT : Parametres du projet
!------------------------------------------------------------------------------!
type mnu_OUTPUT
  character       :: format     ! format de la sortie
  character(len=strlen) &
                  :: fichier    ! nom du fichier de sortie
  integer         :: type       ! type de sortie (cf. VARCOM)
endtype mnu_OUTPUT

! -- INTERFACES -------------------------------------------------------------


! -- Fonctions et Operateurs ------------------------------------------------


! -- IMPLEMENTATION ---------------------------------------------------------
!contains


endmodule MENU_GEN



!------------------------------------------------------------------------------!
! Historique des modifications
!
! nov  2002 : creation du module
!------------------------------------------------------------------------------!



