!------------------------------------------------------------------------------!
! MODULE : MENU_CAPTEURS                  Auteur : J. Gressier
!                                         Date   : Juillet 2003
! Fonction                                Modif  : (cf Historique)
!   Définition des structures pour les entrées du programme TYPHON
!   Structures pour la définition des capteurs
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
module MENU_CAPTEURS

use TYPHMAKE   ! Definition de la precision
use GEO3D      ! Définition des vecteurs 3D

implicit none

! -- Variables globales du module -------------------------------------------

! -- type de capteur --
character, parameter :: probe         = 'P'    ! calcul ponctuel d'une quantité
character, parameter :: boco_field    = 'F'    ! restitution d'un champ sur boco
character, parameter :: boco_integral = 'I'    ! intégrale   d'un champ sur boco
character, parameter :: residuals     = 'R'    ! calcul de résidu moyen

! -- type de stockage --
character, parameter :: phys_tstep = 'T'       ! à chaque cycle
character, parameter :: ziter      = 'Z'       ! pour chaque itération interne de zone


! -- DECLARATIONS -----------------------------------------------------------

!------------------------------------------------------------------------------!
! structure MNU_CAPTEUR : options numériques les capteurs
!------------------------------------------------------------------------------!
type mnu_capteur
  character             :: type        ! type de capteur
  character             :: store       ! type de stockage
  character(len=strlen) :: family      ! famille associée (si nécessaire)
                                       !   DEV: on peut extrapoler à plusieurs familles
  integer               :: quantity    ! quantité à calculer (selon solveur)
  type(v3d)             :: center, dir ! vecteurs centre et direction (si nécessaire)
endtype mnu_capteur


! -- INTERFACES -------------------------------------------------------------

interface delete
  module procedure delete_mnu_capteur
endinterface

! -- Fonctions et Operateurs ------------------------------------------------


! -- IMPLEMENTATION ---------------------------------------------------------
contains

!------------------------------------------------------------------------------!
! Procédure : desallocation d'une structure MNU_SOLVER
!------------------------------------------------------------------------------!
subroutine delete_mnu_capteur(defcapteur)
implicit none
type(mnu_capteur)  :: defcapteur

  print*,'!! DEBUG destruction de structure "paramètres" à compléter'

endsubroutine delete_mnu_capteur



endmodule MENU_CAPTEURS


!------------------------------------------------------------------------------!
! Historique des modifications
!
! juil 2003 : création du module
!
! améliorations futures : capteurs sur plusieurs familles simultanément
!------------------------------------------------------------------------------!




