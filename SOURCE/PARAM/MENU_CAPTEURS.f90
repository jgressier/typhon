!------------------------------------------------------------------------------!
! MODULE : MENU_CAPTEURS                  Auteur : J. Gressier
!                                         Date   : Juillet 2003
! Fonction                                Modif  : (cf Historique)
!   Definition des structures pour les entrees du programme TYPHON
!   Structures pour la definition des capteurs
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
module MENU_CAPTEURS

use TYPHMAKE   ! Definition de la precision
use GEO3D      ! Definition des vecteurs 3D

implicit none

! -- Variables globales du module -------------------------------------------

! -- type de capteur --
character, parameter :: probe         = 'P'    ! calcul ponctuel d'une quantite
character, parameter :: boco_field    = 'F'    ! restitution d'un champ sur boco
character, parameter :: boco_integral = 'I'    ! integrale   d'un champ sur boco
character, parameter :: residuals     = 'R'    ! calcul de residu moyen

! -- type de stockage --
character, parameter :: no_store   = 'X'       ! stockage momentanne de l'iteration uniquement
character, parameter :: prb_cycle  = 'C'       ! a chaque cycle
character, parameter :: prb_iter   = 'I'       ! pour chaque iteration interne de zone


! -- DECLARATIONS -----------------------------------------------------------

!------------------------------------------------------------------------------!
! structure MNU_CAPTEUR : options numeriques les capteurs
!------------------------------------------------------------------------------!
type mnu_capteur
  character             :: type        ! type de capteur
  character             :: store       ! type de stockage
  logical               :: write       ! ecriture des donnees
  character(len=strlen) :: name        ! 
  character(len=strlen) :: boco_name   ! famille associee (si necessaire)
                                       !   DEV: on peut extrapoler a plusieurs familles
                                       !        ou proposer la fusion de condition limite dans MESH
  integer               :: boco_index  ! index de condition limite
  integer               :: quantity    ! quantite a calculer (selon solveur)
  type(v3d)             :: center, dir ! vecteurs centre et direction (si necessaire)
endtype mnu_capteur


! -- INTERFACES -------------------------------------------------------------

interface delete
  module procedure delete_mnu_capteur
endinterface

! -- Fonctions et Operateurs ------------------------------------------------


! -- IMPLEMENTATION ---------------------------------------------------------
contains

!------------------------------------------------------------------------------!
! Procedure : desallocation d'une structure MNU_CAPTEUR
!------------------------------------------------------------------------------!
subroutine delete_mnu_capteur(defcapteur)
implicit none
type(mnu_capteur)  :: defcapteur

  !print*,'!! DEBUG destruction de structure "parametres" a completer'

endsubroutine delete_mnu_capteur



endmodule MENU_CAPTEURS


!------------------------------------------------------------------------------!
! Historique des modifications
!
! juil 2003 : creation du module
!
! ameliorations futures : capteurs sur plusieurs familles simultanement
!------------------------------------------------------------------------------!




