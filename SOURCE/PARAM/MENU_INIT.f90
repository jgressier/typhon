!------------------------------------------------------------------------------!
! MODULE : MENU_INIT                      Auteur : J. Gressier
!                                         Date   : Mars 2003
! Fonction                                Modif  : Mars 2003 (cf Historique)
!   Définition des structures pour les entrées du programme TYPHON
!   Structures pour la définition de l'initialisation des champs
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
module MENU_INIT

use STRING
use TYPHMAKE   ! Definition de la precision
use VARCOM     ! Définition des constantes
!use MENU_NS   ! Définition des solveurs type NS
use MENU_KDIF ! Définition des solveurs type Equation de diffusion

implicit none

! -- Variables globales du module -------------------------------------------

! -- Définition des entiers caractéristiques pour le type de solveur --


! -- DECLARATIONS -----------------------------------------------------------

!------------------------------------------------------------------------------!
! structure MNU_INIT : options numériques les solveurs 
!------------------------------------------------------------------------------!
type mnu_init
  type(st_init_kdif)    :: kdif     ! condition aux limites propre au solveur KDIF
endtype mnu_init


! -- INTERFACES -------------------------------------------------------------


! -- Fonctions et Operateurs ------------------------------------------------

!integer inittype, bctype_of_init

! -- IMPLEMENTATION ---------------------------------------------------------
!contains



endmodule MENU_INIT


!------------------------------------------------------------------------------!
! Historique des modifications
!
! mars 2003 (v0.0.1b): création du module
!
! Améliorations futures : 
!   - définitions de zones géométrique pour initialisation
!   - initilisation de champs non uniformes
!------------------------------------------------------------------------------!


