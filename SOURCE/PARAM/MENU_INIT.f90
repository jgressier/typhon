!------------------------------------------------------------------------------!
! MODULE : MENU_INIT                      Auteur : J. Gressier
!                                         Date   : Mars 2003
! Fonction                                Modif  : (cf Historique)
!   Définition des structures pour les entrées du programme TYPHON
!   Structures pour la définition de l'initialisation des champs
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
module MENU_INIT

use STRING
use TYPHMAKE    ! Definition de la precision
use VARCOM      ! Définition des constantes
use MENU_NS    ! Définition des solveurs type NS
use MENU_KDIF   ! Définition des solveurs type Equation de diffusion
use MENU_VORTEX ! Définition des solveurs type Equation de diffusion

implicit none

! -- Variables globales du module -------------------------------------------

! -- Définition des entiers caractéristiques pour l'uniformité de la CI --
integer, parameter :: init_unif    = 10   
integer, parameter :: init_nonunif = 20 

! -- DECLARATIONS -----------------------------------------------------------

!------------------------------------------------------------------------------!
! structure MNU_INIT : options numériques les solveurs 
!------------------------------------------------------------------------------!
type mnu_init
  type(st_init_ns)    :: ns       ! condition aux limites propre au solveur NS
  type(st_init_kdif)  :: kdif     ! condition aux limites propre au solveur KDIF
  type(st_init_vort)  :: vortex   ! condition aux limites propre au solveur VORTEX
  integer             :: unif     ! uniformité de la condition initiale
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
! mars 2003 : création du module
! fev  2004 : ajout des données d'initialisation pour solveur VORTEX
! juil 2004 : données d'initialistion pour solveur NS
!
! Améliorations futures : 
!   - définitions de zones géométrique pour initialisation
!------------------------------------------------------------------------------!


