!------------------------------------------------------------------------------!
! MODULE : MENU_INIT                      Auteur : J. Gressier
!                                         Date   : Mars 2003
! Fonction                                Modif  : (cf Historique)
!   Definition des structures pour les entrees du programme TYPHON
!   Structures pour la definition de l'initialisation des champs
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
module MENU_INIT

use STRING
use TYPHMAKE    ! Definition de la precision
use VARCOM      ! Definition des constantes
use MENU_NS    ! Definition des solveurs type NS
use MENU_KDIF   ! Definition des solveurs type Equation de diffusion
use MENU_VORTEX ! Definition des solveurs type Equation de diffusion

implicit none

! -- Variables globales du module -------------------------------------------

! -- Definition des entiers caracteristiques pour l'uniformite de la CI --

integer(kpp), parameter :: init_unif    = 10   
integer(kpp), parameter :: init_nonunif = 20

! -- Way of definition of initialization --

integer(kpp), parameter :: init_def    = 100   
integer(kpp), parameter :: init_cgns   = 101
integer(kpp), parameter :: init_typhon = 105
integer(kpp), parameter :: init_file   = 110
integer(kpp), parameter :: init_udf    = 120

! -- DECLARATIONS -----------------------------------------------------------

!------------------------------------------------------------------------------!
! structure MNU_INIT : options numeriques les solveurs 
!------------------------------------------------------------------------------!
type mnu_init
  type(st_init_ns)    :: ns       ! condition aux limites propre au solveur NS
  type(st_init_kdif)  :: kdif     ! condition aux limites propre au solveur KDIF
  type(st_init_vort)  :: vortex   ! condition aux limites propre au solveur VORTEX
  integer(kpp)        :: unif     ! uniformite de la condition initiale
  integer(kpp)        :: type     ! kind of definition of initial condition
  character (len=longname) &
                      :: file     ! file name for definition with a file
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
! mars 2003 : creation du module
! fev  2004 : ajout des donnees d'initialisation pour solveur VORTEX
! juil 2004 : donnees d'initialistion pour solveur NS
!
! Ameliorations futures : 
!   - definitions de zones geometrique pour initialisation
!------------------------------------------------------------------------------!


