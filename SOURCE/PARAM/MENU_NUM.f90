!------------------------------------------------------------------------------!
! MODULE : MENU_NUM                       Auteur : J. Gressier
!                                         Date   : Mai 2002
! Fonction                                Modif  : Novembre 2002
!   Définition des structures pour les entrées du programme TYPHON
!   Structures pour les options numériques
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

module MENU_NUM

use TYPHMAKE   ! Definition de la precision

implicit none

! -- Variables globales du module -------------------------------------------

! -- Constantes pour le choix du paramètre "temps"
!character, parameter :: stationnaire   = 'S'
!character, parameter :: instationnaire = 'I'
!character, parameter :: periodique     = 'P'

! -- Constantes pour le calcul du pas de temps
character, parameter :: stab_cond = 'S'
character, parameter :: given_dt  = 'T'


! -- DECLARATIONS -----------------------------------------------------------


!------------------------------------------------------------------------------!
! structure MNU_RK : options numériques pour la méthode Runge Kutta
!------------------------------------------------------------------------------!
type mnu_rk
  integer         :: ordre        ! ordre d'intégration temporelle Runge-Kutta
endtype mnu_rk

!------------------------------------------------------------------------------!
! structure MNU_IMP : options numériques pour l'implicitation
!------------------------------------------------------------------------------!
type mnu_imp
  character       :: methode      ! (A)DI
  real(krp)       :: ponderation  ! ponderation implicite/explicite
  integer         :: max_it       ! nombre d'itération maximal
endtype mnu_imp

!------------------------------------------------------------------------------!
! structure MNU_TIME : options numériques pour l'intégration temporelle
!------------------------------------------------------------------------------!
type mnu_time
  character       :: temps      ! (S)tationnaire, (I)nstationnaire, (P)ériodique
  character       :: methode    ! méthode d'intégration temporelle
  logical         :: local_dt   ! methode de calcul du pas de temps (global/local)
  character       :: stab_meth  ! methode de calcul de la stabilité
  real(krp)       :: dt, stabnb ! pas de temps fixe ou nombre de stabilité associé
                                !                      (CFL/Fourier)
  type(mnu_rk)    :: rk         ! paramètres de la méthode Runge Kutta
  type(mnu_imp)   :: implicite  ! paramètres pour la méthode d'implicitation
endtype mnu_time

!------------------------------------------------------------------------------!
! structure MNU_MUSCL : options numériques pour la méthode MUSCL
!------------------------------------------------------------------------------!
type mnu_muscl
  real(krp)      :: precision     ! paramètre de précision
  real(krp)      :: compression   ! paramètre de compression
  character      :: limiteur      ! limiteur (X) aucun, (M)inmod, (V)an Leer
                                  !          (A) Van Albada, (S)uperbee
endtype mnu_muscl

!------------------------------------------------------------------------------!
! structure MNU_SPAT : options numériques pour l'intégration spatiale
!------------------------------------------------------------------------------!
type mnu_spat
  integer         :: ordre        ! ordre d'intégration spatiale
  character       :: methode      ! méthode d'ordre élevé (M)USCL, (E)NO
  type(mnu_muscl) :: muscl        ! paramètres de la méthode MUSCL
endtype mnu_spat


! -- INTERFACES -------------------------------------------------------------


! -- Fonctions et Operateurs ------------------------------------------------


! -- IMPLEMENTATION ---------------------------------------------------------
!contains


endmodule MENU_NUM

