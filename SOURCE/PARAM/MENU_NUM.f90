!------------------------------------------------------------------------------!
! MODULE : MENU_NUM                       Auteur : J. Gressier
!                                         Date   : Mai 2002
! Fonction                                Modif  : (cf historique)
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
integer(kpp), parameter :: given_dt  = 1
integer(kpp), parameter :: stab_cond = 2

! -- Constantes pour la méthode d'intégration temporelle
integer(kpp), parameter :: tps_expl  = 10   ! intégration explicite basique
integer(kpp), parameter :: tps_impl  = 20   ! intégration implicite linéarisé (theta schéma)
integer(kpp), parameter :: tps_dualt = 25   ! intégration implicite / convergence locale
integer(kpp), parameter :: tps_rk    = 30   ! intégration en Runge Kutta explicite

! -- Constantes pour schéma de calcul des flux hyperboliques (sch_hyp)
integer(kpp), parameter :: roe      = 10
integer(kpp), parameter :: hlle     = 20
integer(kpp), parameter :: hllc     = 25
integer(kpp), parameter :: stegwar  = 30
integer(kpp), parameter :: vanleer  = 31
integer(kpp), parameter :: efm      = 40

! -- Constantes pour schéma de calcul des flux dissipatifs (sch_dis)
integer(kpp), parameter :: dis_dif2 = 1     ! différence des 2 états/face (NON CONSISTANT)
integer(kpp), parameter :: dis_avg2 = 5     ! moyenne des 2 gradients/face
integer(kpp), parameter :: dis_full = 10    ! évaluation complète (pondérée de 1 et 5)

! -- Constantes pour le calcul des gradients (gradmeth)
integer(kpp), parameter :: lsm1 = 10     ! moindres carrés basée sur les centres voisins

! -- Constantes pour la méthode de résolution matricielle
integer(kpp), parameter :: alg_lu    = 10  ! résolution directe LU
integer(kpp), parameter :: alg_cho   = 15  ! resolution directe (décomposition Choleski) (SYM)
integer(kpp), parameter :: alg_jac   = 20  ! resolution itérative Jacobi
integer(kpp), parameter :: alg_gs    = 25  ! resolution itérative Gauss-Seidel
integer(kpp), parameter :: alg_sor   = 26  ! resolution itérative Gauss-Seidel avec OverRelaxation
integer(kpp), parameter :: alg_gmres = 40  ! resol. par proj. : GMRES


! -- DECLARATIONS -----------------------------------------------------------


!------------------------------------------------------------------------------!
! structure MNU_RK : options numériques pour la méthode Runge Kutta
!------------------------------------------------------------------------------!
type mnu_rk
  integer(kpp)    :: ordre        ! ordre d'intégration temporelle Runge-Kutta
endtype mnu_rk

!------------------------------------------------------------------------------!
! structure MNU_IMP : options numériques pour l'implicitation
!------------------------------------------------------------------------------!
type mnu_imp
  integer(kpp)    :: methode      ! (A)DI
  integer(kpp)    :: max_it       ! nombre d'itération maximal
  real(krp)       :: ponderation  ! ponderation implicite/explicite
  real(krp)       :: maxres       ! residu maximal pour convergence de l'inversion
endtype mnu_imp

!------------------------------------------------------------------------------!
! structure MNU_TIME : options numériques pour l'intégration temporelle
!------------------------------------------------------------------------------!
type mnu_time
  integer(kpp)    :: temps      ! (S)tationnaire, (I)nstationnaire, (P)ériodique
  integer(kpp)    :: tps_meth   ! méthode d'intégration temporelle
  logical         :: local_dt   ! methode de calcul du pas de temps (global/local)
  integer(kpp)    :: stab_meth  ! methode de calcul de la stabilité
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
  integer(kpp)    :: ordre        ! ordre d'intégration spatiale
  integer(kpp)    :: sch_hyp      ! type de schéma pour les flux hyperboliques
  integer(kpp)    :: sch_dis      ! type de schéma pour les flux dissipatifs
  character       :: methode      ! méthode d'ordre élevé (M)USCL, (E)NO
  integer(kpp)    :: gradmeth     ! méthode de calcul des gradients
  logical         :: calc_grad    ! nécessite le calcul des gradients
  type(mnu_muscl) :: muscl        ! paramètres de la méthode MUSCL
endtype mnu_spat


! -- INTERFACES -------------------------------------------------------------


! -- Fonctions et Operateurs ------------------------------------------------


! -- IMPLEMENTATION ---------------------------------------------------------
!contains


endmodule MENU_NUM


!------------------------------------------------------------------------------!
! Historique des modifications
!
! mai  2002 : création du module
! aout 2003 : paramètres pour l'intégration temporelle (Fourier, résidu)
! sept 2003 : paramètres pour l'intégration spatiale (calcul de gradients)
!------------------------------------------------------------------------------!
