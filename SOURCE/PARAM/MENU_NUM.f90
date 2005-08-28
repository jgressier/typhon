!------------------------------------------------------------------------------!
! MODULE : MENU_NUM                                 Authors : J. Gressier
!                                                   Created : May 2002
! Fonction
!   Definition des structures pour les entrees du programme TYPHON
!   Structures pour les options numeriques
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

module MENU_NUM

use TYPHMAKE   ! Definition de la precision

implicit none

! -- Variables globales du module -------------------------------------------

! -- Constantes pour le choix du parametre "temps"
!character, parameter :: stationnaire   = 'S'
!character, parameter :: instationnaire = 'I'
!character, parameter :: periodique     = 'P'

! -- Constantes pour le calcul du pas de temps
integer(kpp), parameter :: given_dt  = 1
integer(kpp), parameter :: stab_cond = 2

! -- Constantes pour la methode d'integration temporelle
integer(kpp), parameter :: tps_expl  = 10   ! integration explicite basique
integer(kpp), parameter :: tps_impl  = 20   ! integration implicite linearise (theta schema)
integer(kpp), parameter :: tps_dualt = 25   ! integration implicite / convergence locale
integer(kpp), parameter :: tps_rk    = 30   ! integration en Runge Kutta explicite

! -- Constantes pour schema de calcul des flux hyperboliques (sch_hyp)
integer(kpp), parameter :: sch_roe      = 10
integer(kpp), parameter :: sch_osher_no = 15
integer(kpp), parameter :: sch_osher_io = 16
integer(kpp), parameter :: sch_hllr     = 20
integer(kpp), parameter :: sch_hlle     = 21
integer(kpp), parameter :: sch_hllk     = 22
integer(kpp), parameter :: sch_hllc     = 25
integer(kpp), parameter :: sch_hllck    = 26
integer(kpp), parameter :: sch_stegwarm = 30
integer(kpp), parameter :: sch_vanleer  = 31
integer(kpp), parameter :: sch_vanleerh = 32
integer(kpp), parameter :: sch_efm      = 40
integer(kpp), parameter :: sch_efmo     = 50
integer(kpp), parameter :: sch_ausmm    = 50

! -- Constants for jacobian expression of flux (jac_hyp) --
integer(kpp), parameter :: jac_hll     = 10
integer(kpp), parameter :: jac_hlldiag = 15
integer(kpp), parameter :: jac_efm     = 20


! -- Constantes pour schema de calcul HIGH RESOLUTION
character, parameter :: hres_none       = 'N'
character, parameter :: hres_muscl      = 'M'
character, parameter :: hres_eno        = 'E'
character, parameter :: hres_weno       = 'W'
character, parameter :: hres_spect      = 'S'

! -- Constants for limiters
character, parameter :: lim_none      = 'X'
character, parameter :: lim_minmod    = 'M'
character, parameter :: lim_albada    = 'A'
character, parameter :: lim_vleer     = 'V'
character, parameter :: lim_sbee      = 'S'

! -- Constantes pour schema de calcul des flux dissipatifs (sch_dis)
integer(kpp), parameter :: dis_dif2 = 1     ! difference des 2 etats/face (NON CONSISTANT)
integer(kpp), parameter :: dis_avg2 = 5     ! moyenne des 2 gradients/face
integer(kpp), parameter :: dis_full = 10    ! evaluation complete (ponderee de 1 et 5)

! -- Constantes pour le calcul des gradients (gradmeth)
integer(kpp), parameter :: lsm1 = 10     ! moindres carres basee sur les centres voisins

! -- Constantes pour la methode de resolution matricielle
integer(kpp), parameter :: alg_lu       = 10  ! resolution directe LU
integer(kpp), parameter :: alg_cho      = 15  ! resolution directe (decomposition Choleski) (SYM)
integer(kpp), parameter :: alg_jac      = 20  ! resolution iterative Jacobi
integer(kpp), parameter :: alg_gs       = 25  ! resolution iterative Gauss-Seidel
integer(kpp), parameter :: alg_sor      = 26  ! resolution iterative Gauss-Seidel avec OverRelaxation
integer(kpp), parameter :: alg_gmres    = 40  ! resol. par proj. : GMRES
integer(kpp), parameter :: alg_bicg     = 60  ! Bi-Conjugate Gradient 
integer(kpp), parameter :: alg_bicgpjac = 61  ! Bi-Conjugate Gradient (Jacobi Preconditioned)
integer(kpp), parameter :: alg_cgs      = 62  ! Conjugate Gradient Squared
integer(kpp), parameter :: alg_bicgstab = 70  ! Bi-Conjugate Gradient Stabilized


! -- DECLARATIONS -----------------------------------------------------------


!------------------------------------------------------------------------------!
! structure MNU_RK : options numeriques pour la methode Runge Kutta
!------------------------------------------------------------------------------!
type mnu_rk
  integer(kpp)    :: order        ! ordre d'integration temporelle Runge-Kutta
endtype mnu_rk

!------------------------------------------------------------------------------!
! structure MNU_IMP : options numeriques pour l'implicitation
!------------------------------------------------------------------------------!
type mnu_imp
  integer(kpp)    :: methode      ! inversion method
  integer(kpp)    :: storage      ! storage method for sparse matrix (see SPARSE_MAT)
  integer(kpp)    :: max_it       ! maximal number of iterations
  real(krp)       :: ponderation  ! ponderation implicite/explicite
  real(krp)       :: maxres       ! max residual for convergence (if iterating method)
  real(krp)       :: overrelax    ! overrrelaxation parameter (SOR)
endtype mnu_imp

!------------------------------------------------------------------------------!
! structure MNU_TIME : options numeriques pour l'integration temporelle
!------------------------------------------------------------------------------!
type mnu_time
  integer(kpp)    :: temps      ! (S)tationnaire, (I)nstationnaire, (P)eriodique
  integer(kpp)    :: tps_meth   ! methode d'integration temporelle
  logical         :: local_dt   ! methode de calcul du pas de temps (global/local)
  integer(kpp)    :: stab_meth  ! methode de calcul de la stabilite
  real(krp)       :: dt         ! constant time step (if selected)
  real(krp)       :: stabnb, stabnb_max ! Stability number (CFL/Fourier) and max
  real(krp)       :: maxres     ! residu maximal pour convergence de la zone
  type(mnu_rk)    :: rk         ! parametres de la methode Runge Kutta
  type(mnu_imp)   :: implicite  ! parametres pour la methode d'implicitation
endtype mnu_time

!------------------------------------------------------------------------------!
! structure MNU_MUSCL : options numeriques pour la methode MUSCL
!------------------------------------------------------------------------------!
type mnu_muscl
  integer(kpp)   :: sch_grad      ! type de schema pour les gradients
  real(krp)      :: precision     ! parametre de precision
  real(krp)      :: compression   ! parametre de compression
  character      :: limiter       ! limiteur (X) aucun, (M)inmod, (V)an Leer
                                  !          (A) Van Albada, (S)uperbee
endtype mnu_muscl

!------------------------------------------------------------------------------!
! structure MNU_SPAT : options numeriques pour l'integration spatiale
!------------------------------------------------------------------------------!
type mnu_spat
  integer(kpp)    :: order        ! ordre d'integration spatiale
  integer(kpp)    :: sch_hyp      ! type de schema pour les flux hyperboliques
  integer(kpp)    :: jac_hyp      ! type of jacobian for hyperbolic fluxes
  integer(kpp)    :: sch_dis      ! type de schema pour les flux dissipatifs
  character       :: method       ! methode d'ordre eleve (M)USCL, (E)NO
  integer(kpp)    :: gradmeth     ! methode de calcul des gradients
  logical         :: calc_grad    ! necessite le calcul des gradients
  type(mnu_muscl) :: muscl        ! parametres de la methode MUSCL
endtype mnu_spat


! -- INTERFACES -------------------------------------------------------------


! -- Fonctions et Operateurs ------------------------------------------------


! -- IMPLEMENTATION ---------------------------------------------------------
!contains


endmodule MENU_NUM


!------------------------------------------------------------------------------!
! Historique des modifications
!
! mai  2002 : creation du module
! aout 2003 : parametres pour l'integration temporelle (Fourier, residu)
! sept 2003 : parametres pour l'integration spatiale (calcul de gradients)
!------------------------------------------------------------------------------!
