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
use VARCOM
use RPM

implicit none

! -- Variables globales du module -------------------------------------------

! -- Constantes pour le choix du parametre "temps"
!character, parameter :: stationnaire   = 'S'
!character, parameter :: instationnaire = 'I'
!character, parameter :: periodique     = 'P'

! -- Constantes pour le calcul du pas de temps
integer(kpp), parameter :: given_dt      = 1
integer(kpp), parameter :: stab_cond     = 2
integer(kpp), parameter :: loc_stab_cond = 3

! -- Constantes pour la methode d'integration temporelle
integer(kpp), parameter :: tps_expl   = 10   ! genuine explicit
integer(kpp), parameter :: tps_impl   = 20   ! linearized implicit (theta scheme)
integer(kpp), parameter :: tps_dualt  = 25   ! dual time
integer(kpp), parameter :: tps_rk2    = 30   ! Runge Kutta explicit : predictor-corrector
integer(kpp), parameter :: tps_rk2ssp = 31   ! Runge Kutta explicit : Heun
integer(kpp), parameter :: tps_rk3ssp = 33   ! Runge Kutta explicit 
integer(kpp), parameter :: tps_rk4    = 35   ! Runge Kutta explicit 

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
character, parameter :: hres_musclfast  = 'F'
character, parameter :: hres_muscluns   = 'U'
character, parameter :: hres_eno        = 'E'
character, parameter :: hres_weno       = 'W'
character, parameter :: hres_svm        = 'V'
character, parameter :: hres_sdm        = 'S'

! -- Constants for limiters
character, parameter :: lim_none      = 'X'
character, parameter :: lim_minmod    = 'M'
character, parameter :: lim_albada    = 'A'
character, parameter :: lim_vleer     = 'V'
character, parameter :: lim_sbee      = 'S'
character, parameter :: lim_kim3      = 'K'

! -- Constants for post-limitation --
integer(kpp), parameter :: postlim_none       = 0
integer(kpp), parameter :: postlim_monotonic  = 10

! -- Constants for SVM splitting
integer(kpp), parameter :: svm_2tri    = 21
integer(kpp), parameter :: svm_2quad   = 22

! -- Constantes pour schema de calcul des flux dissipatifs (sch_dis)
integer(kpp), parameter :: dis_dif2 = 1     ! difference des 2 etats/face (NON CONSISTANT)
integer(kpp), parameter :: dis_avg2 = 5     ! moyenne des 2 gradients/face
integer(kpp), parameter :: dis_full = 10    ! evaluation complete (ponderee de 1 et 5)

! -- Constants for gradients construction method (gradmeth)
integer(kpp), parameter :: grad_lsq  = 10     ! least square method
integer(kpp), parameter :: grad_lsqw = 12     ! weighted least square method

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
  integer(kpp)       :: order        ! order of accuracy
  integer(kpp)       :: nstage       ! number of stages
  real(krp), pointer :: coef(:,:)    ! butcher array (1:nstage, 1:nstage)
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
  character       :: temps      ! (S)tationnaire, (I)nstationnaire, (P)eriodique
  integer(kpp)    :: tps_meth   ! methode d'integration temporelle
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
! structure MNU_SVM : Options for  Spectral Volume Method
!------------------------------------------------------------------------------!
type mnu_svm
  integer(kpp)      :: sv_meth              ! SV Method (cf constants)
  integer(kpp)      :: cv_split             ! number of Control Volume (CV subcell) in a SV
  integer(kpp)      :: svface_split         ! number of subface (CV face) by original face
  integer(kpp)      :: intnode              ! number of internal added nodes for cell splitting
  integer(kpp)      :: internal_faces       ! number of internal faces (by cell)
  integer(kpp)      :: nb_subfaces          ! number of sub faces      (by cell) (internal + original*split)
  integer(kpp)      :: nb_facepoints        ! number of integration points by face
  real(krp),pointer :: interp_weights(:,:)  ! weights for cell to face Gauss points interpolation  
                                    
endtype mnu_svm

!------------------------------------------------------------------------------!
! structure MNU_SPAT : Options for numerical spatial parameters
!------------------------------------------------------------------------------!
type mnu_spat
  !integer(kpp)    :: order        ! not used
  integer(kpp)    :: sch_hyp      ! type de schema pour les flux hyperboliques
  integer(kpp)    :: jac_hyp      ! type of jacobian for hyperbolic fluxes
  integer(kpp)    :: sch_dis      ! type de schema pour les flux dissipatifs
  character       :: method       ! methode d'ordre eleve (M)USCL, (E)NO ...
  integer(kpp)    :: gradmeth     ! methode de calcul des gradients
  integer(kpp)    :: postlimiter  ! Limitation method after reconstruction/extrapolation
  logical         :: calc_grad    ! necessite le calcul des gradients
  type(mnu_muscl) :: muscl        ! parametres de la methode MUSCL
  type(mnu_svm)   :: svm          ! parametres de la methode SVM
endtype mnu_spat


! -- INTERFACES -------------------------------------------------------------


! -- Fonctions et Operateurs ------------------------------------------------


! -- IMPLEMENTATION ---------------------------------------------------------
contains

!-------------------------------------------------------------------------
! get method for dissipative flux computation
!-------------------------------------------------------------------------
subroutine get_dissipmethod(pcour, keyword, defspat)
implicit none
type(rpmblock), pointer       :: pcour  ! pointeur de bloc RPM
character(len=*), intent(in)  :: keyword
type(mnu_spat)                :: defspat
character(len=dimrpmlig)      :: str            ! chaine RPM intermediaire

call rpmgetkeyvalstr(pcour, keyword, str, "FULL")
defspat%sch_dis = inull

if (samestring(str,"COMPACT")) defspat%sch_dis = dis_dif2
if (samestring(str,"AVERAGE")) defspat%sch_dis = dis_avg2
if (samestring(str,"FULL"))    defspat%sch_dis = dis_full

if (defspat%sch_dis == inull) &
     call erreur("parameters parsing","unknown DISSIPATIVE_FLUX method")

select case(defspat%sch_dis)
case(dis_dif2)
  
case(dis_avg2)
  defspat%calc_grad = .true.
case(dis_full)
  defspat%calc_grad = .true.
endselect

call get_gradientmethod(pcour, defspat)

endsubroutine get_dissipmethod


!-------------------------------------------------------------------------
! get method for gradient computation
!-------------------------------------------------------------------------
subroutine get_gradientmethod(pcour, defspat)
implicit none
type(rpmblock), pointer       :: pcour  ! pointeur de bloc RPM
type(mnu_spat)                :: defspat
character(len=dimrpmlig)      :: str            ! chaine RPM intermediaire

defspat%calc_grad = .true.

call rpmgetkeyvalstr(pcour, "GRADMETH", str, "LSQ")
defspat%gradmeth = inull

if (samestring(str,"LSQ"))   defspat%gradmeth = grad_lsq
if (samestring(str,"W-LSQ")) defspat%gradmeth = grad_lsqw

if (defspat%gradmeth == inull) &
     call erreur("parameters parsing","unknown GRADIENT computation method")

select case(defspat%gradmeth)
case(grad_lsq)
case(grad_lsqw)
endselect

endsubroutine get_gradientmethod

!-------------------------------------------------------------------------
! init Runge-Kutta parameters
!-------------------------------------------------------------------------
subroutine init_rungekutta(method, rk)
implicit none
! -- parameters --
integer(kpp) :: method
type(mnu_rk) :: rk
! -- internal --
! -- body --

select case(method)
case(tps_rk2)
  rk%order  = 2
  rk%nstage = 2
  allocate(rk%coef(1:rk%nstage, 1:rk%nstage))
  rk%coef(:, :)   = 0._krp
  rk%coef(1, 1)   = .5_krp
  rk%coef(2, 1:2) = (/ 0._krp, 1._krp /)
case(tps_rk2ssp)
  rk%order  = 2
  rk%nstage = 2
  allocate(rk%coef(1:rk%nstage, 1:rk%nstage))
  rk%coef(:, :)   = 0._krp
  rk%coef(1, 1)   = 1._krp
  rk%coef(2, 1:2) = (/ 0.5_krp, 0.5_krp /)
case(tps_rk3ssp)
  rk%order  = 3
  rk%nstage = 3
  allocate(rk%coef(1:rk%nstage, 1:rk%nstage))
  rk%coef(:, :)   = 0._krp
  rk%coef(1, 1)   = 1._krp
  rk%coef(2, 1:2) = (/ 0.25_krp, 0.25_krp /)
  rk%coef(3, 1:3) = (/   1._krp,   1._krp, 4._krp /) / 6._krp
case(tps_rk4)
  rk%order  = 4
  rk%nstage = 4
  allocate(rk%coef(1:rk%nstage, 1:rk%nstage))
  rk%coef(:, :)   = 0._krp
  rk%coef(1, 1)   = 0.5_krp
  rk%coef(2, 1:2) = (/ 0._krp, 0.5_krp /)
  rk%coef(3, 1:3) = (/ 0._krp,  0._krp, 1._krp /) 
  rk%coef(4, 1:4) = (/ 1._krp,  2._krp, 2._krp, 1._krp /) / 6._krp
case default
  call erreur("parameters parsing","unknown RUNGE KUTTA method")
endselect

endsubroutine init_rungekutta


!-------------------------------------------------------------------------
! init SVM parameters
!-------------------------------------------------------------------------
subroutine init_svmparam(svm)
implicit none
! -- parameters --
type(mnu_svm)    :: svm
! -- body --

select case(svm%sv_meth)
case(svm_2quad)
  svm%cv_split       = 3      ! nb of CV in SV
  svm%intnode        = 1      ! nb of internal added nodes for cell splitting
  svm%svface_split   = 2      ! nb of CV face per SV face
  svm%internal_faces = 3      ! number of internal faces (by cell)
  svm%nb_subfaces    = 3+2*3  ! total number of sub faces (by cell)
  svm%nb_facepoints  = 1      ! number of integration points by face
case default
  call erreur("parameters parsing","unknown SVM method (init_svmparam)")
endselect

endsubroutine init_svmparam

!-------------------------------------------------------------------------
! init SVM weights
!-------------------------------------------------------------------------
subroutine init_svmweights(defsvm)
implicit none
! -- parameters --
type(mnu_svm) :: defsvm
! -- internal --
real(krp)     :: k(1:5)
! -- body --

allocate(defsvm%interp_weights(1:defsvm%nb_subfaces*defsvm%nb_facepoints, &
                               1:defsvm%cv_split))

select case(defsvm%sv_meth)

case(svm_2quad)
  k(1) =  4._krp /  3._krp
  k(2) =  2._krp / 15._krp
  k(3) = -7._krp / 15._krp
  k(4) = -1._krp / 15._krp
  k(5) =  8._krp / 15._krp
  defsvm%interp_weights(1, 1:defsvm%cv_split) = (/ k(4), k(5), k(5) /)
  defsvm%interp_weights(2, 1:defsvm%cv_split) = (/ k(5), k(4), k(5) /)
  defsvm%interp_weights(3, 1:defsvm%cv_split) = (/ k(5), k(5), k(4) /)
  defsvm%interp_weights(4, 1:defsvm%cv_split) = (/ k(1), k(3), k(2) /)
  defsvm%interp_weights(5, 1:defsvm%cv_split) = (/ k(1), k(2), k(3) /)
  defsvm%interp_weights(6, 1:defsvm%cv_split) = (/ k(2), k(1), k(3) /)
  defsvm%interp_weights(7, 1:defsvm%cv_split) = (/ k(3), k(1), k(2) /)
  defsvm%interp_weights(8, 1:defsvm%cv_split) = (/ k(3), k(2), k(1) /)
  defsvm%interp_weights(9, 1:defsvm%cv_split) = (/ k(2), k(3), k(1) /)
  if (size(defsvm%interp_weights, 1) /= 9) call erreur("SVM initialization", "bad array size") 

case default
  call erreur("parameters parsing","unknown SVM method (init_svmweights)")
endselect

endsubroutine init_svmweights


endmodule MENU_NUM


!------------------------------------------------------------------------------!
! Changes history
!
! mai  2002 : creation du module
! aout 2003 : parametres pour l'integration temporelle (Fourier, residu)
! sept 2003 : parametres pour l'integration spatiale (calcul de gradients)
! jan  2006 : gradient computation method (local routine to get parameters)
! Nov  2007 : Runge-Kutta parameters
!------------------------------------------------------------------------------!
