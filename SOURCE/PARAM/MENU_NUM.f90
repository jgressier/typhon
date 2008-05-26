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
integer(kpp), parameter :: sch_rusanov  = 05
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
integer(kpp), parameter :: jac_rusanov = 16
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
integer(kpp), parameter :: postlim_none        = 0
integer(kpp), parameter :: postlim_monotonic0  = 10
integer(kpp), parameter :: postlim_monotonic1  = 11
integer(kpp), parameter :: postlim_monotonic2  = 12

! -- Constants for SVM order
integer(kpp), parameter :: svm_2       = 2
integer(kpp), parameter :: svm_3       = 3

! -- Constants for SVM splitting
integer(kpp), parameter :: svm_2tri    = 21
integer(kpp), parameter :: svm_2quad   = 22
integer(kpp), parameter :: svm_3wang   = 31
integer(kpp), parameter :: svm_3kris   = 32
integer(kpp), parameter :: svm_3kris2  = 33

! -- Constants for SVM splitting
integer(kpp), parameter :: svm_fluxQ    = 31
integer(kpp), parameter :: svm_fluxF    = 32

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
  character       :: time_model ! Steady, Unsteady... (cf MENU_GEN)
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
  integer(kpp)      :: sv_order             ! SV Order (cf constants)
  integer(kpp)      :: sv_partition         ! SV Type of partition (cf constants)
  integer(kpp)      :: sv_flux              ! Type of flux integration on gauss points for SVM method
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

select case(svm%sv_order)
case(svm_2)
  svm%cv_split       = 3      ! nb of CV in SV
  svm%intnode        = 1      ! nb of internal added nodes for cell splitting
  svm%svface_split   = 2      ! nb of CV face per SV face
  svm%internal_faces = 3      ! number of internal faces (by cell)
  svm%nb_subfaces    = 3+2*3  ! total number of sub faces (by cell)
  svm%nb_facepoints  = 1      ! number of integration points by face
case(svm_3)
  svm%cv_split       = 6      ! nb of CV in SV
  svm%intnode        = 4      ! nb of internal added nodes for cell splitting
  svm%svface_split   = 3      ! nb of CV face per SV face
  svm%internal_faces = 9      ! number of internal faces (by cell)
  svm%nb_subfaces    = 9+3*3  ! total number of sub faces (by cell)
  svm%nb_facepoints  = 2      ! number of integration points by face
case default
  call erreur("parameters parsing","unknown SVM order (init_svmparam)")
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
real(krp)     :: k1(1:5)
real(krp), dimension(1:7,1:6) :: k
! -- body --

allocate(defsvm%interp_weights(1:defsvm%nb_subfaces*defsvm%nb_facepoints, &
                               1:defsvm%cv_split))

select case(defsvm%sv_order)

case(svm_2)
  k1(1) =  4._krp /  3._krp
  k1(2) =  2._krp / 15._krp
  k1(3) = -7._krp / 15._krp
  k1(4) = -1._krp / 15._krp
  k1(5) =  8._krp / 15._krp
  defsvm%interp_weights(1, 1:defsvm%cv_split) = (/ k1(4), k1(5), k1(5) /)
  defsvm%interp_weights(2, 1:defsvm%cv_split) = (/ k1(5), k1(4), k1(5) /)
  defsvm%interp_weights(3, 1:defsvm%cv_split) = (/ k1(5), k1(5), k1(4) /)
  defsvm%interp_weights(4, 1:defsvm%cv_split) = (/ k1(1), k1(3), k1(2) /)
  defsvm%interp_weights(5, 1:defsvm%cv_split) = (/ k1(1), k1(2), k1(3) /)
  defsvm%interp_weights(6, 1:defsvm%cv_split) = (/ k1(2), k1(1), k1(3) /)
  defsvm%interp_weights(7, 1:defsvm%cv_split) = (/ k1(3), k1(1), k1(2) /)
  defsvm%interp_weights(8, 1:defsvm%cv_split) = (/ k1(3), k1(2), k1(1) /)
  defsvm%interp_weights(9, 1:defsvm%cv_split) = (/ k1(2), k1(3), k1(1) /)
  if (size(defsvm%interp_weights, 1) /= 9) call erreur("SVM initialization", "bad array size") 

case(svm_3) ! 36 Gauss pts, 6 coeff per point, 7 independant points

select case(defsvm%sv_partition)

  case(svm_3wang) !weights for alpha=1/4 and beta =1/3 : ORIGINAL PARTITION BY WANG

!Gauss point1 
  k(1,1) = 5353._krp /4017._krp + 2287._krp / 10712._krp * sqrt(3._krp)    !1.702377414448907  
  k(1,2) =-263._krp / 4017._krp + 623._krp / 10712._krp * sqrt(3._krp)     !0.0352627258945180
  k(1,3) = 781._krp / 4017._krp - 129._krp / 10712._krp * sqrt(3._krp)     ! 0.1735653577754183
  k(1,4) = 346._krp / 1339._krp - 3375._krp / 10712._krp  * sqrt(3._krp)   !-0.2873106306520688
  k(1,5) = 89._krp / 2678._krp + 765._krp / 10712._krp * sqrt(3._krp)      !0.1569285724225346
  k(1,6) =-2017._krp /2678._krp  - 171._krp /10712._krp  * sqrt(3._krp)    !-0.7808234398893090
!Gauss point2 coefficients
  k(2,1) = 5353._krp / 4017._krp - 2287._krp / 10712._krp * sqrt(3._krp)   !0.9627956002386711 
  k(2,2) =-263._krp / 4017._krp - 623._krp / 10712._krp * sqrt(3._krp)     !-0.1662062160613092
  k(2,3) = 781._krp / 4017._krp + 129._krp / 10712._krp * sqrt(3._krp)     !0.2152820407807181
  k(2,4) = 346._krp / 1339._krp + 3375._krp / 10712._krp * sqrt(3._krp)    !0.8041142154168186
  k(2,5) = 89._krp / 2678._krp - 765._krp / 10712._krp * sqrt(3._krp)      !-0.09046105935307983
  k(2,6) =-2017._krp / 2678._krp + 171._krp / 10712._krp * sqrt(3._krp)    !-0.7255245810218187
!Gauss point3 coefficients
  k(3,1) = 725._krp / 8034._krp + 16._krp / 103._krp * sqrt(3._krp)        !0.3592979098638236  
  k(3,2) = 725._krp / 8034._krp - 16._krp / 103._krp * sqrt(3._krp)        !-0.1788149623905849
  k(3,3) = 1949._krp / 8034._krp                                      !0.2425939756036843
  k(3,4) = 4067._krp / 2678._krp                                      !1.518670649738611
  k(3,5) =-97._krp / 206._krp + 9._krp / 103._krp * sqrt(3._krp)           !-0.3195295410862146
  k(3,6) =-97._krp / 206._krp - 9._krp / 103._krp * sqrt(3._krp)           !-0.6222180317293194
 !Gauss point19 coefficients
  k(4,1) = 9407._krp / 16068._krp + 1583._krp /32136._krp * sqrt(3._krp)   !0.6707691196285018
  k(4,2) =-2401._krp / 16068._krp - 529._krp /32136._krp * sqrt(3._krp)    !-0.1779392232139637
  k(4,3) = 719._krp / 16068._krp + 1727._krp /32136._krp * sqrt(3._krp)    !0.1378283465481532
  k(4,4) = 3875._krp / 5356._krp + 1283._krp /10712._krp * sqrt(3._krp)    !0.9309392444091551
  k(4,5) =-367._krp / 5356._krp - 289._krp /10712._krp * sqrt(3._krp)      !-0.1152504372094292
  k(4,6) =-727._krp / 5356._krp - 1921._krp /10712._krp * sqrt(3._krp)     !-0.4463470501624172
!Gauss point20 coefficients
  k(5,1) = 9407._krp / 16068._krp - 1583._krp / 32136._krp * sqrt(3._krp)  !0.5001295609789167   
  k(5,2) =-2401._krp / 16068._krp + 529._krp / 32136._krp * sqrt(3._krp)   !-0.1209156436020682
  k(5,3) = 719._krp / 16068._krp - 1727._krp / 32136._krp * sqrt(3._krp)   !-0.04833369880107822
  k(5,4) = 3875._krp / 5356._krp - 1283._krp / 10712._krp * sqrt(3._krp)   !0.5160361103331899
  k(5,5) =-367._krp / 5356._krp + 289._krp / 10712._krp * sqrt(3._krp)     !-0.02179213187197484
  k(5,6) =-727._krp / 5356._krp + 1921._krp / 10712._krp * sqrt(3._krp)    !0.1748758029630147
!Gauss point31 coefficients
  k(6,1) = 1043._krp / 16068._krp + 263._krp /2678._krp *sqrt(3._krp)      !0.2350122090081957
  k(6,2) =-2221._krp / 16068._krp + 23._krp /2678._krp *sqrt(3._krp)       !-0.1233493271443549
  k(6,3) =-2221._krp / 16068._krp + 23._krp /2678._krp *sqrt(3._krp)       !-0.1233493271443548
  k(6,4) = 2783._krp / 5356._krp - 45._krp /2678._krp *sqrt(3._krp)        !0.4904995196637045
  k(6,5) = 71._krp / 412._krp - 219._krp /2678._krp *sqrt(3._krp)          !0.03068740595310525
  k(6,6) = 2783._krp / 5356._krp - 45._krp /2678._krp *sqrt(3._krp)        !0.4904995196637045  
!Gauss point32 coefficients
  k(7,1) = 1043._krp / 16068._krp - 263._krp /2678._krp * sqrt(3._krp)     !-0.1051889578257212 
  k(7,2) =-2221._krp / 16068._krp - 23._krp /2678._krp * sqrt(3._krp)      !-0.1531007599853439
  k(7,3) =-2221._krp / 16068._krp - 23._krp /2678._krp * sqrt(3._krp)      !-0.1531007599853438
  k(7,4) = 2783._krp / 5356._krp + 45._krp /2678._krp * sqrt(3._krp)       !0.5487088447873784
  k(7,5) = 71._krp / 412._krp + 219._krp /2678._krp * sqrt(3._krp)         !0.3139727882216520
  k(7,6) = 2783._krp / 5356._krp + 45._krp /2678._krp * sqrt(3._krp)       !0.5487088447873784

  case(svm_3kris) !weights for alpha=91/1000 and beta =18/100 : OPTIMISED PARTITION BY ABEELE

!Gauss point1 
  k(1,1) = 9811545755400510792702409._krp /11159461345717846055550000._krp + 274495216728312413405._krp /1785513815314855368888._krp * sqrt(3._krp) !1.145489306847780
  k(1,2) =-1271704797569276507297591._krp /11159461345717846055550000._krp + 77459651342182861405._krp /1785513815314855368888._krp  * sqrt(3._krp) !-0.03881723868803048
  k(1,3) = 1266700807704821442702409._krp /11159461345717846055550000._krp - 11510679813889120865._krp /595171271771618456296._krp * sqrt(3._krp) !0.08001107647864331
  k(1,4) = 2177528451913597653049197._krp /3719820448572615351850000._krp  - 158925359421233879135._krp /595171271771618456296._krp * sqrt(3._krp) !0.1228852242141119
  k(1,5) =-245691028258977653200803._krp /3719820448572615351850000._krp + 37538192290271951865._krp /595171271771618456296._krp * sqrt(3._krp) !0.04319343518225036
  k(1,6) =-1480864230260689890700803._krp /3719820448572615351850000._krp + 15579557588019289865._krp /595171271771618456296._krp * sqrt(3._krp) !-0.352761804034!7553
!Gauss point2 coefficients
  k(2,1) = 9811545755400510792702409._krp /11159461345717846055550000._krp - 274495216728312413405._krp /1785513815314855368888._krp * sqrt(3._krp) !0.6129370994886576 
  k(2,2) =-1271704797569276507297591._krp /11159461345717846055550000._krp - 77459651342182861405._krp /1785513815314855368888._krp * sqrt(3._krp) !-0.1890978475642751
  k(2,3) = 1266700807704821442702409._krp /11159461345717846055550000._krp + 11510679813889120865._krp /595171271771618456296._krp * sqrt(3._krp) !0.1470071941103551
  k(2,4) = 2177528451913597653049197._krp /3719820448572615351850000._krp + 158925359421233879135._krp /595171271771618456296._krp * sqrt(3._krp) !1.047885506265187
  k(2,5) =-245691028258977653200803._krp /3719820448572615351850000._krp - 37538192290271951865._krp /595171271771618456296._krp * sqrt(3._krp) !-0.1752917617846837
  k(2,6) =-1480864230260689890700803._krp /3719820448572615351850000._krp - 15579557588019289865._krp /595171271771618456296._krp * sqrt(3._krp) !-0.4434401905152414
!Gauss point3 coefficients
  k(3,1) =-129515371965573792422591._krp /11159461345717846055550000._krp + 49118118818000._krp /445102789234767._krp * sqrt(3._krp) !0.1795299189974726  
  k(3,2) =-129515371965573792422591._krp /11159461345717846055550000._krp - 49118118818000._krp /445102789234767._krp * sqrt(3._krp) !-0.2027416794854054
  k(3,3) = 2130001793746505507577409._krp /11159461345717846055550000._krp !0.1908695883931564
  k(3,4) = 6150662437444444631424197._krp /3719820448572615351850000._krp !1.653483688924987
  k(3,5) =-1527249502738474293575803._krp /3719820448572615351850000._krp + 5473970276750._krp /148367596411589._krp * sqrt(3._krp) !-0.3466673531718809
  k(3,6) =-1527249502738474293575803._krp /3719820448572615351850000._krp - 5473970276750._krp /148367596411589._krp * sqrt(3._krp) !-0.4744741636583288
 !Gauss point19 coefficients
  k(4,1) = 3875785067027954120639909._krp /11159461345717846055550000._krp + 60311816912857471135._krp /1785513815314855368888._krp * sqrt(3._krp) !0.4058152536817871
  k(4,2) =-1758803022644279079360091._krp /11159461345717846055550000._krp - 33247949076882528865._krp /1785513815314855368888._krp * sqrt(3._krp) !-0.1898588617819963
  k(4,3) = 64951348397981290213303._krp /3719820448572615351850000._krp + 26247913902322565045._krp /595171271771618456296._krp * sqrt(3._krp) !0.09384682840778316
  k(4,4) = 29873336898185690431130273._krp /33478384037153538166650000._krp + 231303193344023458865._krp /1785513815314855368888._krp * sqrt(3._krp) !1.116694533692579
  k(4,5) =-4915369747391599037619727._krp /33478384037153538166650000._krp - 44899245203230048135._krp /1785513815314855368888._krp * sqrt(3._krp) !-0.1903770355581176
  k(4,6) = 1584908617626590037380273._krp /33478384037153538166650000._krp - 292211557683736048135._krp /1785513815314855368888._krp * sqrt(3._krp) !-0.2361207184420353
!Gauss point20 coefficients
  k(5,1) = 3875785067027954120639909._krp /11159461345717846055550000._krp - 60311816912857471135._krp /1785513815314855368888._krp * sqrt(3._krp) !0.2888034105990271
  k(5,2) =-1758803022644279079360091._krp /11159461345717846055550000._krp + 33247949076882528865._krp /1785513815314855368888._krp * sqrt(3._krp) !-0.1253540267539140
  k(5,3) = 64951348397981290213303._krp /3719820448572615351850000._krp - 26247913902322565045._krp /595171271771618456296._krp * sqrt(3._krp) !-0.05892506307209079
  k(5,4) = 29873336898185690431130273._krp /33478384037153538166650000._krp - 231303193344023458865._krp /1785513815314855368888._krp * sqrt(3._krp) !0.6679398061867267
  k(5,5) =-4915369747391599037619727._krp /33478384037153538166650000._krp + 44899245203230048135._krp /1785513815314855368888._krp * sqrt(3._krp) !-0.1032673495434236
  k(5,6) = 1584908617626590037380273._krp /33478384037153538166650000._krp + 292211557683736048135._krp /1785513815314855368888._krp * sqrt(3._krp) !0.3308032225836745
!Gauss point31 coefficients
  k(6,1) =-29744331675183862036697._krp /3719820448572615351850000._krp + 29002367626391885045._krp /446378453828713842222._krp * sqrt(3._krp) !0.1045396659884165
  k(6,2) =-1444655696271980686110091._krp /11159461345717846055550000._krp + 1044472592431499015._krp /148792817942904614074._krp * sqrt(3._krp) !-0.1172973036896558
  k(6,3) =-1444655696271980686110091._krp /11159461345717846055550000._krp + 1044472592431499015._krp /148792817942904614074._krp * sqrt(3._krp) !-0.1172973036896556
  k(6,4) = 19085640343298296776380273._krp /33478384037153538166650000._krp + 10089898912456124365._krp /1339135361486141526666._krp * sqrt(3._krp) !0.5831388325261704
  k(6,5) = 4242736513265483488880273._krp /33478384037153538166650000._krp - 125987407367854886135._krp /1339135361486141526666._krp * sqrt(3._krp) !-0.0362227236614459
  k(6,6) = 19085640343298296776380273._krp /33478384037153538166650000._krp + 10089898912456124365._krp /1339135361486141526666._krp * sqrt(3._krp) !0.5831388325261701
!Gauss point32 coefficients
  k(7,1) =-29744331675183862036697._krp /3719820448572615351850000._krp - 29002367626391885045._krp /446378453828713842222._krp * sqrt(3._krp) !-0.1205320140527420
  k(7,2) =-1444655696271980686110091._krp /11159461345717846055550000._krp - 1044472592431499015._krp /148792817942904614074._krp * sqrt(3._krp) !-0.1416140633587782
  k(7,3) =-1444655696271980686110091._krp /11159461345717846055550000._krp - 1044472592431499015._krp /148792817942904614074._krp * sqrt(3._krp) !-0.1416140633587777
  k(7,4) = 19085640343298296776380273._krp /33478384037153538166650000._krp - 10089898912456124365._krp /1339135361486141526666._krp * sqrt(3._krp) !0.5570380841444490
  k(7,5) = 4242736513265483488880273._krp /33478384037153538166650000._krp + 125987407367854886135._krp /1339135361486141526666._krp * sqrt(3._krp) !0.2896839724814004
  k(7,6) = 19085640343298296776380273._krp /33478384037153538166650000._krp - 10089898912456124365._krp /1339135361486141526666._krp * sqrt(3._krp) !0.5570380841444472

  case(svm_3kris2) !weights for alpha=0.1093621117 and beta =0.1730022492 : OPTIMISED PARTITION BY ABEELE

!Gauss point1 
  k(1,1) = 1.162174755910899
  k(1,2) =-0.03717154387692906
  k(1,3) = 0.08197262370290026
  k(1,4) = 0.1096136397063276
  k(1,5) = 0.04986665960751276
  k(1,6) =-0.3664561350507103
!Gauss point2 coefficients
  k(2,1) = 0.6240398968565745
  k(2,2) =-0.1881802581990285
  k(2,3) = 0.1484186518281137
  k(2,4) = 1.039245803517743
  k(2,5) =-0.1707911844200166
  k(2,6) =-0.4527329095833859
!Gauss point3 coefficients
  k(3,1) = 0.1859015574398123
  k(3,2) =-0.2012245872924130
  k(3,3) = 0.1919209022148539
  k(3,4) = 1.647876481659745
  k(3,5) =-0.3450466422635727
  k(3,6) =-0.4794277117584260
 !Gauss point19 coefficients
  k(4,1) = 0.4145929297162591
  k(4,2) =-0.1890656833961377
  k(4,3) = 0.09479961640339063
  k(4,4) = 1.109716226309553
  k(4,5) =-0.1867423294300532
  k(4,6) =-0.2433007596030126
!Gauss point20 coefficients
  k(5,1) = 0.2961061714853251
  k(5,2) =-0.1248778572655495
  k(5,3) =-0.05846729991333386
  k(5,4) = 0.6620913980845936
  k(5,5) =-0.09983576290715285
  k(5,6) = 0.3249833505161174
!Gauss point31 coefficients
  k(6,1) = 0.1095577357082066
  k(6,2) =-0.116994679373928
  k(6,3) =-0.116994679373928
  k(6,4) = 0.579133116291440
  k(6,5) =-0.0338346095432306
  k(6,6) = 0.579133116291440
!Gauss point32 coefficients
  k(7,1) =-0.1189947927242930
  k(7,2) =-0.141095647435725
  k(7,3) =-0.141095647435725
  k(7,4) = 0.555719113109680
  k(7,5) = 0.2897478613763849
  k(7,6) = 0.555719113109680


endselect


  defsvm%interp_weights(1, 1:defsvm%cv_split) = (/ k(1,1), k(1,2), k(1,3) , k(1,4), k(1,5), k(1,6)/)!
  defsvm%interp_weights(2, 1:defsvm%cv_split) = (/ k(2,1), k(2,2), k(2,3) , k(2,4), k(2,5), k(2,6)/)!
  defsvm%interp_weights(3, 1:defsvm%cv_split) = (/ k(3,1), k(3,2), k(3,3) , k(3,4), k(3,5), k(3,6)/)!

  defsvm%interp_weights(4, 1:defsvm%cv_split) = (/ k(3,2), k(3,1), k(3,3) , k(3,4), k(3,6), k(3,5)/)
  defsvm%interp_weights(5, 1:defsvm%cv_split) = (/ k(2,2), k(2,1), k(2,3) , k(2,4), k(2,6), k(2,5)/)
  defsvm%interp_weights(6, 1:defsvm%cv_split) = (/ k(1,2), k(1,1), k(1,3) , k(1,4), k(1,6), k(1,5)/)
  defsvm%interp_weights(7, 1:defsvm%cv_split) = (/ k(1,3), k(1,1), k(1,2) , k(1,6), k(1,4), k(1,5)/)
  defsvm%interp_weights(8, 1:defsvm%cv_split) = (/ k(2,3), k(2,1), k(2,2) , k(2,6), k(2,4), k(2,5)/)
  defsvm%interp_weights(9, 1:defsvm%cv_split) = (/ k(3,3), k(3,1), k(3,2) , k(3,6), k(3,4), k(3,5)/)
  defsvm%interp_weights(10, 1:defsvm%cv_split) = (/ k(3,3), k(3,2), k(3,1) , k(3,5), k(3,4), k(3,6)/)
  defsvm%interp_weights(11, 1:defsvm%cv_split) = (/ k(2,3), k(2,2), k(2,1) , k(2,5), k(2,4), k(2,6)/)
  defsvm%interp_weights(12, 1:defsvm%cv_split) = (/ k(1,3), k(1,2), k(1,1) , k(1,5), k(1,4), k(1,6)/)
  defsvm%interp_weights(13, 1:defsvm%cv_split) = (/ k(1,2), k(1,3), k(1,1) , k(1,5), k(1,6), k(1,4)/)
  defsvm%interp_weights(14, 1:defsvm%cv_split) = (/ k(2,2), k(2,3), k(2,1) , k(2,5), k(2,6), k(2,4)/)
  defsvm%interp_weights(15, 1:defsvm%cv_split) = (/ k(3,2), k(3,3), k(3,1) , k(3,5), k(3,6), k(3,4)/)
  defsvm%interp_weights(16, 1:defsvm%cv_split) = (/ k(3,1), k(3,3), k(3,2) , k(3,6), k(3,5), k(3,4)/)
  defsvm%interp_weights(17, 1:defsvm%cv_split) = (/ k(2,1), k(2,3), k(2,2) , k(2,6), k(2,5), k(2,4)/)
  defsvm%interp_weights(18, 1:defsvm%cv_split) = (/ k(1,1), k(1,3), k(1,2) , k(1,6), k(1,5), k(1,4)/)

  defsvm%interp_weights(19, 1:defsvm%cv_split) = (/ k(4,1), k(4,2), k(4,3) , k(4,4), k(4,5), k(4,6)/)!
  defsvm%interp_weights(20, 1:defsvm%cv_split) = (/ k(5,1), k(5,2), k(5,3) , k(5,4), k(5,5), k(5,6)/)!
 
  defsvm%interp_weights(21, 1:defsvm%cv_split) = (/ k(4,2), k(4,1), k(4,3) , k(4,4), k(4,6), k(4,5)/)
  defsvm%interp_weights(22, 1:defsvm%cv_split) = (/ k(5,2), k(5,1), k(5,3) , k(5,4), k(5,6), k(5,5)/)
  defsvm%interp_weights(23, 1:defsvm%cv_split) = (/ k(4,3), k(4,1), k(4,2) , k(4,6), k(4,4), k(4,5)/)
  defsvm%interp_weights(24, 1:defsvm%cv_split) = (/ k(5,3), k(5,1), k(5,2) , k(5,6), k(5,4), k(5,5)/)
  defsvm%interp_weights(25, 1:defsvm%cv_split) = (/ k(4,3), k(4,2), k(4,1) , k(4,5), k(4,4), k(4,6)/)
  defsvm%interp_weights(26, 1:defsvm%cv_split) = (/ k(5,3), k(5,2), k(5,1) , k(5,5), k(5,4), k(5,6)/)
  defsvm%interp_weights(27, 1:defsvm%cv_split) = (/ k(4,2), k(4,3), k(4,1) , k(4,5), k(4,6), k(4,4)/)
  defsvm%interp_weights(28, 1:defsvm%cv_split) = (/ k(5,2), k(5,3), k(5,1) , k(5,5), k(5,6), k(5,4)/)
  defsvm%interp_weights(29, 1:defsvm%cv_split) = (/ k(4,1), k(4,3), k(4,2) , k(4,6), k(4,5), k(4,4)/)
  defsvm%interp_weights(30, 1:defsvm%cv_split) = (/ k(5,1), k(5,3), k(5,2) , k(5,6), k(5,5), k(5,4)/)

  defsvm%interp_weights(31, 1:defsvm%cv_split) = (/ k(6,1), k(6,2), k(6,3) , k(6,4), k(6,5), k(6,6)/)!
  defsvm%interp_weights(32, 1:defsvm%cv_split) = (/ k(7,1), k(7,2), k(7,3) , k(7,4), k(7,5), k(7,6)/)!

  defsvm%interp_weights(33, 1:defsvm%cv_split) = (/ k(6,2), k(6,1), k(6,3) , k(6,4), k(6,6), k(6,5)/)
  defsvm%interp_weights(34, 1:defsvm%cv_split) = (/ k(7,2), k(7,1), k(7,3) , k(7,4), k(7,6), k(7,5)/)
  defsvm%interp_weights(35, 1:defsvm%cv_split) = (/ k(6,3), k(6,2), k(6,1) , k(6,5), k(6,4), k(6,6)/)
  defsvm%interp_weights(36, 1:defsvm%cv_split) = (/ k(7,3), k(7,2), k(7,1) , k(7,5), k(7,4), k(7,6)/)

  if (size(defsvm%interp_weights, 1) /= 36) call erreur("SVM initialization", "bad array size") 

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
