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
integer(kpp), parameter :: postlim_barth       = 20

! -- Constants for SVM order
integer(kpp), parameter :: svm_2       = 2
integer(kpp), parameter :: svm_3       = 3
integer(kpp), parameter :: svm_4       = 4

! -- Constants for SVM splitting
integer(kpp), parameter :: svm_2tri    = 21
integer(kpp), parameter :: svm_2quad   = 22
integer(kpp), parameter :: svm_3wang   = 31
integer(kpp), parameter :: svm_3kris   = 32
integer(kpp), parameter :: svm_3kris2  = 33
integer(kpp), parameter :: svm_4wang   = 41
integer(kpp), parameter :: svm_4kris   = 42
integer(kpp), parameter :: svm_4kris2  = 43

! -- Constants for SVM fluxes computation
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
case(svm_4)
  svm%cv_split       = 10      ! nb of CV in SV
  svm%svface_split   = 4      ! nb of CV face per SV face
  svm%nb_facepoints  = 2      ! number of integration points by face
  select case(svm%sv_partition)
    case(svm_4wang) 
     svm%intnode        = 6      ! nb of internal added nodes for cell splitting
     svm%internal_faces = 15      ! number of internal faces (by cell)
     svm%nb_subfaces    = 15+4*3  ! total number of sub faces (by cell)
    case(svm_4kris) 
     svm%intnode        = 9      ! nb of internal added nodes for cell splitting
     svm%internal_faces = 18      ! number of internal faces (by cell)
     svm%nb_subfaces    = 18+4*3  ! total number of sub faces (by cell)
    case(svm_4kris2) 
     svm%intnode        = 9      ! nb of internal added nodes for cell splitting
     svm%internal_faces = 18      ! number of internal faces (by cell)
     svm%nb_subfaces    = 18+4*3  ! total number of sub faces (by cell)
  endselect 
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
real(krp), dimension(1:10,1:10) :: k4
real(krp), dimension(1:12,1:10) :: kk4
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

case(svm_3) ! 36 Gauss pts, 6 coeff per point, 7 independent points

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
  k(1,1) = 3287080870527984892769449._krp /2789865336429461513887500._krp + 1418701068984221352033613._krp /22318922691435692111100000._krp * sqrt(3._krp) !1.28831976766918
  k(1,2) =-71144047021860659130551._krp /2789865336429461513887500._krp + 522189246477331890433613._krp /22318922691435692111100000._krp * sqrt(3._krp) !.0150233922365182
  k(1,3) = 210615222587040306569449._krp /2789865336429461513887500._krp - 63476794901672945922129._krp /7439640897145230703700000._krp * sqrt(3._krp) !.0607146992628959
  k(1,4) = 56266710319992738581017._krp /929955112143153837962500._krp - 876409787064336349877871._krp /7439640897145230703700000._krp * sqrt(3._krp) !-.143535502808458
  k(1,5) = 52880933332645863637267._krp /929955112143153837962500._krp + 196417465687037246872129._krp /7439640897145230703700000._krp * sqrt(3._krp) !.102592652962310
  k(1,6) =-321376546873872944325233._krp /929955112143153837962500._krp + 96505677791787634772129._krp /7439640897145230703700000._krp * sqrt(3._krp) !-.323115009322447
!Gauss point2 coefficients
  k(2,1) = 3287080870527984892769449._krp /2789865336429461513887500._krp - 1418701068984221352033613._krp /22318922691435692111100000._krp * sqrt(3._krp) !1.06812434280883
  k(2,2) =-71144047021860659130551._krp /2789865336429461513887500._krp - 522189246477331890433613._krp /22318922691435692111100000._krp * sqrt(3._krp) !-.0660251707760604
  k(2,3) = 210615222587040306569449._krp /2789865336429461513887500._krp + 63476794901672945922129._krp /7439640897145230703700000._krp * sqrt(3._krp) !.0902712424862430
  k(2,4) = 56266710319992738581017._krp /929955112143153837962500._krp + 876409787064336349877871._krp /7439640897145230703700000._krp * sqrt(3._krp) !.264545021623451
  k(2,5) = 52880933332645863637267._krp /929955112143153837962500._krp - 196417465687037246872129._krp /7439640897145230703700000._krp * sqrt(3._krp) !.0111352735626110
  k(2,6) =-321376546873872944325233._krp /929955112143153837962500._krp - 96505677791787634772129._krp /7439640897145230703700000._krp * sqrt(3._krp) !-.368050709705079
!Gauss point3 coefficients
  k(3,1) = 137469674468030631738287._krp /1394932668214730756943750._krp + 80357242386248._krp /445102789234767._krp * sqrt(3._krp) !.411247493702439
  k(3,2) = 137469674468030631738287._krp /1394932668214730756943750._krp - 80357242386248._krp /445102789234767._krp * sqrt(3._krp) !-.214148841415555
  k(3,3) = 236097710678835289113287._krp /1394932668214730756943750._krp !.169253840030142
  k(3,4) = 630063153967919686514321._krp /464977556071576918981250._krp !1.35503992771412
  k(3,5) =-167715642217320809198179._krp /464977556071576918981250._krp + 8955415372763._krp /148367596411589._krp * sqrt(3._krp) !-.256150239037658
  k(3,6) =-167715642217320809198179._krp /464977556071576918981250._krp - 8955415372763._krp /148367596411589._krp * sqrt(3._krp) !-.465242180993486
 !Gauss point19 coefficients
  k(4,1) = 9251092766669808504418591._krp /11159461345717846055550000._krp + 22281691035126377513779._krp /421111748895013058700000._krp * sqrt(3._krp) !.920636534649943 
  k(4,2) =-748200062253774879181409._krp /11159461345717846055550000._krp - 3293964392767447286221._krp /421111748895013058700000._krp * sqrt(3._krp) !-.0805944702065471
  k(4,3) = 263898843663433393618591._krp /11159461345717846055550000._krp + 3383896341922490704593._krp /140370582965004352900000._krp * sqrt(3._krp) !.0654023265278943 
  k(4,4) = 1203018128665083511793803._krp /3719820448572615351850000._krp + 3586821656594948395407._krp /140370582965004352900000._krp * sqrt(3._krp) !.367665836904311 
  k(4,5) =-199181021302678486731197._krp /3719820448572615351850000._krp + 1470113694091063145407._krp /140370582965004352900000._krp * sqrt(3._krp) !-.0354059464509639 
  k(4,6) =-206280508149612012831197._krp /3719820448572615351850000._krp - 14770073906728145654593._krp /140370582965004352900000._krp * sqrt(3._krp) !-.237704281424637 
!Gauss point20 coefficients
  k(5,1) = 9251092766669808504418591._krp /11159461345717846055550000._krp - 22281691035126377513779._krp /421111748895013058700000._krp * sqrt(3._krp) !.737345419867903
  k(5,2) =-748200062253774879181409._krp /11159461345717846055550000._krp + 3293964392767447286221._krp /421111748895013058700000._krp * sqrt(3._krp) !-.0534980346330126
  k(5,3) = 263898843663433393618591._krp /11159461345717846055550000._krp - 3383896341922490704593._krp /140370582965004352900000._krp * sqrt(3._krp) !-.0181063441344972
  k(5,4) = 1203018128665083511793803._krp /3719820448572615351850000._krp - 3586821656594948395407._krp /140370582965004352900000._krp * sqrt(3._krp) !.279149322750349
  k(5,5) =-199181021302678486731197._krp /3719820448572615351850000._krp - 1470113694091063145407._krp /140370582965004352900000._krp * sqrt(3._krp) !-.0716857930866837
  k(5,6) =-206280508149612012831197._krp /3719820448572615351850000._krp + 14770073906728145654593._krp /140370582965004352900000._krp * sqrt(3._krp) !.126795429235941
!Gauss point31 coefficients
  k(6,1) = 505076654113076972792003._krp /3719820448572615351850000._krp + 30355756079882984487361._krp /223189226914356921111000._krp * sqrt(3._krp) !.371354443156496
  k(6,2) =-1249121416964422867623991._krp /11159461345717846055550000._krp + 1205693445067739415787._krp /74396408971452307037000._krp * sqrt(3._krp) !-.0838636419975609
  k(6,3) =-1249121416964422867623991._krp /11159461345717846055550000._krp + 1205693445067739415787._krp /74396408971452307037000._krp * sqrt(3._krp) !-.0838636419975616
  k(6,4) = 17289833884649582135671973._krp /33478384037153538166650000._krp - 25239640854818887127083._krp /669567680743070763333000._krp * sqrt(3._krp) !.451157285597195
  k(6,5) = 1847754882623218345921973._krp /33478384037153538166650000._krp - 62290468541230488692083._krp /669567680743070763333000._krp * sqrt(3._krp) !-.105941730355765
  k(6,6) = 17289833884649582135671973._krp /33478384037153538166650000._krp - 25239640854818887127083._krp /669567680743070763333000._krp * sqrt(3._krp) !.451157285597204
!Gauss point32 coefficients
  k(7,1) = 505076654113076972792003._krp /3719820448572615351850000._krp - 30355756079882984487361._krp /223189226914356921111000._krp * sqrt(3._krp) !-.0997947476841605
  k(7,2) =-1249121416964422867623991._krp /11159461345717846055550000._krp - 1205693445067739415787._krp /74396408971452307037000._krp * sqrt(3._krp) !-.140004048075805
  k(7,3) =-1249121416964422867623991._krp /11159461345717846055550000._krp - 1205693445067739415787._krp /74396408971452307037000._krp * sqrt(3._krp) !-.140004048075802
  k(7,4) = 17289833884649582135671973._krp /33478384037153538166650000._krp + 25239640854818887127083._krp /669567680743070763333000._krp * sqrt(3._krp) !.581738081482745
  k(7,5) = 1847754882623218345921973._krp /33478384037153538166650000._krp + 62290468541230488692083._krp /669567680743070763333000._krp * sqrt(3._krp) !.216326680870262
  k(7,6) = 17289833884649582135671973._krp /33478384037153538166650000._krp + 25239640854818887127083._krp /669567680743070763333000._krp * sqrt(3._krp) !.581738081482761

  case(svm_3kris2) !weights for alpha=0.1093621117 and beta =0.1730022492 : OPTIMISED PARTITION BY ABEELE

!Gauss point1 
  k(1,1) = 1.28944701993
  k(1,2) = 0.0103882882085
  k(1,3) = 0.0651156162188
  k(1,4) =-0.126229145560
  k(1,5) = 0.102635587273
  k(1,6) =-0.341357366061
!Gauss point2 coefficients
  k(2,1) = 1.02577584775
  k(2,2) =-0.0839351532395
  k(2,3) = 0.0996328359745
  k(2,4) = 0.356693891316
  k(2,5) =-0.0064796292939
  k(2,6) =-0.391687792499
!Gauss point3 coefficients
  k(3,1) = 0.390385234969
  k(3,2) =-0.214519323766
  k(3,3) = 0.173486796107
  k(3,4) = 1.389968846972
  k(3,5) =-0.264672102711
  k(3,6) =-0.474649451571
 !Gauss point19 coefficients
  k(4,1) = 0.885879027380
  k(4,2) =-0.0966632190484
  k(4,3) = 0.0749322000852
  k(4,4) = 0.450383624165
  k(4,5) =-0.0480832622194
  k(4,6) =-0.266448370364
!Gauss point20 coefficients
  k(5,1) = 0.755327834819
  k(5,2) =-0.0552290654330
  k(5,3) =-0.0124743075030
  k(5,4) = 0.280712020840
  k(5,5) =-0.0654699723469
  k(5,6) = 0.0971334896216
!Gauss point31 coefficients
  k(6,1) = 0.394269474653
  k(6,2) =-0.0814463773805
  k(6,3) =-0.0814463773805
  k(6,4) = 0.435664029246
  k(6,5) =-0.102704778384
  k(6,6) = 0.435664029246
!Gauss point32 coefficients
  k(7,1) =-0.0966823063266
  k(7,2) =-0.139478945801
  k(7,3) =-0.139478945801
  k(7,4) = 0.581005928105
  k(7,5) = 0.213628341733
  k(7,6) = 0.581005928105


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

case(svm_4) ! 54 Gauss pts, 10 coeff per point

select case(defsvm%sv_partition)

  case(svm_4wang)   ! 10 independent points
!Gauss point1 coefficients
  k4(1,1) = 1768981733._krp /1381048200._krp + 4453635187._krp /43503018300._krp * sqrt(3._krp) !1.458217141953126
  k4(1,2) =-42581._krp /197292600._krp - 876224413._krp /43503018300._krp * sqrt(3._krp) !-.03510226122972749
  k4(1,3) =-669387319._krp /9667337400._krp + 35661._krp /76724900._krp * sqrt(3._krp) !-.06843711892739588
  k4(1,4) =-1292945493883._krp /3086761276177488._krp - 1948167267562391._krp /13890425742798696._krp * sqrt(3._krp) !-.2433433651618313
  k4(1,5) = 2070067978117._krp /3086761276177488._krp + 802347429185609._krp /13890425742798696._krp * sqrt(3._krp) !.1007184260927767
  k4(1,6) = 1590450600497._krp /10803664466621208._krp - 396689847695287._krp /97232980199590872._krp * sqrt(3._krp) !-.006919184357709881
  k4(1,7) = 7885037444497._krp /10803664466621208._krp - 143139725078429._krp /32410993399863624._krp * sqrt(3._krp) !-.006919570817126903
  k4(1,8) = 2072224105156747._krp /10803664466621208._krp + 11668442484071._krp /32410993399863624._krp * sqrt(3._krp) !.1924310857662292
  k4(1,9) =-4350258709591253._krp /10803664466621208._krp - 1377665997703787._krp /97232980199590872._krp * sqrt(3._krp) !-.4272060344642104
  k4(1,10)=-109._krp /63700._krp + 1307._krp /59150._krp * sqrt(3._krp) !.03656088114587044
!Gauss point2 coefficients
  k4(2,1) = 1768981733._krp /1381048200._krp - 4453635187._krp /43503018300._krp * sqrt(3._krp) !1.103578649098918
  k4(2,2) =-42581._krp /197292600._krp + 876224413._krp /43503018300._krp * sqrt(3._krp) !.03467060793913270
  k4(2,3) =-669387319._krp /9667337400._krp - 35661._krp /76724900._krp * sqrt(3._krp) !-.07004720023994796
  k4(2,4) =-1292945493883._krp /3086761276177488._krp + 1948167267562391._krp /13890425742798696._krp * sqrt(3._krp) !.2425056291801981
  k4(2,5) = 2070067978117._krp /3086761276177488._krp - 802347429185609._krp /13890425742798696._krp * sqrt(3._krp) !-.09937717045756253
  k4(2,6) = 1590450600497._krp /10803664466621208._krp + 396689847695287._krp /97232980199590872._krp * sqrt(3._krp) !.007213612346548450
  k4(2,7) = 7885037444497._krp /10803664466621208._krp + 143139725078429._krp /32410993399863624._krp * sqrt(3._krp) !.008379267657742133
  k4(2,8) = 2072224105156747._krp /10803664466621208._krp - 11668442484071._krp /32410993399863624._krp * sqrt(3._krp) !.1911839573627110
  k4(2,9) =-4350258709591253._krp /10803664466621208._krp + 1377665997703787._krp /97232980199590872._krp * sqrt(3._krp)!-.3781241797481484
  k4(2,10)=-109._krp /63700._krp - 1307._krp /59150._krp * sqrt(3._krp) !-.03998317313959101
!Gauss point3 coefficients
  k4(3,1) = 2439331739._krp /38669349600._krp + 15686961901._krp /87006036600._krp * sqrt(3._krp) !.3753660403108074
  k4(3,2) =-614999411._krp /38669349600._krp + 241031323._krp /3551266800._krp * sqrt(3._krp) !.1016535732845099
  k4(3,3) =-736786609._krp /9667337400._krp + 6026709._krp /4296594400._krp * sqrt(3._krp) !-.07378451848298236
  k4(3,4) = 517471099935941._krp /440965896596784._krp - 148215128122391._krp /4602744624832704._krp * sqrt(3._krp) !1.117720026854252
  k4(3,5) = 28268412026941._krp /440965896596784._krp - 997604710254391._krp /4602744624832704._krp * sqrt(3._krp) !-.3113012248115170
  k4(3,6) = 368567921114069._krp /13296817805072256._krp + 407116159299559._krp /119671360245650304._krp * sqrt(3._krp) !.03361085855679458
  k4(3,7) = 1103241082882069._krp /13296817805072256._krp - 829749352300147._krp /39890453415216768._krp * sqrt(3._krp) !.04694243992224653
  k4(3,8) = 174303637939063._krp /1022832138851712._krp + 341427445521103._krp /39890453415216768._krp * sqrt(3._krp) !.1852375927004566
  k4(3,9) =-160082362372937._krp /1022832138851712._krp - 6997580727924691._krp /119671360245650304._krp * sqrt(3._krp) !-.2577876752226963
  k4(3,10)=-275887._krp /828100._krp + 1307._krp /19600._krp * sqrt(3._krp) !-.2176571131118718
 !Gauss point4 coefficients
  k4(4,1) = 2439331739._krp /38669349600._krp - 15686961901._krp /87006036600._krp * sqrt(3._krp) !-.2492024629953009
  k4(4,2) =-614999411._krp /38669349600._krp - 241031323._krp /3551266800._krp * sqrt(3._krp) !-.1334616806026631
  k4(4,3) =-736786609._krp /9667337400._krp - 6026709._krp /4296594400._krp * sqrt(3._krp) !-.07864351387264848
  k4(4,4) = 517471099935941._krp /440965896596784._krp + 148215128122391._krp /4602744624832704._krp * sqrt(3._krp) !1.229269179928392
  k4(4,5) = 28268412026941._krp /440965896596784._krp + 997604710254391._krp /4602744624832704._krp * sqrt(3._krp) !.439512554735696
  k4(4,6) = 368567921114069._krp /13296817805072256._krp - 407116159299559._krp /119671360245650304._krp * sqrt(3._krp) !.02182615299246109
  k4(4,7) = 1103241082882069._krp /13296817805072256._krp + 829749352300147._krp /39890453415216768._krp * sqrt(3._krp) !.1189981782098938
  k4(4,8) = 174303637939063._krp /1022832138851712._krp - 341427445521103._krp /39890453415216768._krp * sqrt(3._krp) !.1555879080209930
  k4(4,9) =-160082362372937._krp /1022832138851712._krp + 6997580727924691._krp /119671360245650304._krp * sqrt(3._krp) !-.05523018233632459
  k4(4,10)=-275887._krp /828100._krp - 1307._krp /19600._krp * sqrt(3._krp) !-.4486561340804965
!Gauss point25 coefficients
  k4(5,1) = 3592567981._krp /4833668700._krp + 839433706._krp /10875754575._krp * sqrt(3._krp) !.8769248809228335
  k4(5,2) = 66810139._krp /1208417175._krp - 199901._krp /345262050._krp * sqrt(3._krp) !.05428448465266691
  k4(5,3) = 329477._krp /1611222900._krp - 460815863._krp /21751509150._krp * sqrt(3._krp) !-.03648981515150175
  k4(5,4) = 7705462264376813._krp /21607328933242416._krp + 1277370552912451._krp /97232980199590872._krp * sqrt(3._krp) !.3793676879152942
  k4(5,5) =-3368535131175187._krp /21607328933242416._krp + 9575056126817._krp /32410993399863624._krp * sqrt(3._krp) !-.1553861108461736
  k4(5,6) =-13034778674137._krp /21607328933242416._krp + 33073087956373._krp /8102748349965906._krp * sqrt(3._krp) !.006466475871862843
  k4(5,7) =-1229503611379._krp /7202442977747472._krp + 107401595754119._krp /24308245049897718._krp * sqrt(3._krp) !.007482047576794072
  k4(5,8) =-360226438617._krp /600203581478956._krp + 431550141940577._krp /7479460015353144._krp * sqrt(3._krp) !.09933575347817026
  k4(5,9) = 1832509166447._krp /5401832233310604._krp - 844779563255423._krp /7479460015353144._krp * sqrt(3._krp) !-.1952900077507750
  k4(5,10)= 47._krp /29575._krp - 352._krp /15925._krp * sqrt(3._krp) !-.03669539666917123
!Gauss point26 coefficients
  k4(6,1) = 3592567981._krp /4833668700._krp - 839433706._krp /10875754575._krp * sqrt(3._krp) !.6095518323032922
  k4(6,2) = 66810139._krp /1208417175._krp + 199901._krp /345262050._krp * sqrt(3._krp) !.05629014202789150
  k4(6,3) = 329477._krp /1611222900._krp + 460815863._krp /21751509150._krp * sqrt(3._krp) !.03689879270513509
  k4(6,4) = 7705462264376813._krp /21607328933242416._krp - 1277370552912451._krp /97232980199590872._krp * sqrt(3._krp) !.3338590406806799
  k4(6,5) =-3368535131175187._krp /21607328933242416._krp - 9575056126817._krp /32410993399863624._krp * sqrt(3._krp) !-.1564094971702126
  k4(6,6) =-13034778674137._krp /21607328933242416._krp - 33073087956373._krp /8102748349965906._krp * sqrt(3._krp) !-.007672990449801567
  k4(6,7) =-1229503611379._krp /7202442977747472._krp - 107401595754119._krp /24308245049897718._krp * sqrt(3._krp) !-.007823460515481026
  k4(6,8) =-360226438617._krp /600203581478956._krp - 431550141940577._krp /7479460015353144._krp * sqrt(3._krp) !-.1005361009926898
  k4(6,9) = 1832509166447._krp /5401832233310604._krp + 844779563255423._krp /7479460015353144._krp * sqrt(3._krp) !.1959684846405780
  k4(6,10)= 47._krp /29575._krp + 352._krp /15925._krp * sqrt(3._krp) !.03987375677060830
!Gauss point27 coefficients
  k4(7,1) =-15473103913._krp /77338699200._krp - 5716773361._krp /696048292800._krp * sqrt(3._krp) !-.2142950118210653
  k4(7,2) =-15473103913._krp /77338699200._krp - 5716773361._krp /696048292800._krp * sqrt(3._krp) !-.2142950118210653
  k4(7,3) = 99838637._krp /77338699200._krp - 16836181111._krp /696048292800._krp * sqrt(3._krp) !-.04060432824709804
  k4(7,4) = 729319422439._krp /1045619213184._krp + 514339228783._krp /9410572918656._krp * sqrt(3._krp) !.7921660607666403
  k4(7,5) = 729319422439._krp /1045619213184._krp + 514339228783._krp /9410572918656._krp * sqrt(3._krp) !.7921660607666403
  k4(7,6) = 659561207._krp /522809606592._krp - 1555325987._krp /588160807416._krp * sqrt(3._krp) !-.003318645597139651
  k4(7,7) =-1129134793._krp /522809606592._krp + 25424172013._krp /588160807416._krp * sqrt(3._krp) !.07271086492934911
  k4(7,8) =-1129134793._krp /522809606592._krp + 25424172013._krp /588160807416._krp * sqrt(3._krp) !.07271086492934911
  k4(7,9) = 659561207._krp /522809606592._krp - 1555325987._krp /588160807416._krp * sqrt(3._krp) !-.003318645597139651
  k4(7,10)= 37391._krp /6624800._krp - 76369._krp /509600._krp * sqrt(3._krp) !-.2539222083084711
!Gauss point28 coefficients
  k4(8,1) =-15473103913._krp /77338699200._krp + 5716773361._krp /696048292800._krp * sqrt(3._krp) !-.1858437045797919
  k4(8,2) =-15473103913._krp /77338699200._krp + 5716773361._krp /696048292800._krp * sqrt(3._krp) !-.1858437045797919
  k4(8,3) = 99838637._krp /77338699200._krp + 16836181111._krp /696048292800._krp * sqrt(3._krp) !.04318618281751990
  k4(8,4) = 729319422439._krp /1045619213184._krp - 514339228783._krp /9410572918656._krp * sqrt(3._krp) !.6028339798660485
  k4(8,5) = 729319422439._krp /1045619213184._krp - 514339228783._krp /9410572918656._krp * sqrt(3._krp) !.6028339798660485
  k4(8,6) = 659561207._krp /522809606592._krp + 1555325987._krp /588160807416._krp * sqrt(3._krp) !.005841786712695776
  k4(8,7) =-1129134793._krp /522809606592._krp - 25424172013._krp /588160807416._krp * sqrt(3._krp) !-.07703035247802057
  k4(8,8) =-1129134793._krp /522809606592._krp - 25424172013._krp /588160807416._krp * sqrt(3._krp) !-.07703035247802057
  k4(8,9) = 659561207._krp /522809606592._krp + 1555325987._krp /588160807416._krp * sqrt(3._krp) !.005841786712695776
  k4(8,10)= 37391._krp /6624800._krp + 76369._krp /509600._krp * sqrt(3._krp) !.2652103981406168
!Gauss point43 coefficients
  k4(9,1) =-526303._krp /9367575._krp + 13349422._krp /134268575._krp * sqrt(3._krp) !.1160226867853809
  k4(9,2) = 6302921._krp /402805725._krp + 7471972._krp /134268575._krp * sqrt(3._krp) !.1120352160093954
  k4(9,3) = 25535096._krp /402805725._krp - 30754._krp /19181225._krp * sqrt(3._krp) !.06061601706069237
  k4(9,4) = 375004593519727._krp /450152686109217._krp - 160706727047._krp /300101790739478._krp * sqrt(3._krp) !.8321333555350553
  k4(9,5) =-26938004810273._krp /450152686109217._krp - 45753673419047._krp /300101790739478._krp * sqrt(3._krp) !-.3239112856630841
  k4(9,6) =-50642096137817._krp /1800610744436868._krp - 499400961586._krp /150050895369739._krp * sqrt(3._krp) !-.03388958462684823
  k4(9,7) =-130619563525817._krp /1800610744436868._krp + 2714927155414._krp /150050895369739._krp * sqrt(3._krp) !-.04120316536526829
  k4(9,8) =-20116865096759._krp /138508518802836._krp - 1792228465897._krp /300101790739478._krp * sqrt(3._krp) !-.1555831162807538
  k4(9,9) = 19223615355241._krp /138508518802836._krp + 15588348384103._krp /300101790739478._krp * sqrt(3._krp) !.2287589748838570
  k4(9,10)= 9199._krp /29575._krp - 12672._krp /207025._krp * sqrt(3._krp) !.2050209016615732
!Gauss point44 coefficients
  k4(10,1) =-526303._krp /9367575._krp - 13349422._krp /134268575._krp * sqrt(3._krp) !-.2283896547573468
  k4(10,2) = 6302921._krp /402805725._krp - 7471972._krp /134268575._krp * sqrt(3._krp) !-.0807401245605332
  k4(10,3) = 25535096._krp /402805725._krp + 30754._krp /19181225._krp * sqrt(3._krp) !.06617014517669887
  k4(10,4) = 375004593519727._krp /450152686109217._krp + 160706727047._krp /300101790739478._krp * sqrt(3._krp) !.833988407553757
  k4(10,5) =-26938004810273._krp /450152686109217._krp + 45753673419047._krp /300101790739478._krp * sqrt(3._krp) !.204227428867271
  k4(10,6) =-50642096137817._krp /1800610744436868._krp + 499400961586._krp /150050895369739._krp * sqrt(3._krp) !-.02236032534873068
  k4(10,7) =-130619563525817._krp /1800610744436868._krp - 2714927155414._krp /150050895369739._krp * sqrt(3._krp) !-.1038804557664801
  k4(10,8) =-20116865096759._krp /138508518802836._krp + 1792228465897._krp /300101790739478._krp * sqrt(3._krp) !-.1348952639753363
  k4(10,9) = 19223615355241._krp /138508518802836._krp - 15588348384103._krp /300101790739478._krp * sqrt(3._krp) !.04882128546973781
  k4(10,10)= 9199._krp /29575._krp + 12672._krp /207025._krp * sqrt(3._krp) !.4170585573409627

  defsvm%interp_weights(1, 1:defsvm%cv_split)  = (/ k4(1,1), k4(1,2), k4(1,3) , k4(1,4), k4(1,5), k4(1,6),k4(1,7),k4(1,8),k4(1,9),k4(1,10)/)!
  defsvm%interp_weights(8, 1:defsvm%cv_split)  = (/ k4(1,2), k4(1,1), k4(1,3) , k4(1,5), k4(1,4), k4(1,9),k4(1,8),k4(1,7),k4(1,6),k4(1,10)/)
  defsvm%interp_weights(9, 1:defsvm%cv_split)  = (/ k4(1,3), k4(1,1), k4(1,2) , k4(1,8), k4(1,9), k4(1,4),k4(1,5),k4(1,6),k4(1,7),k4(1,10)/)
  defsvm%interp_weights(16, 1:defsvm%cv_split) = (/ k4(1,3), k4(1,2), k4(1,1) , k4(1,7), k4(1,6), k4(1,5),k4(1,4),k4(1,9),k4(1,8),k4(1,10)/)
  defsvm%interp_weights(17, 1:defsvm%cv_split) = (/ k4(1,2), k4(1,3), k4(1,1) , k4(1,6), k4(1,7), k4(1,8),k4(1,9),k4(1,4),k4(1,5),k4(1,10)/)
  defsvm%interp_weights(24, 1:defsvm%cv_split) = (/ k4(1,1), k4(1,3), k4(1,2) , k4(1,9), k4(1,8), k4(1,7),k4(1,6),k4(1,5),k4(1,4),k4(1,10)/)

  defsvm%interp_weights(2, 1:defsvm%cv_split)  = (/ k4(2,1), k4(2,2), k4(2,3) , k4(2,4), k4(2,5), k4(2,6),k4(2,7),k4(2,8),k4(2,9),k4(2,10)/)!
  defsvm%interp_weights(7, 1:defsvm%cv_split)  = (/ k4(2,2), k4(2,1), k4(2,3) , k4(2,5), k4(2,4), k4(2,9),k4(2,8),k4(2,7),k4(2,6),k4(2,10)/)
  defsvm%interp_weights(10, 1:defsvm%cv_split) = (/ k4(2,3), k4(2,1), k4(2,2) , k4(2,8), k4(2,9), k4(2,4),k4(2,5),k4(2,6),k4(2,7),k4(2,10)/)
  defsvm%interp_weights(15, 1:defsvm%cv_split) = (/ k4(2,3), k4(2,2), k4(2,1) , k4(2,7), k4(2,6), k4(2,5),k4(2,4),k4(2,9),k4(2,8),k4(2,10)/)
  defsvm%interp_weights(18, 1:defsvm%cv_split) = (/ k4(2,2), k4(2,3), k4(2,1) , k4(2,6), k4(2,7), k4(2,8),k4(2,9),k4(2,4),k4(2,5),k4(2,10)/)
  defsvm%interp_weights(23, 1:defsvm%cv_split) = (/ k4(2,1), k4(2,3), k4(2,2) , k4(2,9), k4(2,8), k4(2,7),k4(2,6),k4(2,5),k4(2,4),k4(2,10)/)

  defsvm%interp_weights(3, 1:defsvm%cv_split)  = (/ k4(3,1), k4(3,2), k4(3,3) , k4(3,4), k4(3,5), k4(3,6),k4(3,7),k4(3,8),k4(3,9),k4(3,10)/)!
  defsvm%interp_weights(6, 1:defsvm%cv_split)  = (/ k4(3,2), k4(3,1), k4(3,3) , k4(3,5), k4(3,4), k4(3,9),k4(3,8),k4(3,7),k4(3,6),k4(3,10)/)
  defsvm%interp_weights(11, 1:defsvm%cv_split) = (/ k4(3,3), k4(3,1), k4(3,2) , k4(3,8), k4(3,9), k4(3,4),k4(3,5),k4(3,6),k4(3,7),k4(3,10)/)
  defsvm%interp_weights(14, 1:defsvm%cv_split) = (/ k4(3,3), k4(3,2), k4(3,1) , k4(3,7), k4(3,6), k4(3,5),k4(3,4),k4(3,9),k4(3,8),k4(3,10)/)
  defsvm%interp_weights(19, 1:defsvm%cv_split) = (/ k4(3,2), k4(3,3), k4(3,1) , k4(3,6), k4(3,7), k4(3,8),k4(3,9),k4(3,4),k4(3,5),k4(3,10)/)
  defsvm%interp_weights(22, 1:defsvm%cv_split) = (/ k4(3,1), k4(3,3), k4(3,2) , k4(3,9), k4(3,8), k4(3,7),k4(3,6),k4(3,5),k4(3,4),k4(3,10)/)

  defsvm%interp_weights(4, 1:defsvm%cv_split)  = (/ k4(4,1), k4(4,2), k4(4,3) , k4(4,4), k4(4,5), k4(4,6),k4(4,7),k4(4,8),k4(4,9),k4(4,10)/)!
  defsvm%interp_weights(5, 1:defsvm%cv_split)  = (/ k4(4,2), k4(4,1), k4(4,3) , k4(4,5), k4(4,4), k4(4,9),k4(4,8),k4(4,7),k4(4,6),k4(4,10)/)
  defsvm%interp_weights(12, 1:defsvm%cv_split) = (/ k4(4,3), k4(4,1), k4(4,2) , k4(4,8), k4(4,9), k4(4,4),k4(4,5),k4(4,6),k4(4,7),k4(4,10)/)
  defsvm%interp_weights(13, 1:defsvm%cv_split) = (/ k4(4,3), k4(4,2), k4(4,1) , k4(4,7), k4(4,6), k4(4,5),k4(4,4),k4(4,9),k4(4,8),k4(4,10)/)
  defsvm%interp_weights(20, 1:defsvm%cv_split) = (/ k4(4,2), k4(4,3), k4(4,1) , k4(4,6), k4(4,7), k4(4,8),k4(4,9),k4(4,4),k4(4,5),k4(4,10)/)
  defsvm%interp_weights(21, 1:defsvm%cv_split) = (/ k4(4,1), k4(4,3), k4(4,2) , k4(4,9), k4(4,8), k4(4,7),k4(4,6),k4(4,5),k4(4,4),k4(4,10)/)

  defsvm%interp_weights(25, 1:defsvm%cv_split) = (/ k4(5,1), k4(5,2), k4(5,3) , k4(5,4), k4(5,5), k4(5,6),k4(5,7),k4(5,8),k4(5,9),k4(5,10)/)!
  defsvm%interp_weights(29, 1:defsvm%cv_split) = (/ k4(5,2), k4(5,1), k4(5,3) , k4(5,5), k4(5,4), k4(5,9),k4(5,8),k4(5,7),k4(5,6),k4(5,10)/)
  defsvm%interp_weights(31, 1:defsvm%cv_split) = (/ k4(5,3), k4(5,1), k4(5,2) , k4(5,8), k4(5,9), k4(5,4),k4(5,5),k4(5,6),k4(5,7),k4(5,10)/)
  defsvm%interp_weights(35, 1:defsvm%cv_split) = (/ k4(5,3), k4(5,2), k4(5,1) , k4(5,7), k4(5,6), k4(5,5),k4(5,4),k4(5,9),k4(5,8),k4(5,10)/)
  defsvm%interp_weights(37, 1:defsvm%cv_split) = (/ k4(5,2), k4(5,3), k4(5,1) , k4(5,6), k4(5,7), k4(5,8),k4(5,9),k4(5,4),k4(5,5),k4(5,10)/)
  defsvm%interp_weights(41, 1:defsvm%cv_split) = (/ k4(5,1), k4(5,3), k4(5,2) , k4(5,9), k4(5,8), k4(5,7),k4(5,6),k4(5,5),k4(5,4),k4(5,10)/)

  defsvm%interp_weights(26, 1:defsvm%cv_split) = (/ k4(6,1), k4(6,2), k4(6,3) , k4(6,4), k4(6,5), k4(6,6),k4(6,7),k4(6,8),k4(6,9),k4(6,10)/)!
  defsvm%interp_weights(30, 1:defsvm%cv_split) = (/ k4(6,2), k4(6,1), k4(6,3) , k4(6,5), k4(6,4), k4(6,9),k4(6,8),k4(6,7),k4(6,6),k4(6,10)/)
  defsvm%interp_weights(32, 1:defsvm%cv_split) = (/ k4(6,3), k4(6,1), k4(6,2) , k4(6,8), k4(6,9), k4(6,4),k4(6,5),k4(6,6),k4(6,7),k4(6,10)/)
  defsvm%interp_weights(36, 1:defsvm%cv_split) = (/ k4(6,3), k4(6,2), k4(6,1) , k4(6,7), k4(6,6), k4(6,5),k4(6,4),k4(6,9),k4(6,8),k4(6,10)/)
  defsvm%interp_weights(38, 1:defsvm%cv_split) = (/ k4(6,2), k4(6,3), k4(6,1) , k4(6,6), k4(6,7), k4(6,8),k4(6,9),k4(6,4),k4(6,5),k4(6,10)/)
  defsvm%interp_weights(42, 1:defsvm%cv_split) = (/ k4(6,1), k4(6,3), k4(6,2) , k4(6,9), k4(6,8), k4(6,7),k4(6,6),k4(6,5),k4(6,4),k4(6,10)/)

  defsvm%interp_weights(27, 1:defsvm%cv_split) = (/ k4(7,1), k4(7,2), k4(7,3) , k4(7,4), k4(7,5), k4(7,6),k4(7,7),k4(7,8),k4(7,9),k4(7,10)/)!
  defsvm%interp_weights(33, 1:defsvm%cv_split) = (/ k4(7,3), k4(7,1), k4(7,2) , k4(7,8), k4(7,9), k4(7,4),k4(7,5),k4(7,6),k4(7,7),k4(7,10)/)
  defsvm%interp_weights(39, 1:defsvm%cv_split) = (/ k4(7,1), k4(7,3), k4(7,2) , k4(7,9), k4(7,8), k4(7,7),k4(7,6),k4(7,5),k4(7,4),k4(7,10)/)

  defsvm%interp_weights(28, 1:defsvm%cv_split) = (/ k4(8,1), k4(8,2), k4(8,3) , k4(8,4), k4(8,5), k4(8,6),k4(8,7),k4(8,8),k4(8,9),k4(8,10)/)!
  defsvm%interp_weights(34, 1:defsvm%cv_split) = (/ k4(8,3), k4(8,1), k4(8,2) , k4(8,8), k4(8,9), k4(8,4),k4(8,5),k4(8,6),k4(8,7),k4(8,10)/)
  defsvm%interp_weights(40, 1:defsvm%cv_split) = (/ k4(8,1), k4(8,3), k4(8,2) , k4(8,9), k4(8,8), k4(8,7),k4(8,6),k4(8,5),k4(8,4),k4(8,10)/)

  defsvm%interp_weights(43, 1:defsvm%cv_split) = (/ k4(9,1), k4(9,2), k4(9,3) , k4(9,4), k4(9,5), k4(9,6),k4(9,7),k4(9,8),k4(9,9),k4(9,10)/)!
  defsvm%interp_weights(46, 1:defsvm%cv_split) = (/ k4(9,2), k4(9,1), k4(9,3) , k4(9,5), k4(9,4), k4(9,9),k4(9,8),k4(9,7),k4(9,6),k4(9,10)/)
  defsvm%interp_weights(47, 1:defsvm%cv_split) = (/ k4(9,3), k4(9,1), k4(9,2) , k4(9,8), k4(9,9), k4(9,4),k4(9,5),k4(9,6),k4(9,7),k4(9,10)/)
  defsvm%interp_weights(50, 1:defsvm%cv_split) = (/ k4(9,3), k4(9,2), k4(9,1) , k4(9,7), k4(9,6), k4(9,5),k4(9,4),k4(9,9),k4(9,8),k4(9,10)/)
  defsvm%interp_weights(51, 1:defsvm%cv_split) = (/ k4(9,2), k4(9,3), k4(9,1) , k4(9,6), k4(9,7), k4(9,8),k4(9,9),k4(9,4),k4(9,5),k4(9,10)/)
  defsvm%interp_weights(54, 1:defsvm%cv_split) = (/ k4(9,1), k4(9,3), k4(9,2) , k4(9,9), k4(9,8), k4(9,7),k4(9,6),k4(9,5),k4(9,4),k4(9,10)/)

  defsvm%interp_weights(44, 1:defsvm%cv_split) = (/ k4(10,1), k4(10,2), k4(10,3) , k4(10,4), k4(10,5), k4(10,6),k4(10,7),k4(10,8),k4(10,9),k4(10,10)/)!
  defsvm%interp_weights(45, 1:defsvm%cv_split) = (/ k4(10,2), k4(10,1), k4(10,3) , k4(10,5), k4(10,4), k4(10,9),k4(10,8),k4(10,7),k4(10,6),k4(10,10)/)
  defsvm%interp_weights(48, 1:defsvm%cv_split) = (/ k4(10,3), k4(10,1), k4(10,2) , k4(10,8), k4(10,9), k4(10,4),k4(10,5),k4(10,6),k4(10,7),k4(10,10)/)
  defsvm%interp_weights(49, 1:defsvm%cv_split) = (/ k4(10,3), k4(10,2), k4(10,1) , k4(10,7), k4(10,6), k4(10,5),k4(10,4),k4(10,9),k4(10,8),k4(10,10)/)
  defsvm%interp_weights(52, 1:defsvm%cv_split) = (/ k4(10,2), k4(10,3), k4(10,1) , k4(10,6), k4(10,7), k4(10,8),k4(10,9),k4(10,4),k4(10,5),k4(10,10)/)
  defsvm%interp_weights(53, 1:defsvm%cv_split) = (/ k4(10,1), k4(10,3), k4(10,2) , k4(10,9), k4(10,8), k4(10,7),k4(10,6),k4(10,5),k4(10,4),k4(10,10)/)

  if (size(defsvm%interp_weights, 1) /= 54) call erreur("SVM initialization", "bad array size") 

  case(svm_4kris)   ! 12 independent points 
 !Gauss point1 coefficients
  kk4(1,1) = 1.350186571632712
  kk4(1,2) =-0.004622267015403575
  kk4(1,3) =-0.03635534870087191
  kk4(1,4) =-0.1283957991451250
  kk4(1,5) = 0.02622895306569355
  kk4(1,6) =-0.01954704109687104
  kk4(1,7) =-0.04333476514151501
  kk4(1,8) = 0.1443498783387870
  kk4(1,9) =-0.3950927984532665
  kk4(1,10)= 0.1065826165158600
!Gauss point2 coefficients
  kk4(2,1) = 0.9835240410386652
  kk4(2,2) = 0.04102022837846748
  kk4(2,3) =-0.05586217796953953
  kk4(2,4) = 0.4610003455829734
  kk4(2,5) =-0.1673253860405332
  kk4(2,6) = 0.04572072750873149
  kk4(2,7) =-0.02468996556531046
  kk4(2,8) = 0.1957886235502523
  kk4(2,9) =-0.4250631209223152
  kk4(2,10)=-0.05411331556139172
!Gauss point3 coefficients
  kk4(3,1) = 0.3417745757471658
  kk4(3,2) = 0.05266376003579684
  kk4(3,3) =-0.09415065694535101
  kk4(3,4) = 1.346286436345379
  kk4(3,5) =-0.2756435522082158
  kk4(3,6) = 0.1206389269298530
  kk4(3,7) = 0.03320521491529624
  kk4(3,8) = 0.2754551295023642
  kk4(3,9) =-0.4306986344858194
  kk4(3,10)=-0.3695311998364683
!Gauss point4 coefficients
  kk4(4,1) =-0.2335641982401887
  kk4(4,2) =-0.1669322780991597
  kk4(4,3) =-0.1424549867673852
  kk4(4,4) = 1.436257218449623
  kk4(4,5) = 0.614599972828722
  kk4(4,6) =-0.0046714839361168
  kk4(4,7) = 0.1959882360123814
  kk4(4,8) = 0.2862184400973558
  kk4(4,9) =-0.2179819385402839
  kk4(4,10)=-0.7674589818049491
!Gauss point25 coefficients
  kk4(5,1) = 0.8288889984051868
  kk4(5,2) = 0.04677743023354147
  kk4(5,3) =-0.03991604821659466
  kk4(5,4) = 0.5446044655419009
  kk4(5,5) =-0.1890638174450713
  kk4(5,6) = 0.05059205267244342
  kk4(5,7) =-0.01233887391294805
  kk4(5,8) = 0.1383331792538543
  kk4(5,9) =-0.2904387454639018
  kk4(5,10)=-0.07743864106841123
!Gauss point26 coefficients
  kk4(6,1) = 0.7347191718430301
  kk4(6,2) = 0.03089206178978147
  kk4(6,3) = 0.009587120631267871
  kk4(6,4) = 0.2936886640989052
  kk4(6,5) =-0.1126383616346481
  kk4(6,6) = 0.02125598318219282
  kk4(6,7) = 0.005363405730222937
  kk4(6,8) =-0.03118350891894955
  kk4(6,9) = 0.07902781467646672
  kk4(6,10)=-0.03071235139826946
!Gauss point27 coefficients
  kk4(7,1) =-0.1845434719636343
  kk4(7,2) =-0.1845434719636343
  kk4(7,3) =-0.03907751185445954
  kk4(7,4) = 0.8137520094270351
  kk4(7,5) = 0.8137520094270351
  kk4(7,6) =-0.07987703593857617
  kk4(7,7) = 0.06772188147188028
  kk4(7,8) = 0.06772188147188028
  kk4(7,9) =-0.07987703593857617
  kk4(7,10)=-0.1950292541389505
!Gauss point28 coefficients
  kk4(8,1) =-0.2181559923787162
  kk4(8,2) =-0.2181559923787162
  kk4(8,3) =-0.1142031331798774
  kk4(8,4) = 0.9929893462207865
  kk4(8,5) = 0.9929893462207865
  kk4(8,6) =-0.09934845070431942
  kk4(8,7) = 0.1946263795788897
  kk4(8,8) = 0.1946263795788897
  kk4(8,9) =-0.09934845070431942
  kk4(8,10)=-0.6260194322534035
!Gauss point43 coefficients
  kk4(9,1) = 0.4171428636300509
  kk4(9,2) = 0.04469143885360190
  kk4(9,3) = 0.04469143885360187
  kk4(9,4) = 0.3809996057073503
  kk4(9,5) =-0.1494978760930465
  kk4(9,6) = 0.01247272805849735
  kk4(9,7) = 0.01247272805849725
  kk4(9,8) =-0.1494978760930462
  kk4(9,9) = 0.3809996057073512
  kk4(9,10)= 0.005525343317141464
!Gauss point44 coefficients
  kk4(10,1) =-0.03372290429346719
  kk4(10,2) = 0.08422845078964551
  kk4(10,3) = 0.08422845078964559
  kk4(10,4) = 0.5279195360563089
  kk4(10,5) =-0.2373417331142653
  kk4(10,6) =-0.05641319088329125
  kk4(10,7) =-0.05641319088329131
  kk4(10,8) =-0.2373417331142654
  kk4(10,9) = 0.5279195360563111
  kk4(10,10)= 0.3969367785966691
!Gauss point49 coefficients
  kk4(11,1) =-0.1324880465205211
  kk4(11,2) = 0.06839911609813847
  kk4(11,3) = 0.09438486033397265
  kk4(11,4) = 0.6574368789079342
  kk4(11,5) =-0.1856491158592828
  kk4(11,6) =-0.09050328282806565
  kk4(11,7) =-0.1202638750179787
  kk4(11,8) =-0.2214388485701374
  kk4(11,9) = 0.2498587299410721
  kk4(11,10)= 0.6802635835148674
!Gauss point50 coefficients
  kk4(12,1) =-0.1717935614576200
  kk4(12,2) =-0.1071850079229601
  kk4(12,3) = 0.03092823558004528
  kk4(12,4) = 0.7957181895895807
  kk4(12,5) = 0.4221111503732683
  kk4(12,6) =-0.05725763709499984
  kk4(12,7) =-0.05361615040959700
  kk4(12,8) =-0.05487803385310206
  kk4(12,9) =-0.06065292890941012
  kk4(12,10)= 0.2566257441047929

  defsvm%interp_weights(1, 1:defsvm%cv_split)  = (/ kk4(1,1), kk4(1,2), kk4(1,3) , kk4(1,4), kk4(1,5), kk4(1,6),kk4(1,7),kk4(1,8),kk4(1,9),kk4(1,10)/)!
  defsvm%interp_weights(8, 1:defsvm%cv_split)  = (/ kk4(1,2), kk4(1,1), kk4(1,3) , kk4(1,5), kk4(1,4), kk4(1,9),kk4(1,8),kk4(1,7),kk4(1,6),kk4(1,10)/)
  defsvm%interp_weights(9, 1:defsvm%cv_split)  = (/ kk4(1,3), kk4(1,1), kk4(1,2) , kk4(1,8), kk4(1,9), kk4(1,4),kk4(1,5),kk4(1,6),kk4(1,7),kk4(1,10)/)
  defsvm%interp_weights(16, 1:defsvm%cv_split) = (/ kk4(1,3), kk4(1,2), kk4(1,1) , kk4(1,7), kk4(1,6), kk4(1,5),kk4(1,4),kk4(1,9),kk4(1,8),kk4(1,10)/)
  defsvm%interp_weights(17, 1:defsvm%cv_split) = (/ kk4(1,2), kk4(1,3), kk4(1,1) , kk4(1,6), kk4(1,7), kk4(1,8),kk4(1,9),kk4(1,4),kk4(1,5),kk4(1,10)/)
  defsvm%interp_weights(24, 1:defsvm%cv_split) = (/ kk4(1,1), kk4(1,3), kk4(1,2) , kk4(1,9), kk4(1,8), kk4(1,7),kk4(1,6),kk4(1,5),kk4(1,4),kk4(1,10)/)

  defsvm%interp_weights(2, 1:defsvm%cv_split)  = (/ kk4(2,1), kk4(2,2), kk4(2,3) , kk4(2,4), kk4(2,5), kk4(2,6),kk4(2,7),kk4(2,8),kk4(2,9),kk4(2,10)/)!
  defsvm%interp_weights(7, 1:defsvm%cv_split)  = (/ kk4(2,2), kk4(2,1), kk4(2,3) , kk4(2,5), kk4(2,4), kk4(2,9),kk4(2,8),kk4(2,7),kk4(2,6),kk4(2,10)/)
  defsvm%interp_weights(10, 1:defsvm%cv_split) = (/ kk4(2,3), kk4(2,1), kk4(2,2) , kk4(2,8), kk4(2,9), kk4(2,4),kk4(2,5),kk4(2,6),kk4(2,7),kk4(2,10)/)
  defsvm%interp_weights(15, 1:defsvm%cv_split) = (/ kk4(2,3), kk4(2,2), kk4(2,1) , kk4(2,7), kk4(2,6), kk4(2,5),kk4(2,4),kk4(2,9),kk4(2,8),kk4(2,10)/)
  defsvm%interp_weights(18, 1:defsvm%cv_split) = (/ kk4(2,2), kk4(2,3), kk4(2,1) , kk4(2,6), kk4(2,7), kk4(2,8),kk4(2,9),kk4(2,4),kk4(2,5),kk4(2,10)/)
  defsvm%interp_weights(23, 1:defsvm%cv_split) = (/ kk4(2,1), kk4(2,3), kk4(2,2) , kk4(2,9), kk4(2,8), kk4(2,7),kk4(2,6),kk4(2,5),kk4(2,4),kk4(2,10)/)

  defsvm%interp_weights(3, 1:defsvm%cv_split)  = (/ kk4(3,1), kk4(3,2), kk4(3,3) , kk4(3,4), kk4(3,5), kk4(3,6),kk4(3,7),kk4(3,8),kk4(3,9),kk4(3,10)/)!
  defsvm%interp_weights(6, 1:defsvm%cv_split)  = (/ kk4(3,2), kk4(3,1), kk4(3,3) , kk4(3,5), kk4(3,4), kk4(3,9),kk4(3,8),kk4(3,7),kk4(3,6),kk4(3,10)/)
  defsvm%interp_weights(11, 1:defsvm%cv_split) = (/ kk4(3,3), kk4(3,1), kk4(3,2) , kk4(3,8), kk4(3,9), kk4(3,4),kk4(3,5),kk4(3,6),kk4(3,7),kk4(3,10)/)
  defsvm%interp_weights(14, 1:defsvm%cv_split) = (/ kk4(3,3), kk4(3,2), kk4(3,1) , kk4(3,7), kk4(3,6), kk4(3,5),kk4(3,4),kk4(3,9),kk4(3,8),kk4(3,10)/)
  defsvm%interp_weights(19, 1:defsvm%cv_split) = (/ kk4(3,2), kk4(3,3), kk4(3,1) , kk4(3,6), kk4(3,7), kk4(3,8),kk4(3,9),kk4(3,4),kk4(3,5),kk4(3,10)/)
  defsvm%interp_weights(22, 1:defsvm%cv_split) = (/ kk4(3,1), kk4(3,3), kk4(3,2) , kk4(3,9), kk4(3,8), kk4(3,7),kk4(3,6),kk4(3,5),kk4(3,4),kk4(3,10)/)

  defsvm%interp_weights(4, 1:defsvm%cv_split)  = (/ kk4(4,1), kk4(4,2), kk4(4,3) , kk4(4,4), kk4(4,5), kk4(4,6),kk4(4,7),kk4(4,8),kk4(4,9),kk4(4,10)/)!
  defsvm%interp_weights(5, 1:defsvm%cv_split)  = (/ kk4(4,2), kk4(4,1), kk4(4,3) , kk4(4,5), kk4(4,4), kk4(4,9),kk4(4,8),kk4(4,7),kk4(4,6),kk4(4,10)/)
  defsvm%interp_weights(12, 1:defsvm%cv_split) = (/ kk4(4,3), kk4(4,1), kk4(4,2) , kk4(4,8), kk4(4,9), kk4(4,4),kk4(4,5),kk4(4,6),kk4(4,7),kk4(4,10)/)
  defsvm%interp_weights(13, 1:defsvm%cv_split) = (/ kk4(4,3), kk4(4,2), kk4(4,1) , kk4(4,7), kk4(4,6), kk4(4,5),kk4(4,4),kk4(4,9),kk4(4,8),kk4(4,10)/)
  defsvm%interp_weights(20, 1:defsvm%cv_split) = (/ kk4(4,2), kk4(4,3), kk4(4,1) , kk4(4,6), kk4(4,7), kk4(4,8),kk4(4,9),kk4(4,4),kk4(4,5),kk4(4,10)/)
  defsvm%interp_weights(21, 1:defsvm%cv_split) = (/ kk4(4,1), kk4(4,3), kk4(4,2) , kk4(4,9), kk4(4,8), kk4(4,7),kk4(4,6),kk4(4,5),kk4(4,4),kk4(4,10)/)

  defsvm%interp_weights(25, 1:defsvm%cv_split) = (/ kk4(5,1), kk4(5,2), kk4(5,3) , kk4(5,4), kk4(5,5), kk4(5,6),kk4(5,7),kk4(5,8),kk4(5,9),kk4(5,10)/)!
  defsvm%interp_weights(29, 1:defsvm%cv_split) = (/ kk4(5,2), kk4(5,1), kk4(5,3) , kk4(5,5), kk4(5,4), kk4(5,9),kk4(5,8),kk4(5,7),kk4(5,6),kk4(5,10)/)
  defsvm%interp_weights(31, 1:defsvm%cv_split) = (/ kk4(5,3), kk4(5,1), kk4(5,2) , kk4(5,8), kk4(5,9), kk4(5,4),kk4(5,5),kk4(5,6),kk4(5,7),kk4(5,10)/)
  defsvm%interp_weights(35, 1:defsvm%cv_split) = (/ kk4(5,3), kk4(5,2), kk4(5,1) , kk4(5,7), kk4(5,6), kk4(5,5),kk4(5,4),kk4(5,9),kk4(5,8),kk4(5,10)/)
  defsvm%interp_weights(37, 1:defsvm%cv_split) = (/ kk4(5,2), kk4(5,3), kk4(5,1) , kk4(5,6), kk4(5,7), kk4(5,8),kk4(5,9),kk4(5,4),kk4(5,5),kk4(5,10)/)
  defsvm%interp_weights(41, 1:defsvm%cv_split) = (/ kk4(5,1), kk4(5,3), kk4(5,2) , kk4(5,9), kk4(5,8), kk4(5,7),kk4(5,6),kk4(5,5),kk4(5,4),kk4(5,10)/)

  defsvm%interp_weights(26, 1:defsvm%cv_split) = (/ kk4(6,1), kk4(6,2), kk4(6,3) , kk4(6,4), kk4(6,5), kk4(6,6),kk4(6,7),kk4(6,8),kk4(6,9),kk4(6,10)/)!
  defsvm%interp_weights(30, 1:defsvm%cv_split) = (/ kk4(6,2), kk4(6,1), kk4(6,3) , kk4(6,5), kk4(6,4), kk4(6,9),kk4(6,8),kk4(6,7),kk4(6,6),kk4(6,10)/)
  defsvm%interp_weights(32, 1:defsvm%cv_split) = (/ kk4(6,3), kk4(6,1), kk4(6,2) , kk4(6,8), kk4(6,9), kk4(6,4),kk4(6,5),kk4(6,6),kk4(6,7),kk4(6,10)/)
  defsvm%interp_weights(36, 1:defsvm%cv_split) = (/ kk4(6,3), kk4(6,2), kk4(6,1) , kk4(6,7), kk4(6,6), kk4(6,5),kk4(6,4),kk4(6,9),kk4(6,8),kk4(6,10)/)
  defsvm%interp_weights(38, 1:defsvm%cv_split) = (/ kk4(6,2), kk4(6,3), kk4(6,1) , kk4(6,6), kk4(6,7), kk4(6,8),kk4(6,9),kk4(6,4),kk4(6,5),kk4(6,10)/)
  defsvm%interp_weights(42, 1:defsvm%cv_split) = (/ kk4(6,1), kk4(6,3), kk4(6,2) , kk4(6,9), kk4(6,8), kk4(6,7),kk4(6,6),kk4(6,5),kk4(6,4),kk4(6,10)/)

  defsvm%interp_weights(27, 1:defsvm%cv_split) = (/ kk4(7,1), kk4(7,2), kk4(7,3) , kk4(7,4), kk4(7,5), kk4(7,6),kk4(7,7),kk4(7,8),kk4(7,9),kk4(7,10)/)!
  defsvm%interp_weights(33, 1:defsvm%cv_split) = (/ kk4(7,3), kk4(7,1), kk4(7,2) , kk4(7,8), kk4(7,9), kk4(7,4),kk4(7,5),kk4(7,6),kk4(7,7),kk4(7,10)/)
  defsvm%interp_weights(39, 1:defsvm%cv_split) = (/ kk4(7,1), kk4(7,3), kk4(7,2) , kk4(7,9), kk4(7,8), kk4(7,7),kk4(7,6),kk4(7,5),kk4(7,4),kk4(7,10)/)

  defsvm%interp_weights(28, 1:defsvm%cv_split) = (/ kk4(8,1), kk4(8,2), kk4(8,3) , kk4(8,4), kk4(8,5), kk4(8,6),kk4(8,7),kk4(8,8),kk4(8,9),kk4(8,10)/)!
  defsvm%interp_weights(34, 1:defsvm%cv_split) = (/ kk4(8,3), kk4(8,1), kk4(8,2) , kk4(8,8), kk4(8,9), kk4(8,4),kk4(8,5),kk4(8,6),kk4(8,7),kk4(8,10)/)
  defsvm%interp_weights(40, 1:defsvm%cv_split) = (/ kk4(8,1), kk4(8,3), kk4(8,2) , kk4(8,9), kk4(8,8), kk4(8,7),kk4(8,6),kk4(8,5),kk4(8,4),kk4(8,10)/)


  defsvm%interp_weights(43, 1:defsvm%cv_split) = (/ kk4(9,1), kk4(9,2), kk4(9,3) , kk4(9,4), kk4(9,5), kk4(9,6),kk4(9,7),kk4(9,8),kk4(9,9),kk4(9,10)/)!
  defsvm%interp_weights(45, 1:defsvm%cv_split) = (/ kk4(9,3), kk4(9,1), kk4(9,2) , kk4(9,8), kk4(9,9), kk4(9,4),kk4(9,5),kk4(9,6),kk4(9,7),kk4(9,10)/)
  defsvm%interp_weights(47, 1:defsvm%cv_split) = (/ kk4(9,2), kk4(9,3), kk4(9,1) , kk4(9,6), kk4(9,7), kk4(9,8),kk4(9,9),kk4(9,4),kk4(9,5),kk4(9,10)/)

  defsvm%interp_weights(44, 1:defsvm%cv_split) = (/ kk4(10,1), kk4(10,2), kk4(10,3) , kk4(10,4), kk4(10,5), kk4(10,6),kk4(10,7),kk4(10,8),kk4(10,9),kk4(10,10)/)!
  defsvm%interp_weights(46, 1:defsvm%cv_split) = (/ kk4(10,3), kk4(10,1), kk4(10,2) , kk4(10,8), kk4(10,9), kk4(10,4),kk4(10,5),kk4(10,6),kk4(10,7),kk4(10,10)/)
  defsvm%interp_weights(48, 1:defsvm%cv_split) = (/ kk4(10,2), kk4(10,3), kk4(10,1) , kk4(10,6), kk4(10,7), kk4(10,8),kk4(10,9),kk4(10,4),kk4(10,5),kk4(10,10)/)

  defsvm%interp_weights(49, 1:defsvm%cv_split) = (/ kk4(11,1), kk4(11,2), kk4(11,3) , kk4(11,4), kk4(11,5), kk4(11,6),kk4(11,7),kk4(11,8),kk4(11,9),kk4(11,10)/)!
  defsvm%interp_weights(52, 1:defsvm%cv_split) = (/ kk4(11,2), kk4(11,1), kk4(11,3) , kk4(11,5), kk4(11,4), kk4(11,9),kk4(11,8),kk4(11,7),kk4(11,6),kk4(11,10)/)
  defsvm%interp_weights(53, 1:defsvm%cv_split) = (/ kk4(11,3), kk4(11,1), kk4(11,2) , kk4(11,8), kk4(11,9), kk4(11,4),kk4(11,5),kk4(11,6),kk4(11,7),kk4(11,10)/)
  defsvm%interp_weights(56, 1:defsvm%cv_split) = (/ kk4(11,3), kk4(11,2), kk4(11,1) , kk4(11,7), kk4(11,6), kk4(11,5),kk4(11,4),kk4(11,9),kk4(11,8),kk4(11,10)/)
  defsvm%interp_weights(57, 1:defsvm%cv_split) = (/ kk4(11,2), kk4(11,3), kk4(11,1) , kk4(11,6), kk4(11,7), kk4(11,8),kk4(11,9),kk4(11,4),kk4(11,5),kk4(11,10)/)
  defsvm%interp_weights(60, 1:defsvm%cv_split) = (/ kk4(11,1), kk4(11,3), kk4(11,2) , kk4(11,9), kk4(11,8), kk4(11,7),kk4(11,6),kk4(11,5),kk4(11,4),kk4(11,10)/)


  defsvm%interp_weights(50, 1:defsvm%cv_split) = (/ kk4(12,1), kk4(12,2), kk4(12,3) , kk4(12,4), kk4(12,5), kk4(12,6),kk4(12,7),kk4(12,8),kk4(12,9),kk4(12,10)/)!
  defsvm%interp_weights(51, 1:defsvm%cv_split) = (/ kk4(12,2), kk4(12,1), kk4(12,3) , kk4(12,5), kk4(12,4), kk4(12,9),kk4(12,8),kk4(12,7),kk4(12,6),kk4(12,10)/)
  defsvm%interp_weights(54, 1:defsvm%cv_split) = (/ kk4(12,3), kk4(12,1), kk4(12,2) , kk4(12,8), kk4(12,9), kk4(12,4),kk4(12,5),kk4(12,6),kk4(12,7),kk4(12,10)/)
  defsvm%interp_weights(55, 1:defsvm%cv_split) = (/ kk4(12,3), kk4(12,2), kk4(12,1) , kk4(12,7), kk4(12,6), kk4(12,5),kk4(12,4),kk4(12,9),kk4(12,8),kk4(12,10)/)
  defsvm%interp_weights(58, 1:defsvm%cv_split) = (/ kk4(12,2), kk4(12,3), kk4(12,1) , kk4(12,6), kk4(12,7), kk4(12,8),kk4(12,9),kk4(12,4),kk4(12,5),kk4(12,10)/)
  defsvm%interp_weights(59, 1:defsvm%cv_split) = (/ kk4(12,1), kk4(12,3), kk4(12,2) , kk4(12,9), kk4(12,8), kk4(12,7),kk4(12,6),kk4(12,5),kk4(12,4),kk4(12,10)/)

  if (size(defsvm%interp_weights, 1) /= 60) call erreur("SVM initialization", "bad array size")

  case(svm_4kris2)   ! 12 independent points ... to be continued ...
  call erreur("parameters parsing","unknown SVM method (init_svmweights)")

endselect

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
