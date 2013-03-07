!------------------------------------------------------------------------------!
! MODULE : MENU_NUM                                 Authors : J. Gressier
!                                                   Created : May 2002
! Fonction
!   Definiti on des structures pour les entrees du programme TYPHON
!   Structures pour les options numeriques
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

module MENU_NUM

use TYPHMAKE   ! Definition de la precision
use VARCOM
use OUTPUT
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
integer(kpp), parameter :: sch_rusanov    = 05
integer(kpp), parameter :: sch_roe        = 10
integer(kpp), parameter :: sch_osher_no   = 15
integer(kpp), parameter :: sch_osher_io   = 16
integer(kpp), parameter :: sch_hllr       = 20
integer(kpp), parameter :: sch_hlle       = 21
integer(kpp), parameter :: sch_hlleb      = 22
integer(kpp), parameter :: sch_hllek      = 23
integer(kpp), parameter :: sch_hllekb     = 24
integer(kpp), parameter :: sch_hllc       = 25
integer(kpp), parameter :: sch_hllcb      = 26
integer(kpp), parameter :: sch_hllck      = 27
integer(kpp), parameter :: sch_hllckb     = 28
integer(kpp), parameter :: sch_stegwarm   = 30
integer(kpp), parameter :: sch_vanleer    = 31
integer(kpp), parameter :: sch_efm        = 35
integer(kpp), parameter :: sch_wps_vleer  = 40
integer(kpp), parameter :: sch_wps_efm    = 41
integer(kpp), parameter :: sch_hwps_vleer = 50   ! or VLEER-H (Hanel variant)
integer(kpp), parameter :: sch_hwps_efm   = 51   ! or   EFM-H (Hanel variant)
integer(kpp), parameter :: sch_efmo       = 60
integer(kpp), parameter :: sch_ausmm      = 70

! -- Constants for jacobian expression of flux (jac_hyp) --
integer(kpp), parameter :: jac_diffnum = 01
integer(kpp), parameter :: jac_hll     = 10
integer(kpp), parameter :: jac_hlldiag = 15
integer(kpp), parameter :: jac_rusanov = 16
integer(kpp), parameter :: jac_efm     = 20
integer(kpp), parameter :: jac_vlh     = 25


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
character, parameter :: lim_lim03     = 'L'
character, parameter :: lim_minmax    = 'B'

! -- Constants for post-limitation --
integer(kpp), parameter :: postlim_none        = 0
integer(kpp), parameter :: postlim_monotonic0  = 10
integer(kpp), parameter :: postlim_monotonic1  = 11
integer(kpp), parameter :: postlim_monotonic2  = 12
integer(kpp), parameter :: postlim_barth       = 20
integer(kpp), parameter :: postlim_superbarth  = 21

! -- Constants for SVM methods
integer(kpp), parameter :: svm_2tri    = 21
integer(kpp), parameter :: svm_2quad   = 22
integer(kpp), parameter :: svm_2q2x2b3 = 23
integer(kpp), parameter :: svm_2q2x2b4 = 24
integer(kpp), parameter :: svm_3wang   = 31
integer(kpp), parameter :: svm_3kris   = 32
integer(kpp), parameter :: svm_3kris2  = 33
integer(kpp), parameter :: svm_3q3x3b6 = 35
integer(kpp), parameter :: svm_4wang   = 41
integer(kpp), parameter :: svm_4kris   = 42
integer(kpp), parameter :: svm_4kris2  = 43

! -- Constants for SVM fluxes computation
integer(kpp), parameter :: svm_fluxQ    = 31
integer(kpp), parameter :: svm_fluxF    = 32

! -- Constantes pour schema de calcul des flux dissipatifs (sch_dis)
integer(kpp), parameter :: dis_noflux   = 1         ! no dissipative flux
integer(kpp), parameter :: dis_celldif2 = 2         ! difference des 2 etats/face (NON CONSISTANT)
integer(kpp), parameter :: dis_cellavg2 = 5         ! moyenne des 2 gradients/face
integer(kpp), parameter :: dis_cellfull = 10        ! evaluation complete (ponderee de 1 et 5)
integer(kpp), parameter :: dis_facecentered = 20    ! average of gradients
integer(kpp), parameter :: dis_facepenalty  = 22    ! Penalty method

! -- Constants for gradients construction method (gradmeth)
integer(kpp), parameter :: gradnone              = 5     ! no gradient computation
integer(kpp), parameter :: cellgrad_lsq          = 10    ! least square method
integer(kpp), parameter :: cellgrad_lsqw         = 12    ! weighted least square method
integer(kpp), parameter :: cellgrad_compactgauss = 20    ! Gauss Theorem
integer(kpp), parameter :: facegrad_svm          = 30    ! direct SVM Gradient evaluation

! -- Constants for implicit resolution method
integer(kpp), parameter :: alg_undef     =  -1
integer(kpp), parameter :: alg_lu        =  10  ! direct LU
integer(kpp), parameter :: alg_cho       =  15  ! direct Choleski decomposition (SYM)
integer(kpp), parameter :: alg_jac       =  20  ! iterative Jacobi
integer(kpp), parameter :: alg_gs        =  25  ! iterative Gauss-Seidel
integer(kpp), parameter :: alg_sor       =  26  ! iterative Gauss-Seidel with OverRelaxation
integer(kpp), parameter :: alg_gmres     =  40  ! Generalized Minimal Residual (GMRes)
integer(kpp), parameter :: alg_bicg      =  60  ! Bi-Conjugate Gradient
integer(kpp), parameter :: alg_bicgpjac  =  61  ! Bi-Conjugate Gradient (Jacobi Preconditioned)
integer(kpp), parameter :: alg_cgs       =  62  ! Conjugate Gradient Squared
integer(kpp), parameter :: alg_bicgstab  =  70  ! Bi-Conjugate Gradient Stabilized
integer(kpp), parameter :: alg_gmresfree = 140  ! matrix-free Generalized Minimal Residual (GMRES)


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
  integer(kip)    :: max_it       ! maximal number of iterations
  integer(kip)    :: nkrylov      ! number of searching directions (Krylov vectors) for GMRES-like methods
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
  integer(kip)    :: maxit      ! max number of iteration
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
  integer(kpp)      :: sv_method                   ! SV method / partition (cf constants)
  integer(kpp)      :: sv_flux                     ! Type of flux integration on gauss points for SVM method
  integer(kip)      :: ncv                         ! number of Control Volume (CV subcell) in a SV
  integer(kip)      :: ncvface                     ! number of sub faces      (by cell) (internal + original*split)  
  integer(kip)      :: nfgauss                     ! number of integration points by face
  real(krp),pointer :: interp_weights(:,:)         ! weights for cell to face Gauss points interpolation
  real(krp),pointer :: grad_interp_weights(:,:,:)  ! weights for gradients evaluation on Gauss points

endtype mnu_svm

!------------------------------------------------------------------------------!
! structure MNU_SPAT : Options for numerical spatial parameters
!------------------------------------------------------------------------------!
type mnu_spat
  integer(kpp)    :: sch_hyp       ! Numerical scheme for hyperbolic part (waves)
  integer(kpp)    :: jac_hyp       ! Jacobian Model for hyperbolic fluxes
  integer(kpp)    :: sch_dis       ! Numerical scheme for diffusive part
  character       :: method        ! State extrapolation method (M)USCL, (E)NO ...
  integer(kpp)    :: gradmeth      ! Numerical method for GRADIENT computation
  integer(kpp)    :: postlimiter   ! Limitation method after reconstruction/extrapolation
  logical         :: calc_cellgrad ! internal: needs gradient computation
  logical         :: calc_facegrad ! internal: needs gradient computation
  logical         :: calc_hresQ    ! internal: needs high order extrapolation
  type(mnu_muscl) :: muscl         ! specific parameters for MUSCL method
  type(mnu_svm)   :: svm           ! specific parameters for SVM   method
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

select case(defspat%method)
case(hres_none)
  call rpmgetkeyvalstr(pcour, keyword, str, "FULL") 
case(hres_muscl, hres_muscluns,hres_musclfast)
  call rpmgetkeyvalstr(pcour, keyword, str, "FULL")
case(hres_svm)
  call rpmgetkeyvalstr(pcour, keyword, str, "FACE-AVERAGE")
case default
  call error_stop("internal error: unknown extrapolation method (get_dissipmethod)")
endselect

defspat%sch_dis = inull

if (samestring(str,"COMPACT"))      defspat%sch_dis = dis_celldif2
if (samestring(str,"CELL-AVERAGE")) defspat%sch_dis = dis_cellavg2
if (samestring(str,"FULL"))         defspat%sch_dis = dis_cellfull
if (samestring(str,"PENALTY"))      defspat%sch_dis = dis_facepenalty
if (samestring(str,"FACE-AVERAGE")) defspat%sch_dis = dis_facecentered

if (defspat%sch_dis == inull) &
     call error_stop("parameters parsing: unknown DISSIPATIVE_FLUX method: "//trim(str))

call get_gradientmethod(pcour, defspat)

select case(defspat%sch_dis)
case(dis_celldif2)
  ! no gradient needed
case(dis_cellavg2, dis_cellfull)
  if (.not.defspat%calc_cellgrad) call error_stop("inconsistent choice of GRADMETH and DISSIPATIVE_FLUX")
case(dis_facecentered, dis_facepenalty)
  if (.not.defspat%calc_facegrad) call error_stop("inconsistent choice of GRADMETH and DISSIPATIVE_FLUX")
case default
  call error_stop("internal error: unknown dissipative flux scheme (get_dissipmethod)")
endselect

endsubroutine get_dissipmethod


!-------------------------------------------------------------------------
! get method for gradient computation
!-------------------------------------------------------------------------
subroutine get_gradientmethod(pcour, defspat)
implicit none
type(rpmblock), pointer       :: pcour  ! pointeur de bloc RPM
type(mnu_spat)                :: defspat
character(len=dimrpmlig)      :: str, defmeth       ! chaine RPM intermediaire

select case(defspat%method)
case(hres_none)
  select case(defspat%sch_dis)
  case(dis_noflux, dis_celldif2)
    defmeth = 'NONE'
  case(dis_cellfull, dis_cellavg2)
    defmeth = 'LSQ'
  case(dis_facecentered, dis_facepenalty)
    call error_stop("inconsistent choice: face based gradient unavailable for 1st order or MUSCL-fast methods")
  case default
    call error_stop("internal error: unknown dissipative flux method (get_gradientmethod)")
  endselect

case(hres_muscl, hres_muscluns, hres_musclfast)
  select case(defspat%sch_dis)
  case(dis_noflux, dis_celldif2, dis_cellfull, dis_cellavg2)
    defmeth = 'LSQ'
  case(dis_facecentered, dis_facepenalty)
    call error_stop("inconsistent choice: face based gradient unavailable for MUSCL methods")
  case default
    call error_stop("internal error: unknown dissipative flux method (get_gradientmethod)")
  endselect

case(hres_svm)
  select case(defspat%sch_dis)
  case(dis_noflux, dis_celldif2)
    defmeth = 'NONE'
  case(dis_cellfull, dis_cellavg2)
    defmeth = 'COMPACT-GAUSS'
  case(dis_facecentered, dis_facepenalty)
    defmeth = 'FACE-SVM'
  case default
    call error_stop("internal error: unknown dissipative flux method (get_gradientmethod)")
  endselect

case default
  call error_stop("internal error: unknown extrapolation method (get_gradientmethod)")
endselect

call rpmgetkeyvalstr(pcour, "GRADMETH", str, trim(defmeth))

defspat%gradmeth = inull

if (samestring(str,"NONE"))          defspat%gradmeth = gradnone
if (samestring(str,"LSQ"))           defspat%gradmeth = cellgrad_lsq
if (samestring(str,"W-LSQ"))         defspat%gradmeth = cellgrad_lsqw
if (samestring(str,"COMPACT-GAUSS")) defspat%gradmeth = cellgrad_compactgauss
if (samestring(str,"FACE-SVM"))      defspat%gradmeth = facegrad_svm

if (defspat%gradmeth == inull) &
     call error_stop("parameters parsing: unknown GRADIENT computation method: "//trim(str))

select case(defspat%gradmeth)
case(gradnone)
case(cellgrad_lsq, cellgrad_lsqw, cellgrad_compactgauss)
  defspat%calc_cellgrad = .true.
case(facegrad_svm)
  defspat%calc_facegrad = .true.
case default
  call error_stop("internal error: unknown gradient method (get_gradientmethod)")
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
  call error_stop("parameters parsing: unknown RUNGE KUTTA method")
endselect

endsubroutine init_rungekutta

!-------------------------------------------------------------------------
! init SVM weights
!-------------------------------------------------------------------------
subroutine init_svmweights(defsvm)
implicit none
! -- parameters --
type(mnu_svm) :: defsvm
! -- internal --
real(krp)     :: sqrt3
real(krp)     :: k1(1:5)
real(krp), dimension(1:7,1:6) :: k
real(krp), dimension(1:10,1:10) :: k4
real(krp), dimension(1:12,1:10) :: kk4

! -- body --

sqrt3 = sqrt(3._krp)

select case(defsvm%sv_method)
case(svm_2quad)
  defsvm%ncv     = 3
  defsvm%ncvface = 9
  defsvm%nfgauss = 1
  allocate(defsvm%interp_weights(defsvm%ncvface*defsvm%nfgauss, defsvm%ncv))
  if (size(defsvm%interp_weights, 1) /= 9) call error_stop("SVM initialization: bad total number Gauss points")
  k1(1) =  4._krp /  3._krp
  k1(2) =  2._krp / 15._krp
  k1(3) = -7._krp / 15._krp
  k1(4) = -1._krp / 15._krp
  k1(5) =  8._krp / 15._krp
  defsvm%interp_weights(1, 1:defsvm%ncv) = (/ k1(4), k1(5), k1(5) /)
  defsvm%interp_weights(2, 1:defsvm%ncv) = (/ k1(5), k1(4), k1(5) /)
  defsvm%interp_weights(3, 1:defsvm%ncv) = (/ k1(5), k1(5), k1(4) /)
  defsvm%interp_weights(4, 1:defsvm%ncv) = (/ k1(1), k1(3), k1(2) /)
  defsvm%interp_weights(5, 1:defsvm%ncv) = (/ k1(1), k1(2), k1(3) /)
  defsvm%interp_weights(6, 1:defsvm%ncv) = (/ k1(2), k1(1), k1(3) /)
  defsvm%interp_weights(7, 1:defsvm%ncv) = (/ k1(3), k1(1), k1(2) /)
  defsvm%interp_weights(8, 1:defsvm%ncv) = (/ k1(3), k1(2), k1(1) /)
  defsvm%interp_weights(9, 1:defsvm%ncv) = (/ k1(2), k1(3), k1(1) /)

case(svm_2q2x2b3)
  defsvm%ncv     = 4
  defsvm%ncvface = 12
  defsvm%nfgauss = 1
  allocate(defsvm%interp_weights(defsvm%ncvface*defsvm%nfgauss, defsvm%ncv))
  if (size(defsvm%interp_weights, 1) /= 12) call error_stop("SVM initialization: bad total number Gauss points")
  call error_stop("DEV: not implemented yet")

case(svm_2q2x2b4)
  defsvm%ncv     = 4
  defsvm%ncvface = 12
  defsvm%nfgauss = 1
  allocate(defsvm%interp_weights(defsvm%ncvface*defsvm%nfgauss, defsvm%ncv))

  if (size(defsvm%interp_weights, 1) /= 12) call error_stop("SVM initialization: bad total number Gauss points")
  call error_stop("DEV: not implemented yet")

case(svm_3wang) !weights for alpha=1/4 and beta =1/3 : ORIGINAL PARTITION BY WANG
  defsvm%ncv     = 6
  defsvm%ncvface = 18
  defsvm%nfgauss = 2
  allocate(defsvm%interp_weights(defsvm%ncvface*defsvm%nfgauss, defsvm%ncv))

  ! 36 Gauss pts, 6 coeff per point, 7 independent points

  !Gauss point1
  k(1,1) =  5353._krp /  4017._krp + 2287._krp / 10712._krp * sqrt3  !  1.702377414448907
  k(1,2) =  -263._krp /  4017._krp +  623._krp / 10712._krp * sqrt3  !  0.0352627258945180
  k(1,3) =   781._krp /  4017._krp -  129._krp / 10712._krp * sqrt3  !  0.1735653577754183
  k(1,4) =   346._krp /  1339._krp - 3375._krp / 10712._krp * sqrt3  ! -0.2873106306520688
  k(1,5) =    89._krp /  2678._krp +  765._krp / 10712._krp * sqrt3  !  0.1569285724225346
  k(1,6) = -2017._krp /  2678._krp -  171._krp / 10712._krp * sqrt3  ! -0.7808234398893090
!Gauss point2 coefficients
  k(2,1) =  5353._krp /  4017._krp - 2287._krp / 10712._krp * sqrt3  !  0.9627956002386711
  k(2,2) =  -263._krp /  4017._krp -  623._krp / 10712._krp * sqrt3  ! -0.1662062160613092
  k(2,3) =   781._krp /  4017._krp +  129._krp / 10712._krp * sqrt3  !  0.2152820407807181
  k(2,4) =   346._krp /  1339._krp + 3375._krp / 10712._krp * sqrt3  !  0.8041142154168186
  k(2,5) =    89._krp /  2678._krp -  765._krp / 10712._krp * sqrt3  ! -0.09046105935307983
  k(2,6) = -2017._krp /  2678._krp +  171._krp / 10712._krp * sqrt3  ! -0.7255245810218187
!Gauss point3 coefficients
  k(3,1) =   725._krp /  8034._krp +   16._krp /   103._krp * sqrt3  !  0.3592979098638236
  k(3,2) =   725._krp /  8034._krp -   16._krp /   103._krp * sqrt3  ! -0.1788149623905849
  k(3,3) =  1949._krp /  8034._krp                                   !  0.2425939756036843
  k(3,4) =  4067._krp /  2678._krp                                   !  1.518670649738611
  k(3,5) =   -97._krp /   206._krp +    9._krp /   103._krp * sqrt3  ! -0.3195295410862146
  k(3,6) =   -97._krp /   206._krp -    9._krp /   103._krp * sqrt3  ! -0.6222180317293194
 !Gauss point19 coefficients
  k(4,1) =  9407._krp / 16068._krp + 1583._krp / 32136._krp * sqrt3  !  0.6707691196285018
  k(4,2) = -2401._krp / 16068._krp -  529._krp / 32136._krp * sqrt3  ! -0.1779392232139637
  k(4,3) =   719._krp / 16068._krp + 1727._krp / 32136._krp * sqrt3  !  0.1378283465481532
  k(4,4) =  3875._krp /  5356._krp + 1283._krp / 10712._krp * sqrt3  !  0.9309392444091551
  k(4,5) =  -367._krp /  5356._krp -  289._krp / 10712._krp * sqrt3  ! -0.1152504372094292
  k(4,6) =  -727._krp /  5356._krp - 1921._krp / 10712._krp * sqrt3  ! -0.4463470501624172
!Gauss point20 coefficients
  k(5,1) =  9407._krp / 16068._krp - 1583._krp / 32136._krp * sqrt3  !  0.5001295609789167
  k(5,2) = -2401._krp / 16068._krp +  529._krp / 32136._krp * sqrt3  ! -0.1209156436020682
  k(5,3) =   719._krp / 16068._krp - 1727._krp / 32136._krp * sqrt3  ! -0.04833369880107822
  k(5,4) =  3875._krp /  5356._krp - 1283._krp / 10712._krp * sqrt3  !  0.5160361103331899
  k(5,5) =  -367._krp /  5356._krp +  289._krp / 10712._krp * sqrt3  ! -0.02179213187197484
  k(5,6) =  -727._krp /  5356._krp + 1921._krp / 10712._krp * sqrt3  !  0.1748758029630147
!Gauss point31 coefficients
  k(6,1) =  1043._krp / 16068._krp +  263._krp /  2678._krp * sqrt3  !  0.2350122090081957
  k(6,2) = -2221._krp / 16068._krp +   23._krp /  2678._krp * sqrt3  ! -0.1233493271443549
  k(6,3) = -2221._krp / 16068._krp +   23._krp /  2678._krp * sqrt3  ! -0.1233493271443548
  k(6,4) =  2783._krp /  5356._krp -   45._krp /  2678._krp * sqrt3  !  0.4904995196637045
  k(6,5) =    71._krp /   412._krp -  219._krp /  2678._krp * sqrt3  !  0.03068740595310525
  k(6,6) =  2783._krp /  5356._krp -   45._krp /  2678._krp * sqrt3  !  0.4904995196637045
!Gauss point32 coefficients
  k(7,1) =  1043._krp / 16068._krp -  263._krp /  2678._krp * sqrt3  ! -0.1051889578257212
  k(7,2) = -2221._krp / 16068._krp -   23._krp /  2678._krp * sqrt3  ! -0.1531007599853439
  k(7,3) = -2221._krp / 16068._krp -   23._krp /  2678._krp * sqrt3  ! -0.1531007599853438
  k(7,4) =  2783._krp /  5356._krp +   45._krp /  2678._krp * sqrt3  !  0.5487088447873784
  k(7,5) =    71._krp /   412._krp +  219._krp /  2678._krp * sqrt3  !  0.3139727882216520
  k(7,6) =  2783._krp /  5356._krp +   45._krp /  2678._krp * sqrt3  !  0.5487088447873784
  call distrib_svmweights_tri3(defsvm, k)

case(svm_3kris) !weights for alpha=91/1000 and beta=18/100 : OPTIMISED PARTITION BY ABEELE
  defsvm%ncv     = 6
  defsvm%ncvface = 18
  defsvm%nfgauss = 2
  allocate(defsvm%interp_weights(defsvm%ncvface*defsvm%nfgauss, defsvm%ncv))

!Gauss point1
  k(1,1) =  3287080870527984892769449._krp /  2789865336429461513887500._krp + 1418701068984221352033613._krp / 22318922691435692111100000._krp * sqrt3  ! 1.28831976766918
  k(1,2) =   -71144047021860659130551._krp /  2789865336429461513887500._krp +  522189246477331890433613._krp / 22318922691435692111100000._krp * sqrt3  !  .0150233922365182
  k(1,3) =   210615222587040306569449._krp /  2789865336429461513887500._krp -   63476794901672945922129._krp /  7439640897145230703700000._krp * sqrt3  !  .0607146992628959
  k(1,4) =    56266710319992738581017._krp /   929955112143153837962500._krp -  876409787064336349877871._krp /  7439640897145230703700000._krp * sqrt3  ! -.143535502808458
  k(1,5) =    52880933332645863637267._krp /   929955112143153837962500._krp +  196417465687037246872129._krp /  7439640897145230703700000._krp * sqrt3  !  .102592652962310
  k(1,6) =  -321376546873872944325233._krp /   929955112143153837962500._krp +   96505677791787634772129._krp /  7439640897145230703700000._krp * sqrt3  ! -.323115009322447
!Gauss point2 coefficients
  k(2,1) =  3287080870527984892769449._krp /  2789865336429461513887500._krp - 1418701068984221352033613._krp / 22318922691435692111100000._krp * sqrt3  ! 1.06812434280883
  k(2,2) =   -71144047021860659130551._krp /  2789865336429461513887500._krp -  522189246477331890433613._krp / 22318922691435692111100000._krp * sqrt3  ! -.0660251707760604
  k(2,3) =   210615222587040306569449._krp /  2789865336429461513887500._krp +   63476794901672945922129._krp /  7439640897145230703700000._krp * sqrt3  !  .0902712424862430
  k(2,4) =    56266710319992738581017._krp /   929955112143153837962500._krp +  876409787064336349877871._krp /  7439640897145230703700000._krp * sqrt3  !  .264545021623451
  k(2,5) =    52880933332645863637267._krp /   929955112143153837962500._krp -  196417465687037246872129._krp /  7439640897145230703700000._krp * sqrt3  !  .0111352735626110
  k(2,6) =  -321376546873872944325233._krp /   929955112143153837962500._krp -   96505677791787634772129._krp /  7439640897145230703700000._krp * sqrt3  ! -.368050709705079
!Gauss point3 coefficients
  k(3,1) =   137469674468030631738287._krp /  1394932668214730756943750._krp +            80357242386248._krp /            445102789234767._krp * sqrt3  !  .411247493702439
  k(3,2) =   137469674468030631738287._krp /  1394932668214730756943750._krp -            80357242386248._krp /            445102789234767._krp * sqrt3  ! -.214148841415555
  k(3,3) =   236097710678835289113287._krp /  1394932668214730756943750._krp                                                                             !  .169253840030142
  k(3,4) =   630063153967919686514321._krp /   464977556071576918981250._krp                                                                             ! 1.35503992771412
  k(3,5) =  -167715642217320809198179._krp /   464977556071576918981250._krp +             8955415372763._krp /            148367596411589._krp * sqrt3  ! -.256150239037658
  k(3,6) =  -167715642217320809198179._krp /   464977556071576918981250._krp -             8955415372763._krp /            148367596411589._krp * sqrt3  ! -.465242180993486
 !Gauss point19 coefficients
  k(4,1) =  9251092766669808504418591._krp / 11159461345717846055550000._krp +   22281691035126377513779._krp /   421111748895013058700000._krp * sqrt3  !  .920636534649943
  k(4,2) =  -748200062253774879181409._krp / 11159461345717846055550000._krp -    3293964392767447286221._krp /   421111748895013058700000._krp * sqrt3  ! -.0805944702065471
  k(4,3) =   263898843663433393618591._krp / 11159461345717846055550000._krp +    3383896341922490704593._krp /   140370582965004352900000._krp * sqrt3  !  .0654023265278943
  k(4,4) =  1203018128665083511793803._krp /  3719820448572615351850000._krp +    3586821656594948395407._krp /   140370582965004352900000._krp * sqrt3  !  .367665836904311
  k(4,5) =  -199181021302678486731197._krp /  3719820448572615351850000._krp +    1470113694091063145407._krp /   140370582965004352900000._krp * sqrt3  ! -.0354059464509639
  k(4,6) =  -206280508149612012831197._krp /  3719820448572615351850000._krp -   14770073906728145654593._krp /   140370582965004352900000._krp * sqrt3  ! -.237704281424637
!Gauss point20 coefficients
  k(5,1) =  9251092766669808504418591._krp / 11159461345717846055550000._krp -   22281691035126377513779._krp /   421111748895013058700000._krp * sqrt3  !  .737345419867903
  k(5,2) =  -748200062253774879181409._krp / 11159461345717846055550000._krp +    3293964392767447286221._krp /   421111748895013058700000._krp * sqrt3  ! -.0534980346330126
  k(5,3) =   263898843663433393618591._krp / 11159461345717846055550000._krp -    3383896341922490704593._krp /   140370582965004352900000._krp * sqrt3  ! -.0181063441344972
  k(5,4) =  1203018128665083511793803._krp /  3719820448572615351850000._krp -    3586821656594948395407._krp /   140370582965004352900000._krp * sqrt3  !  .279149322750349
  k(5,5) =  -199181021302678486731197._krp /  3719820448572615351850000._krp -    1470113694091063145407._krp /   140370582965004352900000._krp * sqrt3  ! -.0716857930866837
  k(5,6) =  -206280508149612012831197._krp /  3719820448572615351850000._krp +   14770073906728145654593._krp /   140370582965004352900000._krp * sqrt3  !  .126795429235941
!Gauss point31 coefficients
  k(6,1) =   505076654113076972792003._krp /  3719820448572615351850000._krp +   30355756079882984487361._krp /   223189226914356921111000._krp * sqrt3  !  .371354443156496
  k(6,2) = -1249121416964422867623991._krp / 11159461345717846055550000._krp +    1205693445067739415787._krp /    74396408971452307037000._krp * sqrt3  ! -.0838636419975609
  k(6,3) = -1249121416964422867623991._krp / 11159461345717846055550000._krp +    1205693445067739415787._krp /    74396408971452307037000._krp * sqrt3  ! -.0838636419975616
  k(6,4) = 17289833884649582135671973._krp / 33478384037153538166650000._krp -   25239640854818887127083._krp /   669567680743070763333000._krp * sqrt3  !  .451157285597195
  k(6,5) =  1847754882623218345921973._krp / 33478384037153538166650000._krp -   62290468541230488692083._krp /   669567680743070763333000._krp * sqrt3  ! -.105941730355765
  k(6,6) = 17289833884649582135671973._krp / 33478384037153538166650000._krp -   25239640854818887127083._krp /   669567680743070763333000._krp * sqrt3  !  .451157285597204
!Gauss point32 coefficients
  k(7,1) =   505076654113076972792003._krp /  3719820448572615351850000._krp -   30355756079882984487361._krp /   223189226914356921111000._krp * sqrt3  ! -.0997947476841605
  k(7,2) = -1249121416964422867623991._krp / 11159461345717846055550000._krp -    1205693445067739415787._krp /    74396408971452307037000._krp * sqrt3  ! -.140004048075805
  k(7,3) = -1249121416964422867623991._krp / 11159461345717846055550000._krp -    1205693445067739415787._krp /    74396408971452307037000._krp * sqrt3  ! -.140004048075802
  k(7,4) = 17289833884649582135671973._krp / 33478384037153538166650000._krp +   25239640854818887127083._krp /   669567680743070763333000._krp * sqrt3  !  .581738081482745
  k(7,5) =  1847754882623218345921973._krp / 33478384037153538166650000._krp +   62290468541230488692083._krp /   669567680743070763333000._krp * sqrt3  !  .216326680870262
  k(7,6) = 17289833884649582135671973._krp / 33478384037153538166650000._krp +   25239640854818887127083._krp /   669567680743070763333000._krp * sqrt3  !  .581738081482761
  call distrib_svmweights_tri3(defsvm, k)

case(svm_3kris2) !weights for alpha=0.1093621117 and beta =0.1730022492 : OPTIMISED PARTITION BY ABEELE
  defsvm%ncv     = 6
  defsvm%ncvface = 18
  defsvm%nfgauss = 2
  allocate(defsvm%interp_weights(defsvm%ncvface*defsvm%nfgauss, defsvm%ncv))

 !Gauss point1
  k(1,1) =  1.28944701993_krp
  k(1,2) =  0.0103882882085_krp
  k(1,3) =  0.0651156162188_krp
  k(1,4) = -0.126229145560_krp
  k(1,5) =  0.102635587273_krp
  k(1,6) = -0.341357366061_krp
!Gauss point2 coefficients
  k(2,1) =  1.02577584775_krp
  k(2,2) = -0.0839351532395_krp
  k(2,3) =  0.0996328359745_krp
  k(2,4) =  0.356693891316_krp
  k(2,5) = -0.0064796292939_krp
  k(2,6) = -0.391687792499_krp
!Gauss point3 coefficients
  k(3,1) =  0.390385234969_krp
  k(3,2) = -0.214519323766_krp
  k(3,3) =  0.173486796107_krp
  k(3,4) =  1.389968846972_krp
  k(3,5) = -0.264672102711_krp
  k(3,6) = -0.474649451571_krp
 !Gauss point19 coefficients
  k(4,1) =  0.885879027380_krp
  k(4,2) = -0.0966632190484_krp
  k(4,3) =  0.0749322000852_krp
  k(4,4) =  0.450383624165_krp
  k(4,5) = -0.0480832622194_krp
  k(4,6) = -0.266448370364_krp
!Gauss point20 coefficients
  k(5,1) =  0.755327834819_krp
  k(5,2) = -0.0552290654330_krp
  k(5,3) = -0.0124743075030_krp
  k(5,4) =  0.280712020840_krp
  k(5,5) = -0.0654699723469_krp
  k(5,6) =  0.0971334896216_krp
!Gauss point31 coefficients
  k(6,1) =  0.394269474653_krp
  k(6,2) = -0.0814463773805_krp
  k(6,3) = -0.0814463773805_krp
  k(6,4) =  0.435664029246_krp
  k(6,5) = -0.102704778384_krp
  k(6,6) =  0.435664029246_krp
!Gauss point32 coefficients
  k(7,1) = -0.0966823063266_krp
  k(7,2) = -0.139478945801_krp
  k(7,3) = -0.139478945801_krp
  k(7,4) =  0.581005928105_krp
  k(7,5) =  0.213628341733_krp
  k(7,6) =  0.581005928105_krp
  call distrib_svmweights_tri3(defsvm, k)

  ! 54 Gauss pts, 10 coeff per point
case(svm_4wang)   ! 10 independent points
  defsvm%ncv     = 10
  defsvm%ncvface = 27
  defsvm%nfgauss = 2
  allocate(defsvm%interp_weights(defsvm%ncvface*defsvm%nfgauss, defsvm%ncv))

  !Gauss point1 coefficients
  k4(1,1) = 1768981733._krp /1381048200._krp + 4453635187._krp /43503018300._krp * sqrt3 !1.458217141953126
  k4(1,2) =-42581._krp /197292600._krp - 876224413._krp /43503018300._krp * sqrt3 !-.03510226122972749
  k4(1,3) =-669387319._krp /9667337400._krp + 35661._krp /76724900._krp * sqrt3 !-.06843711892739588
  k4(1,4) =-1292945493883._krp /3086761276177488._krp - 1948167267562391._krp /13890425742798696._krp * sqrt3 !-.2433433651618313
  k4(1,5) = 2070067978117._krp /3086761276177488._krp + 802347429185609._krp /13890425742798696._krp * sqrt3 !.1007184260927767
  k4(1,6) = 1590450600497._krp /10803664466621208._krp - 396689847695287._krp /97232980199590872._krp * sqrt3 !-.006919184357709881
  k4(1,7) = 7885037444497._krp /10803664466621208._krp - 143139725078429._krp /32410993399863624._krp * sqrt3 !-.006919570817126903
  k4(1,8) = 2072224105156747._krp /10803664466621208._krp + 11668442484071._krp /32410993399863624._krp * sqrt3 !.1924310857662292
  k4(1,9) =-4350258709591253._krp /10803664466621208._krp - 1377665997703787._krp /97232980199590872._krp * sqrt3 !-.4272060344642104
  k4(1,10)=-109._krp /63700._krp + 1307._krp /59150._krp * sqrt3 !.03656088114587044
!Gauss point2 coefficients
  k4(2,1) = 1768981733._krp /1381048200._krp - 4453635187._krp /43503018300._krp * sqrt3 !1.103578649098918
  k4(2,2) =-42581._krp /197292600._krp + 876224413._krp /43503018300._krp * sqrt3 !.03467060793913270
  k4(2,3) =-669387319._krp /9667337400._krp - 35661._krp /76724900._krp * sqrt3 !-.07004720023994796
  k4(2,4) =-1292945493883._krp /3086761276177488._krp + 1948167267562391._krp /13890425742798696._krp * sqrt3 !.2425056291801981
  k4(2,5) = 2070067978117._krp /3086761276177488._krp - 802347429185609._krp /13890425742798696._krp * sqrt3 !-.09937717045756253
  k4(2,6) = 1590450600497._krp /10803664466621208._krp + 396689847695287._krp /97232980199590872._krp * sqrt3 !.007213612346548450
  k4(2,7) = 7885037444497._krp /10803664466621208._krp + 143139725078429._krp /32410993399863624._krp * sqrt3 !.008379267657742133
  k4(2,8) = 2072224105156747._krp /10803664466621208._krp - 11668442484071._krp /32410993399863624._krp * sqrt3 !.1911839573627110
  k4(2,9) =-4350258709591253._krp /10803664466621208._krp + 1377665997703787._krp /97232980199590872._krp * sqrt3!-.3781241797481484
  k4(2,10)=-109._krp /63700._krp - 1307._krp /59150._krp * sqrt3 !-.03998317313959101
!Gauss point3 coefficients
  k4(3,1) = 2439331739._krp /38669349600._krp + 15686961901._krp /87006036600._krp * sqrt3 !.3753660403108074
  k4(3,2) =-614999411._krp /38669349600._krp + 241031323._krp /3551266800._krp * sqrt3 !.1016535732845099
  k4(3,3) =-736786609._krp /9667337400._krp + 6026709._krp /4296594400._krp * sqrt3 !-.07378451848298236
  k4(3,4) = 517471099935941._krp /440965896596784._krp - 148215128122391._krp /4602744624832704._krp * sqrt3 !1.117720026854252
  k4(3,5) = 28268412026941._krp /440965896596784._krp - 997604710254391._krp /4602744624832704._krp * sqrt3 !-.3113012248115170
  k4(3,6) = 368567921114069._krp /13296817805072256._krp + 407116159299559._krp /119671360245650304._krp * sqrt3 !.03361085855679458
  k4(3,7) = 1103241082882069._krp /13296817805072256._krp - 829749352300147._krp /39890453415216768._krp * sqrt3 !.04694243992224653
  k4(3,8) = 174303637939063._krp /1022832138851712._krp + 341427445521103._krp /39890453415216768._krp * sqrt3 !.1852375927004566
  k4(3,9) =-160082362372937._krp /1022832138851712._krp - 6997580727924691._krp /119671360245650304._krp * sqrt3 !-.2577876752226963
  k4(3,10)=-275887._krp /828100._krp + 1307._krp /19600._krp * sqrt3 !-.2176571131118718
 !Gauss point4 coefficients
  k4(4,1) = 2439331739._krp /38669349600._krp - 15686961901._krp /87006036600._krp * sqrt3 !-.2492024629953009
  k4(4,2) =-614999411._krp /38669349600._krp - 241031323._krp /3551266800._krp * sqrt3 !-.1334616806026631
  k4(4,3) =-736786609._krp /9667337400._krp - 6026709._krp /4296594400._krp * sqrt3 !-.07864351387264848
  k4(4,4) = 517471099935941._krp /440965896596784._krp + 148215128122391._krp /4602744624832704._krp * sqrt3 !1.229269179928392
  k4(4,5) = 28268412026941._krp /440965896596784._krp + 997604710254391._krp /4602744624832704._krp * sqrt3 !.439512554735696
  k4(4,6) = 368567921114069._krp /13296817805072256._krp - 407116159299559._krp /119671360245650304._krp * sqrt3 !.02182615299246109
  k4(4,7) = 1103241082882069._krp /13296817805072256._krp + 829749352300147._krp /39890453415216768._krp * sqrt3 !.1189981782098938
  k4(4,8) = 174303637939063._krp /1022832138851712._krp - 341427445521103._krp /39890453415216768._krp * sqrt3 !.1555879080209930
  k4(4,9) =-160082362372937._krp /1022832138851712._krp + 6997580727924691._krp /119671360245650304._krp * sqrt3 !-.05523018233632459
  k4(4,10)=-275887._krp /828100._krp - 1307._krp /19600._krp * sqrt3 !-.4486561340804965
!Gauss point25 coefficients
  k4(5,1) = 3592567981._krp /4833668700._krp + 839433706._krp /10875754575._krp * sqrt3 !.8769248809228335
  k4(5,2) = 66810139._krp /1208417175._krp - 199901._krp /345262050._krp * sqrt3 !.05428448465266691
  k4(5,3) = 329477._krp /1611222900._krp - 460815863._krp /21751509150._krp * sqrt3 !-.03648981515150175
  k4(5,4) = 7705462264376813._krp /21607328933242416._krp + 1277370552912451._krp /97232980199590872._krp * sqrt3 !.3793676879152942
  k4(5,5) =-3368535131175187._krp /21607328933242416._krp + 9575056126817._krp /32410993399863624._krp * sqrt3 !-.1553861108461736
  k4(5,6) =-13034778674137._krp /21607328933242416._krp + 33073087956373._krp /8102748349965906._krp * sqrt3 !.006466475871862843
  k4(5,7) =-1229503611379._krp /7202442977747472._krp + 107401595754119._krp /24308245049897718._krp * sqrt3 !.007482047576794072
  k4(5,8) =-360226438617._krp /600203581478956._krp + 431550141940577._krp /7479460015353144._krp * sqrt3 !.09933575347817026
  k4(5,9) = 1832509166447._krp /5401832233310604._krp - 844779563255423._krp /7479460015353144._krp * sqrt3 !-.1952900077507750
  k4(5,10)= 47._krp /29575._krp - 352._krp /15925._krp * sqrt3 !-.03669539666917123
!Gauss point26 coefficients
  k4(6,1) = 3592567981._krp /4833668700._krp - 839433706._krp /10875754575._krp * sqrt3 !.6095518323032922
  k4(6,2) = 66810139._krp /1208417175._krp + 199901._krp /345262050._krp * sqrt3 !.05629014202789150
  k4(6,3) = 329477._krp /1611222900._krp + 460815863._krp /21751509150._krp * sqrt3 !.03689879270513509
  k4(6,4) = 7705462264376813._krp /21607328933242416._krp - 1277370552912451._krp /97232980199590872._krp * sqrt3 !.3338590406806799
  k4(6,5) =-3368535131175187._krp /21607328933242416._krp - 9575056126817._krp /32410993399863624._krp * sqrt3 !-.1564094971702126
  k4(6,6) =-13034778674137._krp /21607328933242416._krp - 33073087956373._krp /8102748349965906._krp * sqrt3 !-.007672990449801567
  k4(6,7) =-1229503611379._krp /7202442977747472._krp - 107401595754119._krp /24308245049897718._krp * sqrt3 !-.007823460515481026
  k4(6,8) =-360226438617._krp /600203581478956._krp - 431550141940577._krp /7479460015353144._krp * sqrt3 !-.1005361009926898
  k4(6,9) = 1832509166447._krp /5401832233310604._krp + 844779563255423._krp /7479460015353144._krp * sqrt3 !.1959684846405780
  k4(6,10)= 47._krp /29575._krp + 352._krp /15925._krp * sqrt3 !.03987375677060830
!Gauss point27 coefficients
  k4(7,1) =-15473103913._krp /77338699200._krp - 5716773361._krp /696048292800._krp * sqrt3 !-.2142950118210653
  k4(7,2) =-15473103913._krp /77338699200._krp - 5716773361._krp /696048292800._krp * sqrt3 !-.2142950118210653
  k4(7,3) = 99838637._krp /77338699200._krp - 16836181111._krp /696048292800._krp * sqrt3 !-.04060432824709804
  k4(7,4) = 729319422439._krp /1045619213184._krp + 514339228783._krp /9410572918656._krp * sqrt3 !.7921660607666403
  k4(7,5) = 729319422439._krp /1045619213184._krp + 514339228783._krp /9410572918656._krp * sqrt3 !.7921660607666403
  k4(7,6) = 659561207._krp /522809606592._krp - 1555325987._krp /588160807416._krp * sqrt3 !-.003318645597139651
  k4(7,7) =-1129134793._krp /522809606592._krp + 25424172013._krp /588160807416._krp * sqrt3 !.07271086492934911
  k4(7,8) =-1129134793._krp /522809606592._krp + 25424172013._krp /588160807416._krp * sqrt3 !.07271086492934911
  k4(7,9) = 659561207._krp /522809606592._krp - 1555325987._krp /588160807416._krp * sqrt3 !-.003318645597139651
  k4(7,10)= 37391._krp /6624800._krp - 76369._krp /509600._krp * sqrt3 !-.2539222083084711
!Gauss point28 coefficients
  k4(8,1) =-15473103913._krp /77338699200._krp + 5716773361._krp /696048292800._krp * sqrt3 !-.1858437045797919
  k4(8,2) =-15473103913._krp /77338699200._krp + 5716773361._krp /696048292800._krp * sqrt3 !-.1858437045797919
  k4(8,3) = 99838637._krp /77338699200._krp + 16836181111._krp /696048292800._krp * sqrt3 !.04318618281751990
  k4(8,4) = 729319422439._krp /1045619213184._krp - 514339228783._krp /9410572918656._krp * sqrt3 !.6028339798660485
  k4(8,5) = 729319422439._krp /1045619213184._krp - 514339228783._krp /9410572918656._krp * sqrt3 !.6028339798660485
  k4(8,6) = 659561207._krp /522809606592._krp + 1555325987._krp /588160807416._krp * sqrt3 !.005841786712695776
  k4(8,7) =-1129134793._krp /522809606592._krp - 25424172013._krp /588160807416._krp * sqrt3 !-.07703035247802057
  k4(8,8) =-1129134793._krp /522809606592._krp - 25424172013._krp /588160807416._krp * sqrt3 !-.07703035247802057
  k4(8,9) = 659561207._krp /522809606592._krp + 1555325987._krp /588160807416._krp * sqrt3 !.005841786712695776
  k4(8,10)= 37391._krp /6624800._krp + 76369._krp /509600._krp * sqrt3 !.2652103981406168
!Gauss point43 coefficients
  k4(9,1) =-526303._krp /9367575._krp + 13349422._krp /134268575._krp * sqrt3 !.1160226867853809
  k4(9,2) = 6302921._krp /402805725._krp + 7471972._krp /134268575._krp * sqrt3 !.1120352160093954
  k4(9,3) = 25535096._krp /402805725._krp - 30754._krp /19181225._krp * sqrt3 !.06061601706069237
  k4(9,4) = 375004593519727._krp /450152686109217._krp - 160706727047._krp /300101790739478._krp * sqrt3 !.8321333555350553
  k4(9,5) =-26938004810273._krp /450152686109217._krp - 45753673419047._krp /300101790739478._krp * sqrt3 !-.3239112856630841
  k4(9,6) =-50642096137817._krp /1800610744436868._krp - 499400961586._krp /150050895369739._krp * sqrt3 !-.03388958462684823
  k4(9,7) =-130619563525817._krp /1800610744436868._krp + 2714927155414._krp /150050895369739._krp * sqrt3 !-.04120316536526829
  k4(9,8) =-20116865096759._krp /138508518802836._krp - 1792228465897._krp /300101790739478._krp * sqrt3 !-.1555831162807538
  k4(9,9) = 19223615355241._krp /138508518802836._krp + 15588348384103._krp /300101790739478._krp * sqrt3 !.2287589748838570
  k4(9,10)= 9199._krp /29575._krp - 12672._krp /207025._krp * sqrt3 !.2050209016615732
!Gauss point44 coefficients
  k4(10,1) =-526303._krp /9367575._krp - 13349422._krp /134268575._krp * sqrt3 !-.2283896547573468
  k4(10,2) = 6302921._krp /402805725._krp - 7471972._krp /134268575._krp * sqrt3 !-.0807401245605332
  k4(10,3) = 25535096._krp /402805725._krp + 30754._krp /19181225._krp * sqrt3 !.06617014517669887
  k4(10,4) = 375004593519727._krp /450152686109217._krp + 160706727047._krp /300101790739478._krp * sqrt3 !.833988407553757
  k4(10,5) =-26938004810273._krp /450152686109217._krp + 45753673419047._krp /300101790739478._krp * sqrt3 !.204227428867271
  k4(10,6) =-50642096137817._krp /1800610744436868._krp + 499400961586._krp /150050895369739._krp * sqrt3 !-.02236032534873068
  k4(10,7) =-130619563525817._krp /1800610744436868._krp - 2714927155414._krp /150050895369739._krp * sqrt3 !-.1038804557664801
  k4(10,8) =-20116865096759._krp /138508518802836._krp + 1792228465897._krp /300101790739478._krp * sqrt3 !-.1348952639753363
  k4(10,9) = 19223615355241._krp /138508518802836._krp - 15588348384103._krp /300101790739478._krp * sqrt3 !.04882128546973781
  k4(10,10)= 9199._krp /29575._krp + 12672._krp /207025._krp * sqrt3 !.4170585573409627

  defsvm%interp_weights(1, 1:defsvm%ncv)  = (/ k4(1,1), k4(1,2), k4(1,3) , k4(1,4), k4(1,5), k4(1,6),k4(1,7),k4(1,8),k4(1,9),k4(1,10)/)!
  defsvm%interp_weights(8, 1:defsvm%ncv)  = (/ k4(1,2), k4(1,1), k4(1,3) , k4(1,5), k4(1,4), k4(1,9),k4(1,8),k4(1,7),k4(1,6),k4(1,10)/)
  defsvm%interp_weights(9, 1:defsvm%ncv)  = (/ k4(1,3), k4(1,1), k4(1,2) , k4(1,8), k4(1,9), k4(1,4),k4(1,5),k4(1,6),k4(1,7),k4(1,10)/)
  defsvm%interp_weights(16, 1:defsvm%ncv) = (/ k4(1,3), k4(1,2), k4(1,1) , k4(1,7), k4(1,6), k4(1,5),k4(1,4),k4(1,9),k4(1,8),k4(1,10)/)
  defsvm%interp_weights(17, 1:defsvm%ncv) = (/ k4(1,2), k4(1,3), k4(1,1) , k4(1,6), k4(1,7), k4(1,8),k4(1,9),k4(1,4),k4(1,5),k4(1,10)/)
  defsvm%interp_weights(24, 1:defsvm%ncv) = (/ k4(1,1), k4(1,3), k4(1,2) , k4(1,9), k4(1,8), k4(1,7),k4(1,6),k4(1,5),k4(1,4),k4(1,10)/)

  defsvm%interp_weights(2, 1:defsvm%ncv)  = (/ k4(2,1), k4(2,2), k4(2,3) , k4(2,4), k4(2,5), k4(2,6),k4(2,7),k4(2,8),k4(2,9),k4(2,10)/)!
  defsvm%interp_weights(7, 1:defsvm%ncv)  = (/ k4(2,2), k4(2,1), k4(2,3) , k4(2,5), k4(2,4), k4(2,9),k4(2,8),k4(2,7),k4(2,6),k4(2,10)/)
  defsvm%interp_weights(10, 1:defsvm%ncv) = (/ k4(2,3), k4(2,1), k4(2,2) , k4(2,8), k4(2,9), k4(2,4),k4(2,5),k4(2,6),k4(2,7),k4(2,10)/)
  defsvm%interp_weights(15, 1:defsvm%ncv) = (/ k4(2,3), k4(2,2), k4(2,1) , k4(2,7), k4(2,6), k4(2,5),k4(2,4),k4(2,9),k4(2,8),k4(2,10)/)
  defsvm%interp_weights(18, 1:defsvm%ncv) = (/ k4(2,2), k4(2,3), k4(2,1) , k4(2,6), k4(2,7), k4(2,8),k4(2,9),k4(2,4),k4(2,5),k4(2,10)/)
  defsvm%interp_weights(23, 1:defsvm%ncv) = (/ k4(2,1), k4(2,3), k4(2,2) , k4(2,9), k4(2,8), k4(2,7),k4(2,6),k4(2,5),k4(2,4),k4(2,10)/)

  defsvm%interp_weights(3, 1:defsvm%ncv)  = (/ k4(3,1), k4(3,2), k4(3,3) , k4(3,4), k4(3,5), k4(3,6),k4(3,7),k4(3,8),k4(3,9),k4(3,10)/)!
  defsvm%interp_weights(6, 1:defsvm%ncv)  = (/ k4(3,2), k4(3,1), k4(3,3) , k4(3,5), k4(3,4), k4(3,9),k4(3,8),k4(3,7),k4(3,6),k4(3,10)/)
  defsvm%interp_weights(11, 1:defsvm%ncv) = (/ k4(3,3), k4(3,1), k4(3,2) , k4(3,8), k4(3,9), k4(3,4),k4(3,5),k4(3,6),k4(3,7),k4(3,10)/)
  defsvm%interp_weights(14, 1:defsvm%ncv) = (/ k4(3,3), k4(3,2), k4(3,1) , k4(3,7), k4(3,6), k4(3,5),k4(3,4),k4(3,9),k4(3,8),k4(3,10)/)
  defsvm%interp_weights(19, 1:defsvm%ncv) = (/ k4(3,2), k4(3,3), k4(3,1) , k4(3,6), k4(3,7), k4(3,8),k4(3,9),k4(3,4),k4(3,5),k4(3,10)/)
  defsvm%interp_weights(22, 1:defsvm%ncv) = (/ k4(3,1), k4(3,3), k4(3,2) , k4(3,9), k4(3,8), k4(3,7),k4(3,6),k4(3,5),k4(3,4),k4(3,10)/)

  defsvm%interp_weights(4, 1:defsvm%ncv)  = (/ k4(4,1), k4(4,2), k4(4,3) , k4(4,4), k4(4,5), k4(4,6),k4(4,7),k4(4,8),k4(4,9),k4(4,10)/)!
  defsvm%interp_weights(5, 1:defsvm%ncv)  = (/ k4(4,2), k4(4,1), k4(4,3) , k4(4,5), k4(4,4), k4(4,9),k4(4,8),k4(4,7),k4(4,6),k4(4,10)/)
  defsvm%interp_weights(12, 1:defsvm%ncv) = (/ k4(4,3), k4(4,1), k4(4,2) , k4(4,8), k4(4,9), k4(4,4),k4(4,5),k4(4,6),k4(4,7),k4(4,10)/)
  defsvm%interp_weights(13, 1:defsvm%ncv) = (/ k4(4,3), k4(4,2), k4(4,1) , k4(4,7), k4(4,6), k4(4,5),k4(4,4),k4(4,9),k4(4,8),k4(4,10)/)
  defsvm%interp_weights(20, 1:defsvm%ncv) = (/ k4(4,2), k4(4,3), k4(4,1) , k4(4,6), k4(4,7), k4(4,8),k4(4,9),k4(4,4),k4(4,5),k4(4,10)/)
  defsvm%interp_weights(21, 1:defsvm%ncv) = (/ k4(4,1), k4(4,3), k4(4,2) , k4(4,9), k4(4,8), k4(4,7),k4(4,6),k4(4,5),k4(4,4),k4(4,10)/)

  defsvm%interp_weights(25, 1:defsvm%ncv) = (/ k4(5,1), k4(5,2), k4(5,3) , k4(5,4), k4(5,5), k4(5,6),k4(5,7),k4(5,8),k4(5,9),k4(5,10)/)!
  defsvm%interp_weights(29, 1:defsvm%ncv) = (/ k4(5,2), k4(5,1), k4(5,3) , k4(5,5), k4(5,4), k4(5,9),k4(5,8),k4(5,7),k4(5,6),k4(5,10)/)
  defsvm%interp_weights(31, 1:defsvm%ncv) = (/ k4(5,3), k4(5,1), k4(5,2) , k4(5,8), k4(5,9), k4(5,4),k4(5,5),k4(5,6),k4(5,7),k4(5,10)/)
  defsvm%interp_weights(35, 1:defsvm%ncv) = (/ k4(5,3), k4(5,2), k4(5,1) , k4(5,7), k4(5,6), k4(5,5),k4(5,4),k4(5,9),k4(5,8),k4(5,10)/)
  defsvm%interp_weights(37, 1:defsvm%ncv) = (/ k4(5,2), k4(5,3), k4(5,1) , k4(5,6), k4(5,7), k4(5,8),k4(5,9),k4(5,4),k4(5,5),k4(5,10)/)
  defsvm%interp_weights(41, 1:defsvm%ncv) = (/ k4(5,1), k4(5,3), k4(5,2) , k4(5,9), k4(5,8), k4(5,7),k4(5,6),k4(5,5),k4(5,4),k4(5,10)/)

  defsvm%interp_weights(26, 1:defsvm%ncv) = (/ k4(6,1), k4(6,2), k4(6,3) , k4(6,4), k4(6,5), k4(6,6),k4(6,7),k4(6,8),k4(6,9),k4(6,10)/)!
  defsvm%interp_weights(30, 1:defsvm%ncv) = (/ k4(6,2), k4(6,1), k4(6,3) , k4(6,5), k4(6,4), k4(6,9),k4(6,8),k4(6,7),k4(6,6),k4(6,10)/)
  defsvm%interp_weights(32, 1:defsvm%ncv) = (/ k4(6,3), k4(6,1), k4(6,2) , k4(6,8), k4(6,9), k4(6,4),k4(6,5),k4(6,6),k4(6,7),k4(6,10)/)
  defsvm%interp_weights(36, 1:defsvm%ncv) = (/ k4(6,3), k4(6,2), k4(6,1) , k4(6,7), k4(6,6), k4(6,5),k4(6,4),k4(6,9),k4(6,8),k4(6,10)/)
  defsvm%interp_weights(38, 1:defsvm%ncv) = (/ k4(6,2), k4(6,3), k4(6,1) , k4(6,6), k4(6,7), k4(6,8),k4(6,9),k4(6,4),k4(6,5),k4(6,10)/)
  defsvm%interp_weights(42, 1:defsvm%ncv) = (/ k4(6,1), k4(6,3), k4(6,2) , k4(6,9), k4(6,8), k4(6,7),k4(6,6),k4(6,5),k4(6,4),k4(6,10)/)

  defsvm%interp_weights(27, 1:defsvm%ncv) = (/ k4(7,1), k4(7,2), k4(7,3) , k4(7,4), k4(7,5), k4(7,6),k4(7,7),k4(7,8),k4(7,9),k4(7,10)/)!
  defsvm%interp_weights(33, 1:defsvm%ncv) = (/ k4(7,3), k4(7,1), k4(7,2) , k4(7,8), k4(7,9), k4(7,4),k4(7,5),k4(7,6),k4(7,7),k4(7,10)/)
  defsvm%interp_weights(39, 1:defsvm%ncv) = (/ k4(7,1), k4(7,3), k4(7,2) , k4(7,9), k4(7,8), k4(7,7),k4(7,6),k4(7,5),k4(7,4),k4(7,10)/)

  defsvm%interp_weights(28, 1:defsvm%ncv) = (/ k4(8,1), k4(8,2), k4(8,3) , k4(8,4), k4(8,5), k4(8,6),k4(8,7),k4(8,8),k4(8,9),k4(8,10)/)!
  defsvm%interp_weights(34, 1:defsvm%ncv) = (/ k4(8,3), k4(8,1), k4(8,2) , k4(8,8), k4(8,9), k4(8,4),k4(8,5),k4(8,6),k4(8,7),k4(8,10)/)
  defsvm%interp_weights(40, 1:defsvm%ncv) = (/ k4(8,1), k4(8,3), k4(8,2) , k4(8,9), k4(8,8), k4(8,7),k4(8,6),k4(8,5),k4(8,4),k4(8,10)/)

  defsvm%interp_weights(43, 1:defsvm%ncv) = (/ k4(9,1), k4(9,2), k4(9,3) , k4(9,4), k4(9,5), k4(9,6),k4(9,7),k4(9,8),k4(9,9),k4(9,10)/)!
  defsvm%interp_weights(46, 1:defsvm%ncv) = (/ k4(9,2), k4(9,1), k4(9,3) , k4(9,5), k4(9,4), k4(9,9),k4(9,8),k4(9,7),k4(9,6),k4(9,10)/)
  defsvm%interp_weights(47, 1:defsvm%ncv) = (/ k4(9,3), k4(9,1), k4(9,2) , k4(9,8), k4(9,9), k4(9,4),k4(9,5),k4(9,6),k4(9,7),k4(9,10)/)
  defsvm%interp_weights(50, 1:defsvm%ncv) = (/ k4(9,3), k4(9,2), k4(9,1) , k4(9,7), k4(9,6), k4(9,5),k4(9,4),k4(9,9),k4(9,8),k4(9,10)/)
  defsvm%interp_weights(51, 1:defsvm%ncv) = (/ k4(9,2), k4(9,3), k4(9,1) , k4(9,6), k4(9,7), k4(9,8),k4(9,9),k4(9,4),k4(9,5),k4(9,10)/)
  defsvm%interp_weights(54, 1:defsvm%ncv) = (/ k4(9,1), k4(9,3), k4(9,2) , k4(9,9), k4(9,8), k4(9,7),k4(9,6),k4(9,5),k4(9,4),k4(9,10)/)

  defsvm%interp_weights(44, 1:defsvm%ncv) = (/ k4(10,1), k4(10,2), k4(10,3) , k4(10,4), k4(10,5), k4(10,6),k4(10,7),k4(10,8),k4(10,9),k4(10,10)/)!
  defsvm%interp_weights(45, 1:defsvm%ncv) = (/ k4(10,2), k4(10,1), k4(10,3) , k4(10,5), k4(10,4), k4(10,9),k4(10,8),k4(10,7),k4(10,6),k4(10,10)/)
  defsvm%interp_weights(48, 1:defsvm%ncv) = (/ k4(10,3), k4(10,1), k4(10,2) , k4(10,8), k4(10,9), k4(10,4),k4(10,5),k4(10,6),k4(10,7),k4(10,10)/)
  defsvm%interp_weights(49, 1:defsvm%ncv) = (/ k4(10,3), k4(10,2), k4(10,1) , k4(10,7), k4(10,6), k4(10,5),k4(10,4),k4(10,9),k4(10,8),k4(10,10)/)
  defsvm%interp_weights(52, 1:defsvm%ncv) = (/ k4(10,2), k4(10,3), k4(10,1) , k4(10,6), k4(10,7), k4(10,8),k4(10,9),k4(10,4),k4(10,5),k4(10,10)/)
  defsvm%interp_weights(53, 1:defsvm%ncv) = (/ k4(10,1), k4(10,3), k4(10,2) , k4(10,9), k4(10,8), k4(10,7),k4(10,6),k4(10,5),k4(10,4),k4(10,10)/)

  if (size(defsvm%interp_weights, 1) /= 54) call error_stop("SVM initialization: bad array size")

case(svm_4kris)   ! 12 independent points
  defsvm%ncv     = 10
  defsvm%ncvface = 30
  defsvm%nfgauss = 2
  allocate(defsvm%interp_weights(defsvm%ncvface*defsvm%nfgauss, defsvm%ncv))
 !Gauss point1 coefficients
  kk4(1,1) = 1.350186571632712_krp
  kk4(1,2) =-0.004622267015403575_krp
  kk4(1,3) =-0.03635534870087191_krp
  kk4(1,4) =-0.1283957991451250_krp
  kk4(1,5) = 0.02622895306569355_krp
  kk4(1,6) =-0.01954704109687104_krp
  kk4(1,7) =-0.04333476514151501_krp
  kk4(1,8) = 0.1443498783387870_krp
  kk4(1,9) =-0.3950927984532665_krp
  kk4(1,10)= 0.1065826165158600_krp
!Gauss point2 coefficients
  kk4(2,1) = 0.9835240410386652_krp
  kk4(2,2) = 0.04102022837846748_krp
  kk4(2,3) =-0.05586217796953953_krp
  kk4(2,4) = 0.4610003455829734_krp
  kk4(2,5) =-0.1673253860405332_krp
  kk4(2,6) = 0.04572072750873149_krp
  kk4(2,7) =-0.02468996556531046_krp
  kk4(2,8) = 0.1957886235502523_krp
  kk4(2,9) =-0.4250631209223152_krp
  kk4(2,10)=-0.05411331556139172_krp
!Gauss point3 coefficients
  kk4(3,1) = 0.3417745757471658_krp
  kk4(3,2) = 0.05266376003579684_krp
  kk4(3,3) =-0.09415065694535101_krp
  kk4(3,4) = 1.346286436345379_krp
  kk4(3,5) =-0.2756435522082158_krp
  kk4(3,6) = 0.1206389269298530_krp
  kk4(3,7) = 0.03320521491529624_krp
  kk4(3,8) = 0.2754551295023642_krp
  kk4(3,9) =-0.4306986344858194_krp
  kk4(3,10)=-0.3695311998364683_krp
!Gauss point4 coefficients
  kk4(4,1) =-0.2335641982401887_krp
  kk4(4,2) =-0.1669322780991597_krp
  kk4(4,3) =-0.1424549867673852_krp
  kk4(4,4) = 1.436257218449623_krp
  kk4(4,5) = 0.614599972828722_krp
  kk4(4,6) =-0.0046714839361168_krp
  kk4(4,7) = 0.1959882360123814_krp
  kk4(4,8) = 0.2862184400973558_krp
  kk4(4,9) =-0.2179819385402839_krp
  kk4(4,10)=-0.7674589818049491_krp
!Gauss point25 coefficients
  kk4(5,1) = 0.8288889984051868_krp
  kk4(5,2) = 0.04677743023354147_krp
  kk4(5,3) =-0.03991604821659466_krp
  kk4(5,4) = 0.5446044655419009_krp
  kk4(5,5) =-0.1890638174450713_krp
  kk4(5,6) = 0.05059205267244342_krp
  kk4(5,7) =-0.01233887391294805_krp
  kk4(5,8) = 0.1383331792538543_krp
  kk4(5,9) =-0.2904387454639018_krp
  kk4(5,10)=-0.07743864106841123_krp
!Gauss point26 coefficients
  kk4(6,1) = 0.7347191718430301_krp
  kk4(6,2) = 0.03089206178978147_krp
  kk4(6,3) = 0.009587120631267871_krp
  kk4(6,4) = 0.2936886640989052_krp
  kk4(6,5) =-0.1126383616346481_krp
  kk4(6,6) = 0.02125598318219282_krp
  kk4(6,7) = 0.005363405730222937_krp
  kk4(6,8) =-0.03118350891894955_krp
  kk4(6,9) = 0.07902781467646672_krp
  kk4(6,10)=-0.03071235139826946_krp
!Gauss point27 coefficients
  kk4(7,1) =-0.2181559923787162_krp
  kk4(7,2) =-0.2181559923787162_krp
  kk4(7,3) =-0.1142031331798774_krp
  kk4(7,4) = 0.9929893462207865_krp
  kk4(7,5) = 0.9929893462207865_krp
  kk4(7,6) =-0.09934845070431942_krp
  kk4(7,7) = 0.1946263795788897_krp
  kk4(7,8) = 0.1946263795788897_krp
  kk4(7,9) =-0.09934845070431942_krp
  kk4(7,10)=-0.6260194322534035_krp
!Gauss point28 coefficients
  kk4(8,1) =-0.1845434719636343_krp
  kk4(8,2) =-0.1845434719636343_krp
  kk4(8,3) =-0.03907751185445954_krp
  kk4(8,4) = 0.8137520094270351_krp
  kk4(8,5) = 0.8137520094270351_krp
  kk4(8,6) =-0.07987703593857617_krp
  kk4(8,7) = 0.06772188147188028_krp
  kk4(8,8) = 0.06772188147188028_krp
  kk4(8,9) =-0.07987703593857617_krp
  kk4(8,10)=-0.1950292541389505_krp
!Gauss point43 coefficients
  kk4(9,1) = 0.4171428636300509_krp
  kk4(9,2) = 0.04469143885360190_krp
  kk4(9,3) = 0.04469143885360187_krp
  kk4(9,4) = 0.3809996057073503_krp
  kk4(9,5) =-0.1494978760930465_krp
  kk4(9,6) = 0.01247272805849735_krp
  kk4(9,7) = 0.01247272805849725_krp
  kk4(9,8) =-0.1494978760930462_krp
  kk4(9,9) = 0.3809996057073512_krp
  kk4(9,10)= 0.005525343317141464_krp
!Gauss point44 coefficients
  kk4(10,1) =-0.03372290429346719_krp
  kk4(10,2) = 0.08422845078964551_krp
  kk4(10,3) = 0.08422845078964559_krp
  kk4(10,4) = 0.5279195360563089_krp
  kk4(10,5) =-0.2373417331142653_krp
  kk4(10,6) =-0.05641319088329125_krp
  kk4(10,7) =-0.05641319088329131_krp
  kk4(10,8) =-0.2373417331142654_krp
  kk4(10,9) = 0.5279195360563111_krp
  kk4(10,10)= 0.3969367785966691_krp
!Gauss point49 coefficients
  kk4(11,1) =-0.1324880465205211_krp
  kk4(11,2) = 0.06839911609813847_krp
  kk4(11,3) = 0.09438486033397265_krp
  kk4(11,4) = 0.6574368789079342_krp
  kk4(11,5) =-0.1856491158592828_krp
  kk4(11,6) =-0.09050328282806565_krp
  kk4(11,7) =-0.1202638750179787_krp
  kk4(11,8) =-0.2214388485701374_krp
  kk4(11,9) = 0.2498587299410721_krp
  kk4(11,10)= 0.6802635835148674_krp
!Gauss point50 coefficients
  kk4(12,1) =-0.1717935614576200_krp
  kk4(12,2) =-0.1071850079229601_krp
  kk4(12,3) = 0.03092823558004528_krp
  kk4(12,4) = 0.7957181895895807_krp
  kk4(12,5) = 0.4221111503732683_krp
  kk4(12,6) =-0.05725763709499984_krp
  kk4(12,7) =-0.05361615040959700_krp
  kk4(12,8) =-0.05487803385310206_krp
  kk4(12,9) =-0.06065292890941012_krp
  kk4(12,10)= 0.2566257441047929_krp

  defsvm%interp_weights(1, 1:defsvm%ncv)  = (/ kk4(1,1), kk4(1,2), kk4(1,3) , kk4(1,4), kk4(1,5), kk4(1,6),kk4(1,7),kk4(1,8),kk4(1,9),kk4(1,10)/)!
  defsvm%interp_weights(8, 1:defsvm%ncv)  = (/ kk4(1,2), kk4(1,1), kk4(1,3) , kk4(1,5), kk4(1,4), kk4(1,9),kk4(1,8),kk4(1,7),kk4(1,6),kk4(1,10)/)
  defsvm%interp_weights(9, 1:defsvm%ncv)  = (/ kk4(1,3), kk4(1,1), kk4(1,2) , kk4(1,8), kk4(1,9), kk4(1,4),kk4(1,5),kk4(1,6),kk4(1,7),kk4(1,10)/)
  defsvm%interp_weights(16, 1:defsvm%ncv) = (/ kk4(1,3), kk4(1,2), kk4(1,1) , kk4(1,7), kk4(1,6), kk4(1,5),kk4(1,4),kk4(1,9),kk4(1,8),kk4(1,10)/)
  defsvm%interp_weights(17, 1:defsvm%ncv) = (/ kk4(1,2), kk4(1,3), kk4(1,1) , kk4(1,6), kk4(1,7), kk4(1,8),kk4(1,9),kk4(1,4),kk4(1,5),kk4(1,10)/)
  defsvm%interp_weights(24, 1:defsvm%ncv) = (/ kk4(1,1), kk4(1,3), kk4(1,2) , kk4(1,9), kk4(1,8), kk4(1,7),kk4(1,6),kk4(1,5),kk4(1,4),kk4(1,10)/)

  defsvm%interp_weights(2, 1:defsvm%ncv)  = (/ kk4(2,1), kk4(2,2), kk4(2,3) , kk4(2,4), kk4(2,5), kk4(2,6),kk4(2,7),kk4(2,8),kk4(2,9),kk4(2,10)/)!
  defsvm%interp_weights(7, 1:defsvm%ncv)  = (/ kk4(2,2), kk4(2,1), kk4(2,3) , kk4(2,5), kk4(2,4), kk4(2,9),kk4(2,8),kk4(2,7),kk4(2,6),kk4(2,10)/)
  defsvm%interp_weights(10, 1:defsvm%ncv) = (/ kk4(2,3), kk4(2,1), kk4(2,2) , kk4(2,8), kk4(2,9), kk4(2,4),kk4(2,5),kk4(2,6),kk4(2,7),kk4(2,10)/)
  defsvm%interp_weights(15, 1:defsvm%ncv) = (/ kk4(2,3), kk4(2,2), kk4(2,1) , kk4(2,7), kk4(2,6), kk4(2,5),kk4(2,4),kk4(2,9),kk4(2,8),kk4(2,10)/)
  defsvm%interp_weights(18, 1:defsvm%ncv) = (/ kk4(2,2), kk4(2,3), kk4(2,1) , kk4(2,6), kk4(2,7), kk4(2,8),kk4(2,9),kk4(2,4),kk4(2,5),kk4(2,10)/)
  defsvm%interp_weights(23, 1:defsvm%ncv) = (/ kk4(2,1), kk4(2,3), kk4(2,2) , kk4(2,9), kk4(2,8), kk4(2,7),kk4(2,6),kk4(2,5),kk4(2,4),kk4(2,10)/)

  defsvm%interp_weights(3, 1:defsvm%ncv)  = (/ kk4(3,1), kk4(3,2), kk4(3,3) , kk4(3,4), kk4(3,5), kk4(3,6),kk4(3,7),kk4(3,8),kk4(3,9),kk4(3,10)/)!
  defsvm%interp_weights(6, 1:defsvm%ncv)  = (/ kk4(3,2), kk4(3,1), kk4(3,3) , kk4(3,5), kk4(3,4), kk4(3,9),kk4(3,8),kk4(3,7),kk4(3,6),kk4(3,10)/)
  defsvm%interp_weights(11, 1:defsvm%ncv) = (/ kk4(3,3), kk4(3,1), kk4(3,2) , kk4(3,8), kk4(3,9), kk4(3,4),kk4(3,5),kk4(3,6),kk4(3,7),kk4(3,10)/)
  defsvm%interp_weights(14, 1:defsvm%ncv) = (/ kk4(3,3), kk4(3,2), kk4(3,1) , kk4(3,7), kk4(3,6), kk4(3,5),kk4(3,4),kk4(3,9),kk4(3,8),kk4(3,10)/)
  defsvm%interp_weights(19, 1:defsvm%ncv) = (/ kk4(3,2), kk4(3,3), kk4(3,1) , kk4(3,6), kk4(3,7), kk4(3,8),kk4(3,9),kk4(3,4),kk4(3,5),kk4(3,10)/)
  defsvm%interp_weights(22, 1:defsvm%ncv) = (/ kk4(3,1), kk4(3,3), kk4(3,2) , kk4(3,9), kk4(3,8), kk4(3,7),kk4(3,6),kk4(3,5),kk4(3,4),kk4(3,10)/)

  defsvm%interp_weights(4, 1:defsvm%ncv)  = (/ kk4(4,1), kk4(4,2), kk4(4,3) , kk4(4,4), kk4(4,5), kk4(4,6),kk4(4,7),kk4(4,8),kk4(4,9),kk4(4,10)/)!
  defsvm%interp_weights(5, 1:defsvm%ncv)  = (/ kk4(4,2), kk4(4,1), kk4(4,3) , kk4(4,5), kk4(4,4), kk4(4,9),kk4(4,8),kk4(4,7),kk4(4,6),kk4(4,10)/)
  defsvm%interp_weights(12, 1:defsvm%ncv) = (/ kk4(4,3), kk4(4,1), kk4(4,2) , kk4(4,8), kk4(4,9), kk4(4,4),kk4(4,5),kk4(4,6),kk4(4,7),kk4(4,10)/)
  defsvm%interp_weights(13, 1:defsvm%ncv) = (/ kk4(4,3), kk4(4,2), kk4(4,1) , kk4(4,7), kk4(4,6), kk4(4,5),kk4(4,4),kk4(4,9),kk4(4,8),kk4(4,10)/)
  defsvm%interp_weights(20, 1:defsvm%ncv) = (/ kk4(4,2), kk4(4,3), kk4(4,1) , kk4(4,6), kk4(4,7), kk4(4,8),kk4(4,9),kk4(4,4),kk4(4,5),kk4(4,10)/)
  defsvm%interp_weights(21, 1:defsvm%ncv) = (/ kk4(4,1), kk4(4,3), kk4(4,2) , kk4(4,9), kk4(4,8), kk4(4,7),kk4(4,6),kk4(4,5),kk4(4,4),kk4(4,10)/)

  defsvm%interp_weights(25, 1:defsvm%ncv) = (/ kk4(5,1), kk4(5,2), kk4(5,3) , kk4(5,4), kk4(5,5), kk4(5,6),kk4(5,7),kk4(5,8),kk4(5,9),kk4(5,10)/)!
  defsvm%interp_weights(29, 1:defsvm%ncv) = (/ kk4(5,2), kk4(5,1), kk4(5,3) , kk4(5,5), kk4(5,4), kk4(5,9),kk4(5,8),kk4(5,7),kk4(5,6),kk4(5,10)/)
  defsvm%interp_weights(31, 1:defsvm%ncv) = (/ kk4(5,3), kk4(5,1), kk4(5,2) , kk4(5,8), kk4(5,9), kk4(5,4),kk4(5,5),kk4(5,6),kk4(5,7),kk4(5,10)/)
  defsvm%interp_weights(35, 1:defsvm%ncv) = (/ kk4(5,3), kk4(5,2), kk4(5,1) , kk4(5,7), kk4(5,6), kk4(5,5),kk4(5,4),kk4(5,9),kk4(5,8),kk4(5,10)/)
  defsvm%interp_weights(37, 1:defsvm%ncv) = (/ kk4(5,2), kk4(5,3), kk4(5,1) , kk4(5,6), kk4(5,7), kk4(5,8),kk4(5,9),kk4(5,4),kk4(5,5),kk4(5,10)/)
  defsvm%interp_weights(41, 1:defsvm%ncv) = (/ kk4(5,1), kk4(5,3), kk4(5,2) , kk4(5,9), kk4(5,8), kk4(5,7),kk4(5,6),kk4(5,5),kk4(5,4),kk4(5,10)/)

  defsvm%interp_weights(26, 1:defsvm%ncv) = (/ kk4(6,1), kk4(6,2), kk4(6,3) , kk4(6,4), kk4(6,5), kk4(6,6),kk4(6,7),kk4(6,8),kk4(6,9),kk4(6,10)/)!
  defsvm%interp_weights(30, 1:defsvm%ncv) = (/ kk4(6,2), kk4(6,1), kk4(6,3) , kk4(6,5), kk4(6,4), kk4(6,9),kk4(6,8),kk4(6,7),kk4(6,6),kk4(6,10)/)
  defsvm%interp_weights(32, 1:defsvm%ncv) = (/ kk4(6,3), kk4(6,1), kk4(6,2) , kk4(6,8), kk4(6,9), kk4(6,4),kk4(6,5),kk4(6,6),kk4(6,7),kk4(6,10)/)
  defsvm%interp_weights(36, 1:defsvm%ncv) = (/ kk4(6,3), kk4(6,2), kk4(6,1) , kk4(6,7), kk4(6,6), kk4(6,5),kk4(6,4),kk4(6,9),kk4(6,8),kk4(6,10)/)
  defsvm%interp_weights(38, 1:defsvm%ncv) = (/ kk4(6,2), kk4(6,3), kk4(6,1) , kk4(6,6), kk4(6,7), kk4(6,8),kk4(6,9),kk4(6,4),kk4(6,5),kk4(6,10)/)
  defsvm%interp_weights(42, 1:defsvm%ncv) = (/ kk4(6,1), kk4(6,3), kk4(6,2) , kk4(6,9), kk4(6,8), kk4(6,7),kk4(6,6),kk4(6,5),kk4(6,4),kk4(6,10)/)

  defsvm%interp_weights(27, 1:defsvm%ncv) = (/ kk4(7,1), kk4(7,2), kk4(7,3) , kk4(7,4), kk4(7,5), kk4(7,6),kk4(7,7),kk4(7,8),kk4(7,9),kk4(7,10)/)!
  defsvm%interp_weights(33, 1:defsvm%ncv) = (/ kk4(7,3), kk4(7,1), kk4(7,2) , kk4(7,8), kk4(7,9), kk4(7,4),kk4(7,5),kk4(7,6),kk4(7,7),kk4(7,10)/)
  defsvm%interp_weights(39, 1:defsvm%ncv) = (/ kk4(7,1), kk4(7,3), kk4(7,2) , kk4(7,9), kk4(7,8), kk4(7,7),kk4(7,6),kk4(7,5),kk4(7,4),kk4(7,10)/)

  defsvm%interp_weights(28, 1:defsvm%ncv) = (/ kk4(8,1), kk4(8,2), kk4(8,3) , kk4(8,4), kk4(8,5), kk4(8,6),kk4(8,7),kk4(8,8),kk4(8,9),kk4(8,10)/)!
  defsvm%interp_weights(34, 1:defsvm%ncv) = (/ kk4(8,3), kk4(8,1), kk4(8,2) , kk4(8,8), kk4(8,9), kk4(8,4),kk4(8,5),kk4(8,6),kk4(8,7),kk4(8,10)/)
  defsvm%interp_weights(40, 1:defsvm%ncv) = (/ kk4(8,1), kk4(8,3), kk4(8,2) , kk4(8,9), kk4(8,8), kk4(8,7),kk4(8,6),kk4(8,5),kk4(8,4),kk4(8,10)/)


  defsvm%interp_weights(43, 1:defsvm%ncv) = (/ kk4(9,1), kk4(9,2), kk4(9,3) , kk4(9,4), kk4(9,5), kk4(9,6),kk4(9,7),kk4(9,8),kk4(9,9),kk4(9,10)/)!
  defsvm%interp_weights(45, 1:defsvm%ncv) = (/ kk4(9,3), kk4(9,1), kk4(9,2) , kk4(9,8), kk4(9,9), kk4(9,4),kk4(9,5),kk4(9,6),kk4(9,7),kk4(9,10)/)
  defsvm%interp_weights(47, 1:defsvm%ncv) = (/ kk4(9,2), kk4(9,3), kk4(9,1) , kk4(9,6), kk4(9,7), kk4(9,8),kk4(9,9),kk4(9,4),kk4(9,5),kk4(9,10)/)

  defsvm%interp_weights(44, 1:defsvm%ncv) = (/ kk4(10,1), kk4(10,2), kk4(10,3) , kk4(10,4), kk4(10,5), kk4(10,6),kk4(10,7),kk4(10,8),kk4(10,9),kk4(10,10)/)!
  defsvm%interp_weights(46, 1:defsvm%ncv) = (/ kk4(10,3), kk4(10,1), kk4(10,2) , kk4(10,8), kk4(10,9), kk4(10,4),kk4(10,5),kk4(10,6),kk4(10,7),kk4(10,10)/)
  defsvm%interp_weights(48, 1:defsvm%ncv) = (/ kk4(10,2), kk4(10,3), kk4(10,1) , kk4(10,6), kk4(10,7), kk4(10,8),kk4(10,9),kk4(10,4),kk4(10,5),kk4(10,10)/)

  defsvm%interp_weights(49, 1:defsvm%ncv) = (/ kk4(11,1), kk4(11,2), kk4(11,3) , kk4(11,4), kk4(11,5), kk4(11,6),kk4(11,7),kk4(11,8),kk4(11,9),kk4(11,10)/)!
  defsvm%interp_weights(52, 1:defsvm%ncv) = (/ kk4(11,2), kk4(11,1), kk4(11,3) , kk4(11,5), kk4(11,4), kk4(11,9),kk4(11,8),kk4(11,7),kk4(11,6),kk4(11,10)/)
  defsvm%interp_weights(53, 1:defsvm%ncv) = (/ kk4(11,3), kk4(11,1), kk4(11,2) , kk4(11,8), kk4(11,9), kk4(11,4),kk4(11,5),kk4(11,6),kk4(11,7),kk4(11,10)/)
  defsvm%interp_weights(56, 1:defsvm%ncv) = (/ kk4(11,3), kk4(11,2), kk4(11,1) , kk4(11,7), kk4(11,6), kk4(11,5),kk4(11,4),kk4(11,9),kk4(11,8),kk4(11,10)/)
  defsvm%interp_weights(57, 1:defsvm%ncv) = (/ kk4(11,2), kk4(11,3), kk4(11,1) , kk4(11,6), kk4(11,7), kk4(11,8),kk4(11,9),kk4(11,4),kk4(11,5),kk4(11,10)/)
  defsvm%interp_weights(60, 1:defsvm%ncv) = (/ kk4(11,1), kk4(11,3), kk4(11,2) , kk4(11,9), kk4(11,8), kk4(11,7),kk4(11,6),kk4(11,5),kk4(11,4),kk4(11,10)/)


  defsvm%interp_weights(50, 1:defsvm%ncv) = (/ kk4(12,1), kk4(12,2), kk4(12,3) , kk4(12,4), kk4(12,5), kk4(12,6),kk4(12,7),kk4(12,8),kk4(12,9),kk4(12,10)/)!
  defsvm%interp_weights(51, 1:defsvm%ncv) = (/ kk4(12,2), kk4(12,1), kk4(12,3) , kk4(12,5), kk4(12,4), kk4(12,9),kk4(12,8),kk4(12,7),kk4(12,6),kk4(12,10)/)
  defsvm%interp_weights(54, 1:defsvm%ncv) = (/ kk4(12,3), kk4(12,1), kk4(12,2) , kk4(12,8), kk4(12,9), kk4(12,4),kk4(12,5),kk4(12,6),kk4(12,7),kk4(12,10)/)
  defsvm%interp_weights(55, 1:defsvm%ncv) = (/ kk4(12,3), kk4(12,2), kk4(12,1) , kk4(12,7), kk4(12,6), kk4(12,5),kk4(12,4),kk4(12,9),kk4(12,8),kk4(12,10)/)
  defsvm%interp_weights(58, 1:defsvm%ncv) = (/ kk4(12,2), kk4(12,3), kk4(12,1) , kk4(12,6), kk4(12,7), kk4(12,8),kk4(12,9),kk4(12,4),kk4(12,5),kk4(12,10)/)
  defsvm%interp_weights(59, 1:defsvm%ncv) = (/ kk4(12,1), kk4(12,3), kk4(12,2) , kk4(12,9), kk4(12,8), kk4(12,7),kk4(12,6),kk4(12,5),kk4(12,4),kk4(12,10)/)

  if (size(defsvm%interp_weights, 1) /= 60) call erreur("SVM initialization", "bad array size")

case(svm_4kris2)   ! 12 independent points ... to be continued ...
  defsvm%ncv     = 10
  defsvm%ncvface = 9
  defsvm%nfgauss = 1
  allocate(defsvm%interp_weights(defsvm%ncvface*defsvm%nfgauss, defsvm%ncv))

  call error_stop("development: SVM4 Kris 2 method not implemented (init_svmweights)")

case default
  call error_stop("internal error: unknown SVM method (init_svmweights)")
endselect

endsubroutine init_svmweights

!-------------------------------------------------------------------------
! distribute SVM 3 coefficients on tri
!-------------------------------------------------------------------------
subroutine distrib_svmweights_tri3(defsvm, k)
implicit none
! -- parameters --
type(mnu_svm) :: defsvm
real(krp)     :: k(7,6)

  defsvm%interp_weights(  1, 1:defsvm%ncv) = (/ k(1,1), k(1,2), k(1,3) , k(1,4), k(1,5), k(1,6)/)!
  defsvm%interp_weights(  2, 1:defsvm%ncv) = (/ k(2,1), k(2,2), k(2,3) , k(2,4), k(2,5), k(2,6)/)!
  defsvm%interp_weights(  3, 1:defsvm%ncv) = (/ k(3,1), k(3,2), k(3,3) , k(3,4), k(3,5), k(3,6)/)!

  defsvm%interp_weights(  4, 1:defsvm%ncv) = (/ k(3,2), k(3,1), k(3,3) , k(3,4), k(3,6), k(3,5)/)
  defsvm%interp_weights(  5, 1:defsvm%ncv) = (/ k(2,2), k(2,1), k(2,3) , k(2,4), k(2,6), k(2,5)/)
  defsvm%interp_weights(  6, 1:defsvm%ncv) = (/ k(1,2), k(1,1), k(1,3) , k(1,4), k(1,6), k(1,5)/)
  defsvm%interp_weights(  7, 1:defsvm%ncv) = (/ k(1,3), k(1,1), k(1,2) , k(1,6), k(1,4), k(1,5)/)
  defsvm%interp_weights(  8, 1:defsvm%ncv) = (/ k(2,3), k(2,1), k(2,2) , k(2,6), k(2,4), k(2,5)/)
  defsvm%interp_weights(  9, 1:defsvm%ncv) = (/ k(3,3), k(3,1), k(3,2) , k(3,6), k(3,4), k(3,5)/)
  defsvm%interp_weights( 10, 1:defsvm%ncv) = (/ k(3,3), k(3,2), k(3,1) , k(3,5), k(3,4), k(3,6)/)
  defsvm%interp_weights( 11, 1:defsvm%ncv) = (/ k(2,3), k(2,2), k(2,1) , k(2,5), k(2,4), k(2,6)/)
  defsvm%interp_weights( 12, 1:defsvm%ncv) = (/ k(1,3), k(1,2), k(1,1) , k(1,5), k(1,4), k(1,6)/)
  defsvm%interp_weights( 13, 1:defsvm%ncv) = (/ k(1,2), k(1,3), k(1,1) , k(1,5), k(1,6), k(1,4)/)
  defsvm%interp_weights( 14, 1:defsvm%ncv) = (/ k(2,2), k(2,3), k(2,1) , k(2,5), k(2,6), k(2,4)/)
  defsvm%interp_weights( 15, 1:defsvm%ncv) = (/ k(3,2), k(3,3), k(3,1) , k(3,5), k(3,6), k(3,4)/)
  defsvm%interp_weights( 16, 1:defsvm%ncv) = (/ k(3,1), k(3,3), k(3,2) , k(3,6), k(3,5), k(3,4)/)
  defsvm%interp_weights( 17, 1:defsvm%ncv) = (/ k(2,1), k(2,3), k(2,2) , k(2,6), k(2,5), k(2,4)/)
  defsvm%interp_weights( 18, 1:defsvm%ncv) = (/ k(1,1), k(1,3), k(1,2) , k(1,6), k(1,5), k(1,4)/)

  defsvm%interp_weights( 19, 1:defsvm%ncv) = (/ k(4,1), k(4,2), k(4,3) , k(4,4), k(4,5), k(4,6)/)!
  defsvm%interp_weights( 20, 1:defsvm%ncv) = (/ k(5,1), k(5,2), k(5,3) , k(5,4), k(5,5), k(5,6)/)!

  defsvm%interp_weights( 21, 1:defsvm%ncv) = (/ k(4,2), k(4,1), k(4,3) , k(4,4), k(4,6), k(4,5)/)
  defsvm%interp_weights( 22, 1:defsvm%ncv) = (/ k(5,2), k(5,1), k(5,3) , k(5,4), k(5,6), k(5,5)/)
  defsvm%interp_weights( 23, 1:defsvm%ncv) = (/ k(4,3), k(4,1), k(4,2) , k(4,6), k(4,4), k(4,5)/)
  defsvm%interp_weights( 24, 1:defsvm%ncv) = (/ k(5,3), k(5,1), k(5,2) , k(5,6), k(5,4), k(5,5)/)
  defsvm%interp_weights( 25, 1:defsvm%ncv) = (/ k(4,3), k(4,2), k(4,1) , k(4,5), k(4,4), k(4,6)/)
  defsvm%interp_weights( 26, 1:defsvm%ncv) = (/ k(5,3), k(5,2), k(5,1) , k(5,5), k(5,4), k(5,6)/)
  defsvm%interp_weights( 27, 1:defsvm%ncv) = (/ k(4,2), k(4,3), k(4,1) , k(4,5), k(4,6), k(4,4)/)
  defsvm%interp_weights( 28, 1:defsvm%ncv) = (/ k(5,2), k(5,3), k(5,1) , k(5,5), k(5,6), k(5,4)/)
  defsvm%interp_weights( 29, 1:defsvm%ncv) = (/ k(4,1), k(4,3), k(4,2) , k(4,6), k(4,5), k(4,4)/)
  defsvm%interp_weights( 30, 1:defsvm%ncv) = (/ k(5,1), k(5,3), k(5,2) , k(5,6), k(5,5), k(5,4)/)

  defsvm%interp_weights( 31, 1:defsvm%ncv) = (/ k(6,1), k(6,2), k(6,3) , k(6,4), k(6,5), k(6,6)/)!
  defsvm%interp_weights( 32, 1:defsvm%ncv) = (/ k(7,1), k(7,2), k(7,3) , k(7,4), k(7,5), k(7,6)/)!

  defsvm%interp_weights( 33, 1:defsvm%ncv) = (/ k(6,2), k(6,1), k(6,3) , k(6,4), k(6,6), k(6,5)/)
  defsvm%interp_weights( 34, 1:defsvm%ncv) = (/ k(7,2), k(7,1), k(7,3) , k(7,4), k(7,6), k(7,5)/)
  defsvm%interp_weights( 35, 1:defsvm%ncv) = (/ k(6,3), k(6,2), k(6,1) , k(6,5), k(6,4), k(6,6)/)
  defsvm%interp_weights( 36, 1:defsvm%ncv) = (/ k(7,3), k(7,2), k(7,1) , k(7,5), k(7,4), k(7,6)/)

  if (size(defsvm%interp_weights, 1) /= 36) call error_stop("SVM initialization: bad array size")
endsubroutine  

!-------------------------------------------------------------------------
! init SVM gradient weights
!-------------------------------------------------------------------------
subroutine init_gradsvmweights(defsvm)
implicit none
! -- parameters --
type(mnu_svm) :: defsvm
! -- internal --
integer(kpp) :: i
real(krp) :: k(7, 6, 2)
real(krp) :: k4(10, 10, 2)
real(krp) :: kk4(12, 10, 2)

! -- BODY --

allocate(defsvm%grad_interp_weights(1:defsvm%ncvface*defsvm%nfgauss, defsvm%ncv, 2))

select case(defsvm%sv_method)

! 36 Gauss pts, 6 coeff per point, 7 independent points
case(svm_3wang) !weights for alpha=1/4 and beta =1/3 : ORIGINAL PARTITION BY WANG

!Gauss point1
  k(1,1,1:2) = (/-5.751340285 ,-3.297029136 /)
  k(1,2,1:2) = (/-2.023184945 ,1.589555599 /)
  k(1,3,1:2) = (/0.3446439020 ,-3.088974854 /)
  k(1,4,1:2) = (/9.016846272 ,-5.634662673 /)
  k(1,5,1:2) = (/-1.842026161 ,-.4484404109 /)
  k(1,6,1:2) = (/0.2550612173 ,10.87955148 /)
!Gauss point2 coefficients
  k(2,1,1:2) = (/-4.496605943 ,-2.508380890 /)
  k(2,2,1:2) = (/-.7684506031 ,.800907353 /)
  k(2,3,1:2) = (/.2333994140 ,-3.088974854 /)
  k(2,4,1:2) = (/6.106380016 ,-5.634662673 /)
  k(2,5,1:2) = (/-1.585905131 ,1.379789613 /)
  k(2,6,1:2) = (/.5111822479 ,9.051321454 /)
!Gauss point3 coefficients
  k(3,1,1:2) = (/-3.118812012 ,-1.642385013 /)
  k(3,2,1:2) = (/.609343328 ,-.065088524 /)
  k(3,3,1:2) =  (/.1112444880 ,-3.088974854 /)
  k(3,4,1:2) = (/2.910466256 ,-5.634662673 /)
  k(3,5,1:2) = (/-1.304664720 ,3.387325509 /)
  k(3,6,1:2) = (/.7924226584 ,7.043785558 /)
 !Gauss point19 coefficients
  k(4,1,1:2) = (/-3.870680315 ,-2.147002561 /)
  k(4,2,1:2) = (/-.4758469438 ,.5849550990 /)
  k(4,3,1:2) = (/.1926811053 ,-2.727596524 /)
  k(4,4,1:2) = (/5.041075429 ,-5.357478387 /)
  k(4,5,1:2) = (/-1.105807864 ,1.656973898 /)
  k(4,6,1:2) = (/.2185785882 ,7.990148473 /)
!Gauss point20 coefficients 
  k(5,1,1:2) = (/-3.415354039 ,-1.948346848 /)
  k(5,2,1:2) = (/-.9311732206 ,.7836108122 /)
  k(5,3,1:2) = (/.1926811053 ,-1.740292566 /)
  k(5,4,1:2) = (/5.041075429 ,-4.600196833 /)
  k(5,5,1:2) = (/-.0502787680 ,.586025429 /)
  k(5,6,1:2) = (/-.8369505076 ,6.919200004 /)
!Gauss point31 coefficients 
  k(6,1,1:2) = (/-2.622767426 ,-1.514255480 /)
  k(6,2,1:2) = (/-.8052305455 ,.6403715957 /)
  k(6,3,1:2) = (/.1519627967 ,-1.017535906 /)
  k(6,4,1:2) = (/3.975770843 ,-4.045828261 /)
  k(6,5,1:2) = (/.816168964 ,.4712153702 /)
  k(6,6,1:2) = (/-1.515904632 ,5.466032681 /)
!Gauss point32 coefficients
  k(7,1,1:2) = (/-.912706808 ,-.5269515211 /)
  k(7,2,1:2) = (/-.005822480 ,.0503790629 /)
  k(7,3,1:2) = (/.04071830863 ,-.030231948 /)
  k(7,4,1:2) = (/1.065304587 ,-3.288546707 /)
  k(7,5,1:2) = (/2.127819089 ,1.228496925 /)
  k(7,6,1:2) = (/-2.315312696 ,2.566854188 /)
 
  call distrib_svmgradweights_tri3(defsvm, k)

case(svm_3kris) !weights for alpha=91/1000 and beta=18/100 : OPTIMISED PARTITION BY ABEELE

!Gauss point1
  k(1,1,1:2) = (/-4.356795967 ,-2.501654679 /)
  k(1,2,1:2) = (/-1.708340643 ,1.687193540 /)
  k(1,3,1:2) = (/.5950806640 ,-2.470001255 /)
  k(1,4,1:2) = (/8.216144480 ,-6.070980043 /)
  k(1,5,1:2) = (/-1.815780046 ,-.8585994887 /)
  k(1,6,1:2) = (/-.9303084890 ,10.21404192 /)
!Gauss point2 coefficients 
  k(2,1,1:2) = (/-4.025394440 ,-2.272774312 /)
  k(2,2,1:2) = (/-1.376939116 ,1.458313173 /)
  k(2,3,1:2) = (/.5300497658 ,-2.470001255 /)
  k(2,4,1:2) = (/7.318277540 ,-6.070980043 /)
  k(2,5,1:2) = (/-1.665732654 ,-.2535858661 /)
  k(2,6,1:2) = (/-.7802610964 ,9.609028302 /)
!Gauss point3 coefficients 
  k(3,1,1:2) = (/-2.813713645 ,-1.435934639 /)
  k(3,2,1:2) = (/-.165258321 ,.621473500 /)
  k(3,3,1:2) = (/.2922817292 ,-2.470001255 /)
  k(3,4,1:2) = (/4.035467899 ,-6.070980043 /)
  k(3,5,1:2) = (/-1.117124609 ,1.958484168 /)
  k(3,6,1:2) = (/-.2316530517 ,7.396958268 /)
 !Gauss point19 coefficients 
  k(4,1,1:2) = (/-3.773686954 ,-2.127450927 /)
  k(4,2,1:2) = (/-1.268741415 ,1.355070318 /)
  k(4,3,1:2) = (/.4947376812 ,-2.324677870 /)
  k(4,4,1:2) = (/6.830731553 ,-5.914431180 /)
  k(4,5,1:2) = (/-1.394582069 ,-.0970370031 /)
  k(4,6,1:2) = (/-.8884587971 ,9.108526661 /)
!Gauss point20 coefficients 
  k(5,1,1:2) = (/-3.417410840 ,-1.959300424 /)
  k(5,2,1:2) = (/-1.304541327 ,1.301885961 /)
  k(5,3,1:2) = (/.4632941700 ,-1.927647000 /)
  k(5,4,1:2) = (/6.396598087 ,-5.486731731 /)
  k(5,5,1:2) = (/-.8038322855 ,-.2743511768 /)
  k(5,6,1:2) = (/-1.334107805 ,8.346144369 /)
!Gauss point31 coefficients
  k(6,1,1:2) = (/-2.378342388 ,-1.504841695 /)
  k(6,2,1:2) = (/-1.253230362 ,1.003281046 /)
  k(6,3,1:2) = (/.3563116320 ,-1.389412240 /)
  k(6,4,1:2) = (/4.919514322 ,-4.906921124 /)
  k(6,5,1:2) = (/.748511370 ,.0840090729 /)
  k(6,6,1:2) = (/-2.392764573 ,6.713884939 /)
!Gauss point32 coefficients
  k(7,1,1:2) = (/-.686061150 ,-.4313878593 /)
  k(7,2,1:2) = (/-.2870158359 ,.2406616057 /)
  k(7,3,1:2) = (/.0954734140 ,-.315958405 /)
  k(7,4,1:2) = (/1.318179888 ,-3.750548541 /)
  k(7,5,1:2) = (/2.309978322 ,1.240381656 /)
  k(7,6,1:2) = (/-2.750554639 ,3.016851542 /)
 
  call distrib_svmgradweights_tri3(defsvm, k)

case(svm_4wang)   ! 10 independent points
  !Gauss point1 coefficients
  k4(1,1,1:2) =(/-9.8865833  , -5.6835032/)
  k4(1,2,1:2) =(/2.1803442   , -1.2845983/)
  k4(1,3,1:2) =(/-.043556227 , 2.8979661 /)
  k4(1,4,1:2) =(/13.895132   , -7.1458566/)
  k4(1,5,1:2) =(/-6.1648763  , 3.5409676 /)
  k4(1,6,1:2) =(/.39822952   , .030248573/)
  k4(1,7,1:2) =(/.40628618   , .030267319/)
  k4(1,8,1:2) =(/-.026156644 , -7.9486713/)
  k4(1,9,1:2) =(/1.3118652   , 15.728525 /)
  k4(1,10,1:2)=(/-2.0706844  , -.16534599/)
!Gauss point2 coefficients           
  k4(2,1,1:2) =(/-8.5550812  , -4.8514533/)
  k4(2,2,1:2) =(/1.4592683   , -.80099442/)
  k4(2,3,1:2) =(/-.040106053 , 2.9632647 /)
  k4(2,4,1:2) =(/11.380679   , -7.4123379/)
  k4(2,5,1:2) =(/-4.2627520  , 2.4279620 /)
  k4(2,6,1:2) =(/.33605575   , -.46263949/)
  k4(2,7,1:2) =(/.38869959   , -.54885300/)
  k4(2,8,1:2) =(/-.038680844 , -7.8957643/)
  k4(2,9,1:2) =(/1.2385791   , 13.992146 /)
  k4(2,10,1:2)=(/-1.9066614  , 2.5886692 /)
!Gauss point3 coefficients
  k4(3,1,1:2) =(/-5.3349926  , -2.8462315/)
  k4(3,2,1:2) =(/-.08508374  , .24767703 /)
  k4(3,3,1:2) =(/-.030634667 , 3.1148359 /)
  k4(3,4,1:2) =(/5.4143228   , -7.8514339/)
  k4(3,5,1:2) =(/.0226448    , -.33500460/)
  k4(3,6,1:2) =(/.16302110   , -1.3431152/)
  k4(3,7,1:2) =(/.34149420   , -2.0270934/)
  k4(3,8,1:2) =(/-.074135373 , -7.6389691/)
  k4(3,9,1:2) =(/1.0397503   , 9.6980500 /)
  k4(3,10,1:2)=(/-1.4563871  , 8.9812850 /)
 !Gauss point4 coefficients
  k4(4,1,1:2) =(/-.2539137    ,.2714454   /)
  k4(4,2,1:2) =(/-1.1983914   ,1.1004554  /)
  k4(4,3,1:2) =(/-.008208535  ,3.3118979  /)
  k4(4,4,1:2) =(/-3.240045    ,-7.1817765 /)
  k4(4,5,1:2) =(/4.696877     ,-5.1677563 /)
  k4(4,6,1:2) =(/-.26045597   ,-.6655735  /)
  k4(4,7,1:2) =(/.23599527    ,-4.8752007 /)
  k4(4,8,1:2) =(/-.16435669   ,-6.3788983 /)
  k4(4,9,1:2) =(/.58273759    ,2.2928976  /)
  k4(4,10,1:2)=(/-.39023774   ,17.292509  /)
!Gauss point25 coefficients
  k4(5,1,1:2) =(/-7.6157415   ,-4.3118182 /)
  k4(5,2,1:2) =(/1.2360375    ,-.65078451 /)
  k4(5,3,1:2) =(/-.019303859  ,2.6419259  /)
  k4(5,4,1:2) =(/10.011044    ,-7.1826286 /)
  k4(5,5,1:2) =(/-3.6287545   ,2.0528825  /)
  k4(5,6,1:2) =(/.17199691    ,-.53221046 /)
  k4(5,7,1:2) =(/.20683057    ,-.62211450 /)
  k4(5,8,1:2) =(/-.025714399  ,-7.0552264 /)
  k4(5,9,1:2) =(/.68275735    ,12.672474  /)
  k4(5,10,1:2)=(/-1.0191524   ,2.9874992  /)
!Gauss point26 coefficients
  k4(6,1,1:2) =(/-6.3841055   ,-3.6640395 /)
  k4(6,2,1:2) =(/1.2862341    ,-.68511006 /)
  k4(6,3,1:2) =(/.030892913   ,1.7709898  /)
  k4(6,4,1:2) =(/8.7137211    ,-6.3585505 /)
  k4(6,5,1:2) =(/-3.6615990   ,2.0947616  /)
  k4(6,6,1:2) =(/-.20082600   ,-.29947279 /)
  k4(6,7,1:2) =(/-.21837812   ,-.28963715 /)
  k4(6,8,1:2) =(/-.006173169  ,-4.9537265 /)
  k4(6,9,1:2) =(/-.66695166   ,10.828962  /)
  k4(6,10,1:2)=(/1.1071853    ,1.5558240  /)
!Gauss point27 coefficients
  k4(7,1,1:2) =(/.76561659   ,.84633076  /)
  k4(7,2,1:2) =(/-.76561659  ,.84633076  /)
  k4(7,3,1:2) =(/ 0.         ,2.9722406  /)
  k4(7,4,1:2) =(/-4.3507790  ,-6.0539558 /)
  k4(7,5,1:2) =(/4.3507790   ,-6.0539558 /)
  k4(7,6,1:2) =(/-.22727402  ,.43173215  /)
  k4(7,7,1:2) =(/.10346834   ,-5.1696014 /)
  k4(7,8,1:2) =(/-.10346834  ,-5.1696014 /)
  k4(7,9,1:2) =(/.22727402   ,.43173215  /)
  k4(7,10,1:2)=(/ 0.         ,16.918747  /)
!Gauss point28 coefficients
  k4(8,1,1:2) =(/.89817150   ,.85938460  /)
  k4(8,2,1:2) =(/-.89817150  ,.85938460  /)
  k4(8,3,1:2) =(/ 0.         ,2.0721534  /)
  k4(8,4,1:2) =(/-3.9971753  ,-5.3130562 /)
  k4(8,5,1:2) =(/3.9971753   ,-5.3130562 /)
  k4(8,6,1:2) =(/.25868858   ,.12394829  /)
  k4(8,7,1:2) =(/-.11777662  ,-3.8351545 /)
  k4(8,8,1:2) =(/.11777662   ,-3.8351545 /)
  k4(8,9,1:2) =(/-.25868858  ,.12394829  /)
  k4(8,10,1:2)=(/ 0.         ,14.257601  /)
!Gauss point43 coefficients
  k4(9,1,1:2) =(/-3.7362887   ,-2.0413472 /)
  k4(9,2,1:2) =(/.06722873    ,.12078950  /)
  k4(9,3,1:2) =(/.037935400   ,1.5872451  /)
  k4(9,4,1:2) =(/4.0561629    ,-6.3656452 /)
  k4(9,5,1:2) =(/-.4362381    ,.08228027  /)
  k4(9,6,1:2) =(/-.16910588   ,-.9942007  /)
  k4(9,7,1:2) =(/-.32000671   ,-1.0505723 /)
  k4(9,8,1:2) =(/.033213518   ,-4.2459181 /)
  k4(9,9,1:2) =(/-.98114509   ,7.2107522  /)
  k4(9,10,1:2)=(/1.4482441    ,5.6966160  /)
!Gauss point44 coefficients
  k4(10,1,1:2) =(/.2457092    ,.4416047   /)
  k4(10,2,1:2) =(/-1.2288310  ,1.0209473  /)
  k4(10,3,1:2) =(/.010164760  ,1.7551560  /)
  k4(10,4,1:2) =(/-2.947279   ,-5.7791667 /)
  k4(10,5,1:2) =(/3.917235    ,-4.0514505 /)
  k4(10,6,1:2) =(/.26620259   ,-.8571806  /)
  k4(10,7,1:2) =(/-.22154192  ,-2.8967098 /)
  k4(10,8,1:2) =(/.14469592   ,-3.7529003 /)
  k4(10,9,1:2) =(/-.57441141  ,1.3413305  /)
  k4(10,10,1:2)=(/.38805585   ,12.778370  /)

  call distrib_svmgradweights_tri4wang(defsvm, kk4)
  
case(svm_4kris)   ! 12 independent points
 !Gauss point1 coefficients
  kk4(1,1,1:2) =(/-8.8134792  , -5.0076949  /)
  kk4(1,2,1:2) =(/1.3368011   , -1.2156379  /)
  kk4(1,3,1:2) =(/-.45432143  , 2.1986910   /)
  kk4(1,4,1:2) =(/14.638626   , -10.393476  /)
  kk4(1,5,1:2) =(/-5.4193208  , 4.6260857   /)
  kk4(1,6,1:2) =(/1.6948153   , -.91594220  /)
  kk4(1,7,1:2) =(/.36463723   , 1.6422402   /)
  kk4(1,8,1:2) =(/1.2676350   , -8.0614289  /)
  kk4(1,9,1:2) =(/-.87272444  , 18.016237   /)
  kk4(1,10,1:2)=(/-3.7426690  , -.88907361  /)
!Gauss point2 coefficients
  kk4(2,1,1:2) =(/-7.4865112  , -4.0362328  /)
  kk4(2,2,1:2) =(/.70618894   , -.59735689  /)
  kk4(2,3,1:2) =(/-.41200727  , 2.4284578   /)
  kk4(2,4,1:2) =(/11.580180   , -10.518968  /)
  kk4(2,5,1:2) =(/-3.2195339  , 3.1017161   /)
  kk4(2,6,1:2) =(/1.2110805   , -1.3847338  /)
  kk4(2,7,1:2) =(/.46060733   , .73304134   /)
  kk4(2,8,1:2) =(/1.0196399   , -8.0668565  /)
  kk4(2,9,1:2) =(/-.46555671  , 15.784158   /)
  kk4(2,10,1:2)=(/-3.3940878  , 2.5567751   /)
!Gauss point3 coefficients
  kk4(3,1,1:2) =(/-4.7483824  , -2.0617023  /)
  kk4(3,2,1:2) =(/-.39806848  , .54849910   /)
  kk4(3,3,1:2) =(/-.31272485  , 2.8794497   /)
  kk4(3,4,1:2) =(/5.4123434   , -10.497057  /)
  kk4(3,5,1:2) =(/.9336170    , -.15858308  /)
  kk4(3,6,1:2) =(/.24691240   , -1.9667860  /)
  kk4(3,7,1:2) =(/.61982953   , -1.2248509  /)
  kk4(3,8,1:2) =(/.50371877   , -7.9042153  /)
  kk4(3,9,1:2) =(/.31896060   , 11.064875   /)
  kk4(3,10,1:2)=(/-2.5762059  , 9.3203704   /)
!Gauss point4 coefficients
  kk4(4,1,1:2) =(/-.4408945   , .8620208   /)
  kk4(4,2,1:2) =(/-.9380930   , 1.5614221  /)
  kk4(4,3,1:2) =(/-.08379437  , 3.4484163  /)
  kk4(4,4,1:2) =(/-3.420863   , -8.7556708 /)
  kk4(4,5,1:2) =(/5.121258    , -5.9854850 /)
  kk4(4,6,1:2) =(/-1.0632726  , -.5409207  /)
  kk4(4,7,1:2) =(/.63445905   , -4.8021028 /)
  kk4(4,8,1:2) =(/-.33340525  , -6.5918329 /)
  kk4(4,9,1:2) =(/1.2148978   , 2.9509025  /)
  kk4(4,10,1:2)=(/-.69029227  , 17.853250  /)
!Gauss point25 coefficients
  kk4(5,1,1:2) =(/-6.8343389  , -3.6671825  /)
  kk4(5,2,1:2) =(/.61643167   , -.48380934  /)
  kk4(5,3,1:2) =(/-.35207149  , 2.2459274   /)
  kk4(5,4,1:2) =(/10.511911   , -10.163165  /)
  kk4(5,5,1:2) =(/-2.7847165  , 2.7263035   /)
  kk4(5,6,1:2) =(/.96715183   , -1.3583055  /)
  kk4(5,7,1:2) =(/.31059649   , .53289489   /)
  kk4(5,8,1:2) =(/.93707392   , -7.4001255  /)
  kk4(5,9,1:2) =(/-.77212910  , 14.664371   /)
  kk4(5,10,1:2)=(/-2.5999089  , 2.9030905   /)
!Gauss point26 coefficients
  kk4(6,1,1:2) =(/-6.3214893  , -3.5764255  /)
  kk4(6,2,1:2) =(/.94377292   , -.73791741  /)
  kk4(6,3,1:2) =(/-.23063807  , 1.5714336   /)
  kk4(6,4,1:2) =(/10.463775   , -9.1436701  /)
  kk4(6,5,1:2) =(/-3.6085434  , 3.1469583   /)
  kk4(6,6,1:2) =(/.72595195   , -.89537883  /)
  kk4(6,7,1:2) =(/-.15292029  , .81721437   /)
  kk4(6,8,1:2) =(/.91720400   , -5.6512241  /)
  kk4(6,9,1:2) =(/-1.9583588  , 13.759069   /)
  kk4(6,10,1:2)=(/-.77875306  , .70993998   /)
!Gauss point27 coefficients
  kk4(7,1,1:2) =(/.56543801   , 1.2520638   /)
  kk4(7,2,1:2) =(/-.56543801  , 1.2520638   /)
  kk4(7,3,1:2) =(/ 0.         , 2.5569127   /)
  kk4(7,4,1:2) =(/-4.2402246  , -6.5365160  /)
  kk4(7,5,1:2) =(/4.2402246   , -6.5365160  /)
  kk4(7,6,1:2) =(/-.56840265  , .63977494   /)
  kk4(7,7,1:2) =(/.20257803   , -4.3784393  /)
  kk4(7,8,1:2) =(/-.20257803  , -4.3784393  /)
  kk4(7,9,1:2) =(/.56840265   , .63977494   /)
  kk4(7,10,1:2)=(/ 0.         , 15.489321   /)
!Gauss point28 coefficients
  kk4(8,1,1:2) =(/.47268392   , 1.3337394   /)
  kk4(8,2,1:2) =(/-.47268392  , 1.3337394   /)
  kk4(8,3,1:2) =(/0.          , 3.2314065   /)
  kk4(8,4,1:2) =(/-4.6280697  , -7.2565908  /)
  kk4(8,5,1:2) =(/4.6280697   , -7.2565908  /)
  kk4(8,6,1:2) =(/-1.0409177  , .86096256   /)
  kk4(8,7,1:2) =(/.42440145   , -5.3950497  /)
  kk4(8,8,1:2) =(/-.42440145  , -5.3950497  /)
  kk4(8,9,1:2) =(/1.0409177   , .86096256   /)
  kk4(8,10,1:2)=(/0.          , 17.682471   /)
!Gauss point43 coefficients
  kk4(9,1,1:2) =(/-4.7585583   , -2.7473548   /)
  kk4(9,2,1:2) =(/.82135212    , -.55867118   /)
  kk4(9,3,1:2) =(/-.073147440  , .99064737    /)
  kk4(9,4,1:2) =(/8.1157064    , -8.0210351   /)
  kk4(9,5,1:2) =(/-2.8736603   , 2.4582790    /)
  kk4(9,6,1:2) =(/.17071491    , -.72264096   /)
  kk4(9,7,1:2) =(/-.54046801   , .50916378    /)
  kk4(9,8,1:2) =(/.69210178    , -3.7178025   /)
  kk4(9,9,1:2) =(/-2.8885670   , 11.038926    /)
  kk4(9,10,1:2)=(/1.3345259    , .77048881    /)
!Gauss point44 coefficients
  kk4(10,1,1:2) =(/-1.7488650  , -1.0097076  /)
  kk4(10,2,1:2) =(/.21616307   , .032055195  /)
  kk4(10,3,1:2) =(/.13584207   , .17117520   /)
  kk4(10,4,1:2) =(/3.0382814   , -6.0160416  /)
  kk4(10,5,1:2) =(/-.61459679  , .60591074   /)
  kk4(10,6,1:2) =(/-.48411602  , -.71208049  /)
  kk4(10,7,1:2) =(/-.85873785  , -.06321678  /)
  kk4(10,8,1:2) =(/.21743568   , -.83521168  /)
  kk4(10,9,1:2) =(/-3.6909043  , 5.6392491   /)
  kk4(10,10,1:2)=(/3.7894981   , 2.1878677   /)
!Gauss point49 coefficients
  kk4(11,1,1:2) =(/-.52861197  , -.051543085  /)
  kk4(11,2,1:2) =(/-.50867133  , .65348846    /)
  kk4(11,3,1:2) =(/.09193354   , .39947557    /)
  kk4(11,4,1:2) =(/.03423639   , -5.7654937   /)
  kk4(11,5,1:2) =(/1.6060357   , -1.2839696   /)
  kk4(11,6,1:2) =(/-.25197412  , -1.0274205   /)
  kk4(11,7,1:2) =(/-.58420805  , -.64237828   /)
  kk4(11,8,1:2) =(/.15019533   , -1.1648397   /)
  kk4(11,9,1:2) =(/-2.5735452  , 3.2709550    /)
  kk4(11,10,1:2)=(/2.5646105   , 5.6117257    /)
!Gauss point50 coefficients
  kk4(12,1,1:2) =(/.34908743   , .96210027  /)
  kk4(12,2,1:2) =(/-.86360203  , 1.2481175  /)
  kk4(12,3,1:2) =(/-.00955891  , 1.7879145  /)
  kk4(12,4,1:2) =(/-3.2855627  , -6.3285209 /)
  kk4(12,5,1:2) =(/3.9705933   , -4.9590823 /)
  kk4(12,6,1:2) =(/-.19361326  , -.29622993 /)
  kk4(12,7,1:2) =(/-.00961532  , -2.9149587 /)
  kk4(12,8,1:2) =(/.02943082   , -3.4673390 /)
  kk4(12,9,1:2) =(/-.16155584  , 1.2407731  /)
  kk4(12,10,1:2)=(/.1743960    , 12.727225  /)

  call distrib_svmgradweights_tri4kris(defsvm, kk4)
  
case(svm_4kris2)   ! 12 independent points ... to be continued ...
  call error_stop("gradient weights not implemented for SVM4KRIS2 (init_gradsvmweights)")

case default
  call error_stop("internal error: unknown SVM method (init_gradsvmweights)")
endselect

endsubroutine init_gradsvmweights

!-------------------------------------------------------------------------
! distribute SVM 3 of gradient coefficients on tri
!-------------------------------------------------------------------------
subroutine distrib_svmgradweights_tri3(defsvm, k)
implicit none
! -- parameters --
type(mnu_svm) :: defsvm
real(krp)     :: k(7, 6, 2)   ! 7 faces, 6 CV, 2 Gauss-points
integer       :: i

do i=1,2
  defsvm%grad_interp_weights(  1, 1:defsvm%ncv,i) = (/ k(1,1,i), k(1,2,i), k(1,3,i) , k(1,4,i), k(1,5,i), k(1,6,i)/)!
  defsvm%grad_interp_weights(  2, 1:defsvm%ncv,i) = (/ k(2,1,i), k(2,2,i), k(2,3,i) , k(2,4,i), k(2,5,i), k(2,6,i)/)!
  defsvm%grad_interp_weights(  3, 1:defsvm%ncv,i) = (/ k(3,1,i), k(3,2,i), k(3,3,i) , k(3,4,i), k(3,5,i), k(3,6,i)/)!

  defsvm%grad_interp_weights(  4, 1:defsvm%ncv,i) = (/ k(3,2,i), k(3,1,i), k(3,3,i) , k(3,4,i), k(3,6,i), k(3,5,i)/)
  defsvm%grad_interp_weights(  5, 1:defsvm%ncv,i) = (/ k(2,2,i), k(2,1,i), k(2,3,i) , k(2,4,i), k(2,6,i), k(2,5,i)/)
  defsvm%grad_interp_weights(  6, 1:defsvm%ncv,i) = (/ k(1,2,i), k(1,1,i), k(1,3,i) , k(1,4,i), k(1,6,i), k(1,5,i)/)
  defsvm%grad_interp_weights(  7, 1:defsvm%ncv,i) = (/ k(1,3,i), k(1,1,i), k(1,2,i) , k(1,6,i), k(1,4,i), k(1,5,i)/)
  defsvm%grad_interp_weights(  8, 1:defsvm%ncv,i) = (/ k(2,3,i), k(2,1,i), k(2,2,i) , k(2,6,i), k(2,4,i), k(2,5,i)/)
  defsvm%grad_interp_weights(  9, 1:defsvm%ncv,i) = (/ k(3,3,i), k(3,1,i), k(3,2,i) , k(3,6,i), k(3,4,i), k(3,5,i)/)
  defsvm%grad_interp_weights( 10, 1:defsvm%ncv,i) = (/ k(3,3,i), k(3,2,i), k(3,1,i) , k(3,5,i), k(3,4,i), k(3,6,i)/)
  defsvm%grad_interp_weights( 11, 1:defsvm%ncv,i) = (/ k(2,3,i), k(2,2,i), k(2,1,i) , k(2,5,i), k(2,4,i), k(2,6,i)/)
  defsvm%grad_interp_weights( 12, 1:defsvm%ncv,i) = (/ k(1,3,i), k(1,2,i), k(1,1,i) , k(1,5,i), k(1,4,i), k(1,6,i)/)
  defsvm%grad_interp_weights( 13, 1:defsvm%ncv,i) = (/ k(1,2,i), k(1,3,i), k(1,1,i) , k(1,5,i), k(1,6,i), k(1,4,i)/)
  defsvm%grad_interp_weights( 14, 1:defsvm%ncv,i) = (/ k(2,2,i), k(2,3,i), k(2,1,i) , k(2,5,i), k(2,6,i), k(2,4,i)/)
  defsvm%grad_interp_weights( 15, 1:defsvm%ncv,i) = (/ k(3,2,i), k(3,3,i), k(3,1,i) , k(3,5,i), k(3,6,i), k(3,4,i)/)
  defsvm%grad_interp_weights( 16, 1:defsvm%ncv,i) = (/ k(3,1,i), k(3,3,i), k(3,2,i) , k(3,6,i), k(3,5,i), k(3,4,i)/)
  defsvm%grad_interp_weights( 17, 1:defsvm%ncv,i) = (/ k(2,1,i), k(2,3,i), k(2,2,i) , k(2,6,i), k(2,5,i), k(2,4,i)/)
  defsvm%grad_interp_weights( 18, 1:defsvm%ncv,i) = (/ k(1,1,i), k(1,3,i), k(1,2,i) , k(1,6,i), k(1,5,i), k(1,4,i)/)

  defsvm%grad_interp_weights( 19, 1:defsvm%ncv,i) = (/ k(4,1,i), k(4,2,i), k(4,3,i) , k(4,4,i), k(4,5,i), k(4,6,i)/)!
  defsvm%grad_interp_weights( 20, 1:defsvm%ncv,i) = (/ k(5,1,i), k(5,2,i), k(5,3,i) , k(5,4,i), k(5,5,i), k(5,6,i)/)!
                                                   
  defsvm%grad_interp_weights( 21, 1:defsvm%ncv,i) = (/ k(4,2,i), k(4,1,i), k(4,3,i) , k(4,4,i), k(4,6,i), k(4,5,i)/)
  defsvm%grad_interp_weights( 22, 1:defsvm%ncv,i) = (/ k(5,2,i), k(5,1,i), k(5,3,i) , k(5,4,i), k(5,6,i), k(5,5,i)/)
  defsvm%grad_interp_weights( 23, 1:defsvm%ncv,i) = (/ k(4,3,i), k(4,1,i), k(4,2,i) , k(4,6,i), k(4,4,i), k(4,5,i)/)
  defsvm%grad_interp_weights( 24, 1:defsvm%ncv,i) = (/ k(5,3,i), k(5,1,i), k(5,2,i) , k(5,6,i), k(5,4,i), k(5,5,i)/)
  defsvm%grad_interp_weights( 25, 1:defsvm%ncv,i) = (/ k(4,3,i), k(4,2,i), k(4,1,i) , k(4,5,i), k(4,4,i), k(4,6,i)/)
  defsvm%grad_interp_weights( 26, 1:defsvm%ncv,i) = (/ k(5,3,i), k(5,2,i), k(5,1,i) , k(5,5,i), k(5,4,i), k(5,6,i)/)
  defsvm%grad_interp_weights( 27, 1:defsvm%ncv,i) = (/ k(4,2,i), k(4,3,i), k(4,1,i) , k(4,5,i), k(4,6,i), k(4,4,i)/)
  defsvm%grad_interp_weights( 28, 1:defsvm%ncv,i) = (/ k(5,2,i), k(5,3,i), k(5,1,i) , k(5,5,i), k(5,6,i), k(5,4,i)/)
  defsvm%grad_interp_weights( 29, 1:defsvm%ncv,i) = (/ k(4,1,i), k(4,3,i), k(4,2,i) , k(4,6,i), k(4,5,i), k(4,4,i)/)
  defsvm%grad_interp_weights( 30, 1:defsvm%ncv,i) = (/ k(5,1,i), k(5,3,i), k(5,2,i) , k(5,6,i), k(5,5,i), k(5,4,i)/)
                                                   
  defsvm%grad_interp_weights( 31, 1:defsvm%ncv,i) = (/ k(6,1,i), k(6,2,i), k(6,3,i) , k(6,4,i), k(6,5,i), k(6,6,i)/)!
  defsvm%grad_interp_weights( 32, 1:defsvm%ncv,i) = (/ k(7,1,i), k(7,2,i), k(7,3,i) , k(7,4,i), k(7,5,i), k(7,6,i)/)!
                                                    
  defsvm%grad_interp_weights( 33, 1:defsvm%ncv,i) = (/ k(6,2,i), k(6,1,i), k(6,3,i) , k(6,4,i), k(6,6,i), k(6,5,i)/)
  defsvm%grad_interp_weights( 34, 1:defsvm%ncv,i) = (/ k(7,2,i), k(7,1,i), k(7,3,i) , k(7,4,i), k(7,6,i), k(7,5,i)/)
  defsvm%grad_interp_weights( 35, 1:defsvm%ncv,i) = (/ k(6,3,i), k(6,2,i), k(6,1,i) , k(6,5,i), k(6,4,i), k(6,6,i)/)
  defsvm%grad_interp_weights( 36, 1:defsvm%ncv,i) = (/ k(7,3,i), k(7,2,i), k(7,1,i) , k(7,5,i), k(7,4,i), k(7,6,i)/)
enddo

if (size(defsvm%grad_interp_weights, 1) /= 36) call error_stop("SVM initialization: bad array size (tri3)")

endsubroutine  

!-------------------------------------------------------------------------
! distribute SVM 4 of gradient coefficients on tri
!-------------------------------------------------------------------------
subroutine distrib_svmgradweights_tri4wang(defsvm, k4)
implicit none
! -- parameters --
type(mnu_svm) :: defsvm
real(krp)     :: k4(10,10,2)
integer       :: i

do i=1,2    
  defsvm%grad_interp_weights(1, 1:defsvm%ncv,i)  = (/ k4(1,1,i), k4(1,2,i), k4(1,3,i) , k4(1,4,i), k4(1,5,i), k4(1,6,i),k4(1,7,i),k4(1,8,i),k4(1,9,i),k4(1,10,i)/)!
  defsvm%grad_interp_weights(8, 1:defsvm%ncv,i)  = (/ k4(1,2,i), k4(1,1,i), k4(1,3,i) , k4(1,5,i), k4(1,4,i), k4(1,9,i),k4(1,8,i),k4(1,7,i),k4(1,6,i),k4(1,10,i)/)
  defsvm%grad_interp_weights(9, 1:defsvm%ncv,i)  = (/ k4(1,3,i), k4(1,1,i), k4(1,2,i) , k4(1,8,i), k4(1,9,i), k4(1,4,i),k4(1,5,i),k4(1,6,i),k4(1,7,i),k4(1,10,i)/)
  defsvm%grad_interp_weights(16, 1:defsvm%ncv,i) = (/ k4(1,3,i), k4(1,2,i), k4(1,1,i) , k4(1,7,i), k4(1,6,i), k4(1,5,i),k4(1,4,i),k4(1,9,i),k4(1,8,i),k4(1,10,i)/)
  defsvm%grad_interp_weights(17, 1:defsvm%ncv,i) = (/ k4(1,2,i), k4(1,3,i), k4(1,1,i) , k4(1,6,i), k4(1,7,i), k4(1,8,i),k4(1,9,i),k4(1,4,i),k4(1,5,i),k4(1,10,i)/)
  defsvm%grad_interp_weights(24, 1:defsvm%ncv,i) = (/ k4(1,1,i), k4(1,3,i), k4(1,2,i) , k4(1,9,i), k4(1,8,i), k4(1,7,i),k4(1,6,i),k4(1,5,i),k4(1,4,i),k4(1,10,i)/)
         
  defsvm%grad_interp_weights(2, 1:defsvm%ncv,i)  = (/ k4(2,1,i), k4(2,2,i), k4(2,3,i) , k4(2,4,i), k4(2,5,i), k4(2,6,i),k4(2,7,i),k4(2,8,i),k4(2,9,i),k4(2,10,i)/)!
  defsvm%grad_interp_weights(7, 1:defsvm%ncv,i)  = (/ k4(2,2,i), k4(2,1,i), k4(2,3,i) , k4(2,5,i), k4(2,4,i), k4(2,9,i),k4(2,8,i),k4(2,7,i),k4(2,6,i),k4(2,10,i)/)
  defsvm%grad_interp_weights(10, 1:defsvm%ncv,i) = (/ k4(2,3,i), k4(2,1,i), k4(2,2,i) , k4(2,8,i), k4(2,9,i), k4(2,4,i),k4(2,5,i),k4(2,6,i),k4(2,7,i),k4(2,10,i)/)
  defsvm%grad_interp_weights(15, 1:defsvm%ncv,i) = (/ k4(2,3,i), k4(2,2,i), k4(2,1,i) , k4(2,7,i), k4(2,6,i), k4(2,5,i),k4(2,4,i),k4(2,9,i),k4(2,8,i),k4(2,10,i)/)
  defsvm%grad_interp_weights(18, 1:defsvm%ncv,i) = (/ k4(2,2,i), k4(2,3,i), k4(2,1,i) , k4(2,6,i), k4(2,7,i), k4(2,8,i),k4(2,9,i),k4(2,4,i),k4(2,5,i),k4(2,10,i)/)
  defsvm%grad_interp_weights(23, 1:defsvm%ncv,i) = (/ k4(2,1,i), k4(2,3,i), k4(2,2,i) , k4(2,9,i), k4(2,8,i), k4(2,7,i),k4(2,6,i),k4(2,5,i),k4(2,4,i),k4(2,10,i)/)

  defsvm%grad_interp_weights(3, 1:defsvm%ncv,i)  = (/ k4(3,1,i), k4(3,2,i), k4(3,3,i) , k4(3,4,i), k4(3,5,i), k4(3,6,i),k4(3,7,i),k4(3,8,i),k4(3,9,i),k4(3,10,i)/)!
  defsvm%grad_interp_weights(6, 1:defsvm%ncv,i)  = (/ k4(3,2,i), k4(3,1,i), k4(3,3,i) , k4(3,5,i), k4(3,4,i), k4(3,9,i),k4(3,8,i),k4(3,7,i),k4(3,6,i),k4(3,10,i)/)
  defsvm%grad_interp_weights(11, 1:defsvm%ncv,i) = (/ k4(3,3,i), k4(3,1,i), k4(3,2,i) , k4(3,8,i), k4(3,9,i), k4(3,4,i),k4(3,5,i),k4(3,6,i),k4(3,7,i),k4(3,10,i)/)
  defsvm%grad_interp_weights(14, 1:defsvm%ncv,i) = (/ k4(3,3,i), k4(3,2,i), k4(3,1,i) , k4(3,7,i), k4(3,6,i), k4(3,5,i),k4(3,4,i),k4(3,9,i),k4(3,8,i),k4(3,10,i)/)
  defsvm%grad_interp_weights(19, 1:defsvm%ncv,i) = (/ k4(3,2,i), k4(3,3,i), k4(3,1,i) , k4(3,6,i), k4(3,7,i), k4(3,8,i),k4(3,9,i),k4(3,4,i),k4(3,5,i),k4(3,10,i)/)
  defsvm%grad_interp_weights(22, 1:defsvm%ncv,i) = (/ k4(3,1,i), k4(3,3,i), k4(3,2,i) , k4(3,9,i), k4(3,8,i), k4(3,7,i),k4(3,6,i),k4(3,5,i),k4(3,4,i),k4(3,10,i)/)

  defsvm%grad_interp_weights(4, 1:defsvm%ncv,i)  = (/ k4(4,1,i), k4(4,2,i), k4(4,3,i) , k4(4,4,i), k4(4,5,i), k4(4,6,i),k4(4,7,i),k4(4,8,i),k4(4,9,i),k4(4,10,i)/)!
  defsvm%grad_interp_weights(5, 1:defsvm%ncv,i)  = (/ k4(4,2,i), k4(4,1,i), k4(4,3,i) , k4(4,5,i), k4(4,4,i), k4(4,9,i),k4(4,8,i),k4(4,7,i),k4(4,6,i),k4(4,10,i)/)
  defsvm%grad_interp_weights(12, 1:defsvm%ncv,i) = (/ k4(4,3,i), k4(4,1,i), k4(4,2,i) , k4(4,8,i), k4(4,9,i), k4(4,4,i),k4(4,5,i),k4(4,6,i),k4(4,7,i),k4(4,10,i)/)
  defsvm%grad_interp_weights(13, 1:defsvm%ncv,i) = (/ k4(4,3,i), k4(4,2,i), k4(4,1,i) , k4(4,7,i), k4(4,6,i), k4(4,5,i),k4(4,4,i),k4(4,9,i),k4(4,8,i),k4(4,10,i)/)
  defsvm%grad_interp_weights(20, 1:defsvm%ncv,i) = (/ k4(4,2,i), k4(4,3,i), k4(4,1,i) , k4(4,6,i), k4(4,7,i), k4(4,8,i),k4(4,9,i),k4(4,4,i),k4(4,5,i),k4(4,10,i)/)
  defsvm%grad_interp_weights(21, 1:defsvm%ncv,i) = (/ k4(4,1,i), k4(4,3,i), k4(4,2,i) , k4(4,9,i), k4(4,8,i), k4(4,7,i),k4(4,6,i),k4(4,5,i),k4(4,4,i),k4(4,10,i)/)

  defsvm%grad_interp_weights(25, 1:defsvm%ncv,i) = (/ k4(5,1,i), k4(5,2,i), k4(5,3,i) , k4(5,4,i), k4(5,5,i), k4(5,6,i),k4(5,7,i),k4(5,8,i),k4(5,9,i),k4(5,10,i)/)!
  defsvm%grad_interp_weights(29, 1:defsvm%ncv,i) = (/ k4(5,2,i), k4(5,1,i), k4(5,3,i) , k4(5,5,i), k4(5,4,i), k4(5,9,i),k4(5,8,i),k4(5,7,i),k4(5,6,i),k4(5,10,i)/)
  defsvm%grad_interp_weights(31, 1:defsvm%ncv,i) = (/ k4(5,3,i), k4(5,1,i), k4(5,2,i) , k4(5,8,i), k4(5,9,i), k4(5,4,i),k4(5,5,i),k4(5,6,i),k4(5,7,i),k4(5,10,i)/)
  defsvm%grad_interp_weights(35, 1:defsvm%ncv,i) = (/ k4(5,3,i), k4(5,2,i), k4(5,1,i) , k4(5,7,i), k4(5,6,i), k4(5,5,i),k4(5,4,i),k4(5,9,i),k4(5,8,i),k4(5,10,i)/)
  defsvm%grad_interp_weights(37, 1:defsvm%ncv,i) = (/ k4(5,2,i), k4(5,3,i), k4(5,1,i) , k4(5,6,i), k4(5,7,i), k4(5,8,i),k4(5,9,i),k4(5,4,i),k4(5,5,i),k4(5,10,i)/)
  defsvm%grad_interp_weights(41, 1:defsvm%ncv,i) = (/ k4(5,1,i), k4(5,3,i), k4(5,2,i) , k4(5,9,i), k4(5,8,i), k4(5,7,i),k4(5,6,i),k4(5,5,i),k4(5,4,i),k4(5,10,i)/)

  defsvm%grad_interp_weights(26, 1:defsvm%ncv,i) = (/ k4(6,1,i), k4(6,2,i), k4(6,3,i) , k4(6,4,i), k4(6,5,i), k4(6,6,i),k4(6,7,i),k4(6,8,i),k4(6,9,i),k4(6,10,i)/)!
  defsvm%grad_interp_weights(30, 1:defsvm%ncv,i) = (/ k4(6,2,i), k4(6,1,i), k4(6,3,i) , k4(6,5,i), k4(6,4,i), k4(6,9,i),k4(6,8,i),k4(6,7,i),k4(6,6,i),k4(6,10,i)/)
  defsvm%grad_interp_weights(32, 1:defsvm%ncv,i) = (/ k4(6,3,i), k4(6,1,i), k4(6,2,i) , k4(6,8,i), k4(6,9,i), k4(6,4,i),k4(6,5,i),k4(6,6,i),k4(6,7,i),k4(6,10,i)/)
  defsvm%grad_interp_weights(36, 1:defsvm%ncv,i) = (/ k4(6,3,i), k4(6,2,i), k4(6,1,i) , k4(6,7,i), k4(6,6,i), k4(6,5,i),k4(6,4,i),k4(6,9,i),k4(6,8,i),k4(6,10,i)/)
  defsvm%grad_interp_weights(38, 1:defsvm%ncv,i) = (/ k4(6,2,i), k4(6,3,i), k4(6,1,i) , k4(6,6,i), k4(6,7,i), k4(6,8,i),k4(6,9,i),k4(6,4,i),k4(6,5,i),k4(6,10,i)/)
  defsvm%grad_interp_weights(42, 1:defsvm%ncv,i) = (/ k4(6,1,i), k4(6,3,i), k4(6,2,i) , k4(6,9,i), k4(6,8,i), k4(6,7,i),k4(6,6,i),k4(6,5,i),k4(6,4,i),k4(6,10,i)/)

  defsvm%grad_interp_weights(27, 1:defsvm%ncv,i) = (/ k4(7,1,i), k4(7,2,i), k4(7,3,i) , k4(7,4,i), k4(7,5,i), k4(7,6,i),k4(7,7,i),k4(7,8,i),k4(7,9,i),k4(7,10,i)/)!
  defsvm%grad_interp_weights(33, 1:defsvm%ncv,i) = (/ k4(7,3,i), k4(7,1,i), k4(7,2,i) , k4(7,8,i), k4(7,9,i), k4(7,4,i),k4(7,5,i),k4(7,6,i),k4(7,7,i),k4(7,10,i)/)
  defsvm%grad_interp_weights(39, 1:defsvm%ncv,i) = (/ k4(7,1,i), k4(7,3,i), k4(7,2,i) , k4(7,9,i), k4(7,8,i), k4(7,7,i),k4(7,6,i),k4(7,5,i),k4(7,4,i),k4(7,10,i)/)
                                                               
  defsvm%grad_interp_weights(28, 1:defsvm%ncv,i) = (/ k4(8,1,i), k4(8,2,i), k4(8,3,i) , k4(8,4,i), k4(8,5,i), k4(8,6,i),k4(8,7,i),k4(8,8,i),k4(8,9,i),k4(8,10,i)/)!
  defsvm%grad_interp_weights(34, 1:defsvm%ncv,i) = (/ k4(8,3,i), k4(8,1,i), k4(8,2,i) , k4(8,8,i), k4(8,9,i), k4(8,4,i),k4(8,5,i),k4(8,6,i),k4(8,7,i),k4(8,10,i)/)
  defsvm%grad_interp_weights(40, 1:defsvm%ncv,i) = (/ k4(8,1,i), k4(8,3,i), k4(8,2,i) , k4(8,9,i), k4(8,8,i), k4(8,7,i),k4(8,6,i),k4(8,5,i),k4(8,4,i),k4(8,10,i)/)

  defsvm%grad_interp_weights(43, 1:defsvm%ncv,i) = (/ k4(9,1,i), k4(9,2,i), k4(9,3,i) , k4(9,4,i), k4(9,5,i), k4(9,6,i),k4(9,7,i),k4(9,8,i),k4(9,9,i),k4(9,10,i)/)!
  defsvm%grad_interp_weights(46, 1:defsvm%ncv,i) = (/ k4(9,2,i), k4(9,1,i), k4(9,3,i) , k4(9,5,i), k4(9,4,i), k4(9,9,i),k4(9,8,i),k4(9,7,i),k4(9,6,i),k4(9,10,i)/)
  defsvm%grad_interp_weights(47, 1:defsvm%ncv,i) = (/ k4(9,3,i), k4(9,1,i), k4(9,2,i) , k4(9,8,i), k4(9,9,i), k4(9,4,i),k4(9,5,i),k4(9,6,i),k4(9,7,i),k4(9,10,i)/)
  defsvm%grad_interp_weights(50, 1:defsvm%ncv,i) = (/ k4(9,3,i), k4(9,2,i), k4(9,1,i) , k4(9,7,i), k4(9,6,i), k4(9,5,i),k4(9,4,i),k4(9,9,i),k4(9,8,i),k4(9,10,i)/)
  defsvm%grad_interp_weights(51, 1:defsvm%ncv,i) = (/ k4(9,2,i), k4(9,3,i), k4(9,1,i) , k4(9,6,i), k4(9,7,i), k4(9,8,i),k4(9,9,i),k4(9,4,i),k4(9,5,i),k4(9,10,i)/)
  defsvm%grad_interp_weights(54, 1:defsvm%ncv,i) = (/ k4(9,1,i), k4(9,3,i), k4(9,2,i) , k4(9,9,i), k4(9,8,i), k4(9,7,i),k4(9,6,i),k4(9,5,i),k4(9,4,i),k4(9,10,i)/)
                                                  
  defsvm%grad_interp_weights(44, 1:defsvm%ncv,i) = (/ k4(10,1,i), k4(10,2,i), k4(10,3,i) , k4(10,4,i), k4(10,5,i), k4(10,6,i),k4(10,7,i),k4(10,8,i),k4(10,9,i),k4(10,10,i)/)!
  defsvm%grad_interp_weights(45, 1:defsvm%ncv,i) = (/ k4(10,2,i), k4(10,1,i), k4(10,3,i) , k4(10,5,i), k4(10,4,i), k4(10,9,i),k4(10,8,i),k4(10,7,i),k4(10,6,i),k4(10,10,i)/)
  defsvm%grad_interp_weights(48, 1:defsvm%ncv,i) = (/ k4(10,3,i), k4(10,1,i), k4(10,2,i) , k4(10,8,i), k4(10,9,i), k4(10,4,i),k4(10,5,i),k4(10,6,i),k4(10,7,i),k4(10,10,i)/)
  defsvm%grad_interp_weights(49, 1:defsvm%ncv,i) = (/ k4(10,3,i), k4(10,2,i), k4(10,1,i) , k4(10,7,i), k4(10,6,i), k4(10,5,i),k4(10,4,i),k4(10,9,i),k4(10,8,i),k4(10,10,i)/)
  defsvm%grad_interp_weights(52, 1:defsvm%ncv,i) = (/ k4(10,2,i), k4(10,3,i), k4(10,1,i) , k4(10,6,i), k4(10,7,i), k4(10,8,i),k4(10,9,i),k4(10,4,i),k4(10,5,i),k4(10,10,i)/)
  defsvm%grad_interp_weights(53, 1:defsvm%ncv,i) = (/ k4(10,1,i), k4(10,3,i), k4(10,2,i) , k4(10,9,i), k4(10,8,i), k4(10,7,i),k4(10,6,i),k4(10,5,i),k4(10,4,i),k4(10,10,i)/)

enddo
if (size(defsvm%grad_interp_weights, 1) /= 54) call error_stop("SVM initialization: bad array size (tri4wang)")

endsubroutine

!-------------------------------------------------------------------------
! distribute SVM 4 of gradient coefficients on tri
!-------------------------------------------------------------------------
subroutine distrib_svmgradweights_tri4kris(defsvm, kk4)
implicit none
! -- parameters --
type(mnu_svm) :: defsvm
real(krp)     :: kk4(12,10,2)
integer       :: i

do i=1,2
  defsvm%grad_interp_weights(1, 1:defsvm%ncv,i)  = (/ kk4(1,1,i), kk4(1,2,i), kk4(1,3,i) , kk4(1,4,i), kk4(1,5,i), kk4(1,6,i),kk4(1,7,i),kk4(1,8,i),kk4(1,9,i),kk4(1,10,i)/)!
  defsvm%grad_interp_weights(8, 1:defsvm%ncv,i)  = (/ kk4(1,2,i), kk4(1,1,i), kk4(1,3,i) , kk4(1,5,i), kk4(1,4,i), kk4(1,9,i),kk4(1,8,i),kk4(1,7,i),kk4(1,6,i),kk4(1,10,i)/)
  defsvm%grad_interp_weights(9, 1:defsvm%ncv,i)  = (/ kk4(1,3,i), kk4(1,1,i), kk4(1,2,i) , kk4(1,8,i), kk4(1,9,i), kk4(1,4,i),kk4(1,5,i),kk4(1,6,i),kk4(1,7,i),kk4(1,10,i)/)
  defsvm%grad_interp_weights(16, 1:defsvm%ncv,i) = (/ kk4(1,3,i), kk4(1,2,i), kk4(1,1,i) , kk4(1,7,i), kk4(1,6,i), kk4(1,5,i),kk4(1,4,i),kk4(1,9,i),kk4(1,8,i),kk4(1,10,i)/)
  defsvm%grad_interp_weights(17, 1:defsvm%ncv,i) = (/ kk4(1,2,i), kk4(1,3,i), kk4(1,1,i) , kk4(1,6,i), kk4(1,7,i), kk4(1,8,i),kk4(1,9,i),kk4(1,4,i),kk4(1,5,i),kk4(1,10,i)/)
  defsvm%grad_interp_weights(24, 1:defsvm%ncv,i) = (/ kk4(1,1,i), kk4(1,3,i), kk4(1,2,i) , kk4(1,9,i), kk4(1,8,i), kk4(1,7,i),kk4(1,6,i),kk4(1,5,i),kk4(1,4,i),kk4(1,10,i)/)

  defsvm%grad_interp_weights(2, 1:defsvm%ncv,i)  = (/ kk4(2,1,i), kk4(2,2,i), kk4(2,3,i) , kk4(2,4,i), kk4(2,5,i), kk4(2,6,i),kk4(2,7,i),kk4(2,8,i),kk4(2,9,i),kk4(2,10,i)/)!
  defsvm%grad_interp_weights(7, 1:defsvm%ncv,i)  = (/ kk4(2,2,i), kk4(2,1,i), kk4(2,3,i) , kk4(2,5,i), kk4(2,4,i), kk4(2,9,i),kk4(2,8,i),kk4(2,7,i),kk4(2,6,i),kk4(2,10,i)/)
  defsvm%grad_interp_weights(10, 1:defsvm%ncv,i) = (/ kk4(2,3,i), kk4(2,1,i), kk4(2,2,i) , kk4(2,8,i), kk4(2,9,i), kk4(2,4,i),kk4(2,5,i),kk4(2,6,i),kk4(2,7,i),kk4(2,10,i)/)
  defsvm%grad_interp_weights(15, 1:defsvm%ncv,i) = (/ kk4(2,3,i), kk4(2,2,i), kk4(2,1,i) , kk4(2,7,i), kk4(2,6,i), kk4(2,5,i),kk4(2,4,i),kk4(2,9,i),kk4(2,8,i),kk4(2,10,i)/)
  defsvm%grad_interp_weights(18, 1:defsvm%ncv,i) = (/ kk4(2,2,i), kk4(2,3,i), kk4(2,1,i) , kk4(2,6,i), kk4(2,7,i), kk4(2,8,i),kk4(2,9,i),kk4(2,4,i),kk4(2,5,i),kk4(2,10,i)/)
  defsvm%grad_interp_weights(23, 1:defsvm%ncv,i) = (/ kk4(2,1,i), kk4(2,3,i), kk4(2,2,i) , kk4(2,9,i), kk4(2,8,i), kk4(2,7,i),kk4(2,6,i),kk4(2,5,i),kk4(2,4,i),kk4(2,10,i)/)

  defsvm%grad_interp_weights(3, 1:defsvm%ncv,i)  = (/ kk4(3,1,i), kk4(3,2,i), kk4(3,3,i) , kk4(3,4,i), kk4(3,5,i), kk4(3,6,i),kk4(3,7,i),kk4(3,8,i),kk4(3,9,i),kk4(3,10,i)/)!
  defsvm%grad_interp_weights(6, 1:defsvm%ncv,i)  = (/ kk4(3,2,i), kk4(3,1,i), kk4(3,3,i) , kk4(3,5,i), kk4(3,4,i), kk4(3,9,i),kk4(3,8,i),kk4(3,7,i),kk4(3,6,i),kk4(3,10,i)/)
  defsvm%grad_interp_weights(11, 1:defsvm%ncv,i) = (/ kk4(3,3,i), kk4(3,1,i), kk4(3,2,i) , kk4(3,8,i), kk4(3,9,i), kk4(3,4,i),kk4(3,5,i),kk4(3,6,i),kk4(3,7,i),kk4(3,10,i)/)
  defsvm%grad_interp_weights(14, 1:defsvm%ncv,i) = (/ kk4(3,3,i), kk4(3,2,i), kk4(3,1,i) , kk4(3,7,i), kk4(3,6,i), kk4(3,5,i),kk4(3,4,i),kk4(3,9,i),kk4(3,8,i),kk4(3,10,i)/)
  defsvm%grad_interp_weights(19, 1:defsvm%ncv,i) = (/ kk4(3,2,i), kk4(3,3,i), kk4(3,1,i) , kk4(3,6,i), kk4(3,7,i), kk4(3,8,i),kk4(3,9,i),kk4(3,4,i),kk4(3,5,i),kk4(3,10,i)/)
  defsvm%grad_interp_weights(22, 1:defsvm%ncv,i) = (/ kk4(3,1,i), kk4(3,3,i), kk4(3,2,i) , kk4(3,9,i), kk4(3,8,i), kk4(3,7,i),kk4(3,6,i),kk4(3,5,i),kk4(3,4,i),kk4(3,10,i)/)

  defsvm%grad_interp_weights(4, 1:defsvm%ncv,i)  = (/ kk4(4,1,i), kk4(4,2,i), kk4(4,3,i) , kk4(4,4,i), kk4(4,5,i), kk4(4,6,i),kk4(4,7,i),kk4(4,8,i),kk4(4,9,i),kk4(4,10,i)/)!
  defsvm%grad_interp_weights(5, 1:defsvm%ncv,i)  = (/ kk4(4,2,i), kk4(4,1,i), kk4(4,3,i) , kk4(4,5,i), kk4(4,4,i), kk4(4,9,i),kk4(4,8,i),kk4(4,7,i),kk4(4,6,i),kk4(4,10,i)/)
  defsvm%grad_interp_weights(12, 1:defsvm%ncv,i) = (/ kk4(4,3,i), kk4(4,1,i), kk4(4,2,i) , kk4(4,8,i), kk4(4,9,i), kk4(4,4,i),kk4(4,5,i),kk4(4,6,i),kk4(4,7,i),kk4(4,10,i)/)
  defsvm%grad_interp_weights(13, 1:defsvm%ncv,i) = (/ kk4(4,3,i), kk4(4,2,i), kk4(4,1,i) , kk4(4,7,i), kk4(4,6,i), kk4(4,5,i),kk4(4,4,i),kk4(4,9,i),kk4(4,8,i),kk4(4,10,i)/)
  defsvm%grad_interp_weights(20, 1:defsvm%ncv,i) = (/ kk4(4,2,i), kk4(4,3,i), kk4(4,1,i) , kk4(4,6,i), kk4(4,7,i), kk4(4,8,i),kk4(4,9,i),kk4(4,4,i),kk4(4,5,i),kk4(4,10,i)/)
  defsvm%grad_interp_weights(21, 1:defsvm%ncv,i) = (/ kk4(4,1,i), kk4(4,3,i), kk4(4,2,i) , kk4(4,9,i), kk4(4,8,i), kk4(4,7,i),kk4(4,6,i),kk4(4,5,i),kk4(4,4,i),kk4(4,10,i)/)

  defsvm%grad_interp_weights(25, 1:defsvm%ncv,i) = (/ kk4(5,1,i), kk4(5,2,i), kk4(5,3,i) , kk4(5,4,i), kk4(5,5,i), kk4(5,6,i),kk4(5,7,i),kk4(5,8,i),kk4(5,9,i),kk4(5,10,i)/)!
  defsvm%grad_interp_weights(29, 1:defsvm%ncv,i) = (/ kk4(5,2,i), kk4(5,1,i), kk4(5,3,i) , kk4(5,5,i), kk4(5,4,i), kk4(5,9,i),kk4(5,8,i),kk4(5,7,i),kk4(5,6,i),kk4(5,10,i)/)
  defsvm%grad_interp_weights(31, 1:defsvm%ncv,i) = (/ kk4(5,3,i), kk4(5,1,i), kk4(5,2,i) , kk4(5,8,i), kk4(5,9,i), kk4(5,4,i),kk4(5,5,i),kk4(5,6,i),kk4(5,7,i),kk4(5,10,i)/)
  defsvm%grad_interp_weights(35, 1:defsvm%ncv,i) = (/ kk4(5,3,i), kk4(5,2,i), kk4(5,1,i) , kk4(5,7,i), kk4(5,6,i), kk4(5,5,i),kk4(5,4,i),kk4(5,9,i),kk4(5,8,i),kk4(5,10,i)/)
  defsvm%grad_interp_weights(37, 1:defsvm%ncv,i) = (/ kk4(5,2,i), kk4(5,3,i), kk4(5,1,i) , kk4(5,6,i), kk4(5,7,i), kk4(5,8,i),kk4(5,9,i),kk4(5,4,i),kk4(5,5,i),kk4(5,10,i)/)
  defsvm%grad_interp_weights(41, 1:defsvm%ncv,i) = (/ kk4(5,1,i), kk4(5,3,i), kk4(5,2,i) , kk4(5,9,i), kk4(5,8,i), kk4(5,7,i),kk4(5,6,i),kk4(5,5,i),kk4(5,4,i),kk4(5,10,i)/)

  defsvm%grad_interp_weights(26, 1:defsvm%ncv,i) = (/ kk4(6,1,i), kk4(6,2,i), kk4(6,3,i) , kk4(6,4,i), kk4(6,5,i), kk4(6,6,i),kk4(6,7,i),kk4(6,8,i),kk4(6,9,i),kk4(6,10,i)/)!
  defsvm%grad_interp_weights(30, 1:defsvm%ncv,i) = (/ kk4(6,2,i), kk4(6,1,i), kk4(6,3,i) , kk4(6,5,i), kk4(6,4,i), kk4(6,9,i),kk4(6,8,i),kk4(6,7,i),kk4(6,6,i),kk4(6,10,i)/)
  defsvm%grad_interp_weights(32, 1:defsvm%ncv,i) = (/ kk4(6,3,i), kk4(6,1,i), kk4(6,2,i) , kk4(6,8,i), kk4(6,9,i), kk4(6,4,i),kk4(6,5,i),kk4(6,6,i),kk4(6,7,i),kk4(6,10,i)/)
  defsvm%grad_interp_weights(36, 1:defsvm%ncv,i) = (/ kk4(6,3,i), kk4(6,2,i), kk4(6,1,i) , kk4(6,7,i), kk4(6,6,i), kk4(6,5,i),kk4(6,4,i),kk4(6,9,i),kk4(6,8,i),kk4(6,10,i)/)
  defsvm%grad_interp_weights(38, 1:defsvm%ncv,i) = (/ kk4(6,2,i), kk4(6,3,i), kk4(6,1,i) , kk4(6,6,i), kk4(6,7,i), kk4(6,8,i),kk4(6,9,i),kk4(6,4,i),kk4(6,5,i),kk4(6,10,i)/)
  defsvm%grad_interp_weights(42, 1:defsvm%ncv,i) = (/ kk4(6,1,i), kk4(6,3,i), kk4(6,2,i) , kk4(6,9,i), kk4(6,8,i), kk4(6,7,i),kk4(6,6,i),kk4(6,5,i),kk4(6,4,i),kk4(6,10,i)/)

  defsvm%grad_interp_weights(27, 1:defsvm%ncv,i) = (/ kk4(7,1,i), kk4(7,2,i), kk4(7,3,i) , kk4(7,4,i), kk4(7,5,i), kk4(7,6,i),kk4(7,7,i),kk4(7,8,i),kk4(7,9,i),kk4(7,10,i)/)!
  defsvm%grad_interp_weights(33, 1:defsvm%ncv,i) = (/ kk4(7,3,i), kk4(7,1,i), kk4(7,2,i) , kk4(7,8,i), kk4(7,9,i), kk4(7,4,i),kk4(7,5,i),kk4(7,6,i),kk4(7,7,i),kk4(7,10,i)/)
  defsvm%grad_interp_weights(39, 1:defsvm%ncv,i) = (/ kk4(7,1,i), kk4(7,3,i), kk4(7,2,i) , kk4(7,9,i), kk4(7,8,i), kk4(7,7,i),kk4(7,6,i),kk4(7,5,i),kk4(7,4,i),kk4(7,10,i)/)
                                                        
  defsvm%grad_interp_weights(28, 1:defsvm%ncv,i) = (/ kk4(8,1,i), kk4(8,2,i), kk4(8,3,i) , kk4(8,4,i), kk4(8,5,i), kk4(8,6,i),kk4(8,7,i),kk4(8,8,i),kk4(8,9,i),kk4(8,10,i)/)!
  defsvm%grad_interp_weights(34, 1:defsvm%ncv,i) = (/ kk4(8,3,i), kk4(8,1,i), kk4(8,2,i) , kk4(8,8,i), kk4(8,9,i), kk4(8,4,i),kk4(8,5,i),kk4(8,6,i),kk4(8,7,i),kk4(8,10,i)/)
  defsvm%grad_interp_weights(40, 1:defsvm%ncv,i) = (/ kk4(8,1,i), kk4(8,3,i), kk4(8,2,i) , kk4(8,9,i), kk4(8,8,i), kk4(8,7,i),kk4(8,6,i),kk4(8,5,i),kk4(8,4,i),kk4(8,10,i)/)
                                                  
  defsvm%grad_interp_weights(43, 1:defsvm%ncv,i) = (/ kk4(9,1,i), kk4(9,2,i), kk4(9,3,i) , kk4(9,4,i), kk4(9,5,i), kk4(9,6,i),kk4(9,7,i),kk4(9,8,i),kk4(9,9,i),kk4(9,10,i)/)!
  defsvm%grad_interp_weights(45, 1:defsvm%ncv,i) = (/ kk4(9,3,i), kk4(9,1,i), kk4(9,2,i) , kk4(9,8,i), kk4(9,9,i), kk4(9,4,i),kk4(9,5,i),kk4(9,6,i),kk4(9,7,i),kk4(9,10,i)/)
  defsvm%grad_interp_weights(47, 1:defsvm%ncv,i) = (/ kk4(9,2,i), kk4(9,3,i), kk4(9,1,i) , kk4(9,6,i), kk4(9,7,i), kk4(9,8,i),kk4(9,9,i),kk4(9,4,i),kk4(9,5,i),kk4(9,10,i)/)

  defsvm%grad_interp_weights(44, 1:defsvm%ncv,i) = (/ kk4(10,1,i), kk4(10,2,i), kk4(10,3,i) , kk4(10,4,i), kk4(10,5,i), kk4(10,6,i),kk4(10,7,i),kk4(10,8,i),kk4(10,9,i),kk4(10,10,i)/)!
  defsvm%grad_interp_weights(46, 1:defsvm%ncv,i) = (/ kk4(10,3,i), kk4(10,1,i), kk4(10,2,i) , kk4(10,8,i), kk4(10,9,i), kk4(10,4,i),kk4(10,5,i),kk4(10,6,i),kk4(10,7,i),kk4(10,10,i)/)
  defsvm%grad_interp_weights(48, 1:defsvm%ncv,i) = (/ kk4(10,2,i), kk4(10,3,i), kk4(10,1,i) , kk4(10,6,i), kk4(10,7,i), kk4(10,8,i),kk4(10,9,i),kk4(10,4,i),kk4(10,5,i),kk4(10,10,i)/)

  defsvm%grad_interp_weights(49, 1:defsvm%ncv,i) = (/ kk4(11,1,i), kk4(11,2,i), kk4(11,3,i) , kk4(11,4,i), kk4(11,5,i), kk4(11,6,i),kk4(11,7,i),kk4(11,8,i),kk4(11,9,i),kk4(11,10,i)/)!
  defsvm%grad_interp_weights(52, 1:defsvm%ncv,i) = (/ kk4(11,2,i), kk4(11,1,i), kk4(11,3,i) , kk4(11,5,i), kk4(11,4,i), kk4(11,9,i),kk4(11,8,i),kk4(11,7,i),kk4(11,6,i),kk4(11,10,i)/)
  defsvm%grad_interp_weights(53, 1:defsvm%ncv,i) = (/ kk4(11,3,i), kk4(11,1,i), kk4(11,2,i) , kk4(11,8,i), kk4(11,9,i), kk4(11,4,i),kk4(11,5,i),kk4(11,6,i),kk4(11,7,i),kk4(11,10,i)/)
  defsvm%grad_interp_weights(56, 1:defsvm%ncv,i) = (/ kk4(11,3,i), kk4(11,2,i), kk4(11,1,i) , kk4(11,7,i), kk4(11,6,i), kk4(11,5,i),kk4(11,4,i),kk4(11,9,i),kk4(11,8,i),kk4(11,10,i)/)
  defsvm%grad_interp_weights(57, 1:defsvm%ncv,i) = (/ kk4(11,2,i), kk4(11,3,i), kk4(11,1,i) , kk4(11,6,i), kk4(11,7,i), kk4(11,8,i),kk4(11,9,i),kk4(11,4,i),kk4(11,5,i),kk4(11,10,i)/)
  defsvm%grad_interp_weights(60, 1:defsvm%ncv,i) = (/ kk4(11,1,i), kk4(11,3,i), kk4(11,2,i) , kk4(11,9,i), kk4(11,8,i), kk4(11,7,i),kk4(11,6,i),kk4(11,5,i),kk4(11,4,i),kk4(11,10,i)/)


  defsvm%grad_interp_weights(50, 1:defsvm%ncv,i) = (/ kk4(12,1,i), kk4(12,2,i), kk4(12,3,i) , kk4(12,4,i), kk4(12,5,i), kk4(12,6,i),kk4(12,7,i),kk4(12,8,i),kk4(12,9,i),kk4(12,10,i)/)!
  defsvm%grad_interp_weights(51, 1:defsvm%ncv,i) = (/ kk4(12,2,i), kk4(12,1,i), kk4(12,3,i) , kk4(12,5,i), kk4(12,4,i), kk4(12,9,i),kk4(12,8,i),kk4(12,7,i),kk4(12,6,i),kk4(12,10,i)/)
  defsvm%grad_interp_weights(54, 1:defsvm%ncv,i) = (/ kk4(12,3,i), kk4(12,1,i), kk4(12,2,i) , kk4(12,8,i), kk4(12,9,i), kk4(12,4,i),kk4(12,5,i),kk4(12,6,i),kk4(12,7,i),kk4(12,10,i)/)
  defsvm%grad_interp_weights(55, 1:defsvm%ncv,i) = (/ kk4(12,3,i), kk4(12,2,i), kk4(12,1,i) , kk4(12,7,i), kk4(12,6,i), kk4(12,5,i),kk4(12,4,i),kk4(12,9,i),kk4(12,8,i),kk4(12,10,i)/)
  defsvm%grad_interp_weights(58, 1:defsvm%ncv,i) = (/ kk4(12,2,i), kk4(12,3,i), kk4(12,1,i) , kk4(12,6,i), kk4(12,7,i), kk4(12,8,i),kk4(12,9,i),kk4(12,4,i),kk4(12,5,i),kk4(12,10,i)/)
  defsvm%grad_interp_weights(59, 1:defsvm%ncv,i) = (/ kk4(12,1,i), kk4(12,3,i), kk4(12,2,i) , kk4(12,9,i), kk4(12,8,i), kk4(12,7,i),kk4(12,6,i),kk4(12,5,i),kk4(12,4,i),kk4(12,10,i)/)

enddo
if (size(defsvm%grad_interp_weights, 1) /= 60) call error_stop("SVM initialization: bad array size (tri4)")

endsubroutine  



endmodule MENU_NUM
!------------------------------------------------------------------------------!
! Changes history
!
! May  2002 : creation du module
! Aug  2003 : parametres pour l'integration temporelle (Fourier, residu)
! Sep  2003 : parametres pour l'integration spatiale (calcul de gradients)
! Jan  2006 : gradient computation method (local routine to get parameters)
! Nov  2007 : Runge-Kutta parameters
! Jul  2010 : number of Krylov vectors
! Feb  2013 : kinetic/beta evaluations for hllc and hlle
!------------------------------------------------------------------------------!
