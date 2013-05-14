!------------------------------------------------------------------------------!
! MODULE : EQNS                           Auteur : J. Gressier
!                                         Date   : Mai 2002
! Fonction                                Modif  : cf historique
!   Bibliotheque de procedures et fonctions pour la definition des etats
!   dans les equations de Navier-Stokes
!
!------------------------------------------------------------------------------!

module EQNS

use TYPHMAKE   ! Definition de la precision
use GEO3D      ! Compilation conditionnelle ? avec GEO3D_dp
use OUTPUT


! -- DECLARATIONS -----------------------------------------------------------

! NIST reference for molar gas constant : R = 8.3144621(75) J/mol/K
! ref: http://physics.nist.gov/cgi-bin/cuu/Value?r
!real(krp), parameter :: perfect_gas_cst = 8.3144621_krp
real(krp), parameter :: perfect_gas_cst = 8.314472_krp

! -- Variables globales du module -------------------------------------------

! -- Type de solveur (menu_ns%typ_fluid)--
integer, parameter :: eqEULER    = 10
integer, parameter :: eqEULERaxi = 11
integer, parameter :: eqNSLAM    = 15
integer, parameter :: eqNSLAMaxi = 16
integer, parameter :: eqRANS     = 20
integer, parameter :: eqNSLES    = 30

! -- Type de gaz (menu_ns%typ_gaz) --
integer, parameter :: gas_AIR     = 10
integer, parameter :: gas_PERFECT = 5

! -- Viscosity : computation
integer, parameter :: visc_no      = 10
integer, parameter :: visc_suth    = 11
integer, parameter :: visc_dyncst  = 13
integer, parameter :: visc_kincst  = 14
integer, parameter :: visc_lin     = 15



!------------------------------------------------------------------------------!
! Definition de la structure ST_NSETAT : etat physique
!------------------------------------------------------------------------------!
type st_nsetat
  integer                          :: dim
  real(krp), dimension(:), pointer :: density    ! masse volumique
  real(krp), dimension(:), pointer :: pressure   ! pression
  type(v3d), dimension(:), pointer :: velocity   ! vitesse
endtype st_nsetat

!------------------------------------------------------------------------------!
! Definition de la structure ST_ESPECE : Definition d'une espece de gaz
!------------------------------------------------------------------------------!
type st_espece
  integer(kpp) :: fluid_type    ! type of fluid
  real(krp)    :: gamma         ! rapport de chaleurs specifiques
  real(krp)    :: r_const       ! constante du gaz
  integer      :: typ_visc      ! kind of computation of viscosity
  real(krp)    :: prandtl       ! nombre de Prandtl
  real(krp)    :: visc_kin      ! kinematic viscosity (constant)
  real(krp)    :: visc_dyn      ! dynamic   viscosity (constant or sutherland)
  real(krp)    :: tref          ! Sutherland's formula : reference temperature
  real(krp)    :: tsuth         ! Sutherland's formula : Sutherland's constant
endtype st_espece

! -- INTERFACES -------------------------------------------------------------

interface new
  module procedure new_nsetat
endinterface

interface delete
  module procedure delete_nsetat
endinterface


! -- Fonctions et Operateurs ------------------------------------------------


! -- IMPLEMENTATION ---------------------------------------------------------
contains

!------------------------------------------------------------------------------!
! Fonction : nsetat constructor
!------------------------------------------------------------------------------!
subroutine new_nsetat(nsetat, dim)
implicit none
type(st_nsetat) :: nsetat
integer         :: dim

  nsetat%dim = dim
  allocate(nsetat%density(dim))
  allocate(nsetat%pressure(dim))
  allocate(nsetat%velocity(dim))

endsubroutine new_nsetat

!------------------------------------------------------------------------------!
! Fonction : nsetat destructor
!------------------------------------------------------------------------------!
subroutine delete_nsetat(nsetat)
implicit none
type(st_nsetat) :: nsetat

  deallocate(nsetat%density)
  deallocate(nsetat%pressure)
  deallocate(nsetat%velocity)

endsubroutine delete_nsetat


!------------------------------------------------------------------------------!
! Fonction : conversion de parametres en variables primitives
!------------------------------------------------------------------------------!
!type(st_nsetat) function rho_ps_vel2nspri(n, rho, ps, vel) result(nspri)
!implicit none

! -- INPUTS --
!integer         :: n
!type(v3d)       :: vel(n)
!real(krp)       :: rho(n), ps(n)

!  nspri%pressure(1:n) = ps(1:n)
!  nspri%density(1:n)  = rho(1:n)
!  nspri%velocity(1:n) = vel(1:n)

!endfunction rho_ps_vel2nspri


!------------------------------------------------------------------------------!
! Fonction : conversion de parametres en variables primitives
!------------------------------------------------------------------------------!
subroutine pi_ti_mach_dir2nspri(fluid, n, pi, ti, mach, dir, nspri)
implicit none

! -- INPUTS --
type(st_espece) :: fluid
integer         :: n
type(v3d)       :: dir(n)
real(krp)       :: pi(n), ti(n), mach(n)
! -- OUTPUTS --
type(st_nsetat) :: nspri
! -- internal variables --
real(krp)       :: g1, fm(n), ts(n), a(n)

  g1 = fluid%gamma -1._krp
  fm(1:n) = 1._krp / (1._krp + .5_krp*g1*mach(1:n)**2)
  ts(1:n) = ti(1:n) *fm(1:n)
  a(1:n)  = sqrt(fluid%gamma*fluid%r_const*ts(1:n))
  nspri%pressure(1:n) = pi *(fm**(fluid%gamma/g1))
  nspri%density(1:n)  = nspri%pressure(1:n) / (fluid%r_const * ts(1:n))
  nspri%velocity(1:n) = (mach(1:n)*a(1:n))*dir(1:n)       ! product of scalars before

end subroutine pi_ti_mach_dir2nspri


!------------------------------------------------------------------------------!
! Fonction : conversion de parametres en variables primitives
!------------------------------------------------------------------------------!
subroutine pi_ti_ps_dir2nspri(fluid, n, pi, ti, ps, dir, nspri)
implicit none
! -- INPUTS --
integer         :: n
type(st_espece) :: fluid
type(v3d)       :: dir(n)
real(krp)       :: pi(n), ti(n), ps(n)
! -- OUTPUTS --
type(st_nsetat) :: nspri
! -- internal variables
real(krp)       :: g1, fm(n)

  g1   = fluid%gamma -1._krp
  fm   = (pi/ps)**(g1/fluid%gamma)
  if (count(fm(1:n) < 1._krp) > 0) then
    call print_warning("Bad ratio Pi/P : truncated to 1.")
    where (fm(1:n) < 1._krp) fm = 1._krp
  endif

  nspri%pressure(1:n) = ps(1:n)
  nspri%density(1:n)  = ps(1:n)  / (fluid%r_const * ti(1:n)) * fm(1:n)
  nspri%velocity(1:n) = ( sqrt((fm-1._krp)*2._krp/g1)                    &    ! mach number
                         *sqrt(fluid%gamma*ps(1:n)/nspri%density(1:n))   &    ! speed of sound
                         ) * dir(1:n)

end subroutine pi_ti_ps_dir2nspri


!------------------------------------------------------------------------------!
! Fonction : conversion de parametres en variables primitives
!------------------------------------------------------------------------------!
subroutine nspri2pi_ti_mach_dir(fluid, n, nspri, pi, ti, mach, dir)
implicit none
! -- INPUTS --
integer         :: n
type(st_espece) :: fluid
type(st_nsetat) :: nspri
! -- OUTPUTS --
real(krp)       :: pi(n), ti(n), mach(n)
type(v3d)       :: dir(n)
! -- internal variables
real(krp)               :: g1
real(krp), dimension(n) :: fmv    ! automatic array

  g1   = fluid%gamma -1._krp
  fmv(1:n)  = sqrabs(nspri%velocity)                                            ! V^2
  dir(1:n)  = nspri%velocity(1:n) / sqrt(fmv(1:n))                              ! direction
  fmv(1:n)  = fmv(1:n) / (fluid%gamma*nspri%pressure(1:n)) * nspri%density(1:n) ! M^2
  mach(1:n) = sqrt(fmv(1:n))
  fmv(1:n)  = 1._krp + 0.5_krp*g1*fmv(1:n)
  pi        = nspri%pressure(1:n)*fmv(1:n)**(fluid%gamma/g1)
  ti        = nspri%pressure(1:n)/fluid%r_const/nspri%density(1:n)*fmv(1:n)

endsubroutine nspri2pi_ti_mach_dir


!------------------------------------------------------------------------------!
! Fonction : conversion de parametres en variables primitives
!------------------------------------------------------------------------------!
subroutine calc_viscosity(fluid, density, temperature, dynvisc)
implicit none
! -- INPUTS --
type(st_espece)       :: fluid
real(krp), intent(in) :: density(:), temperature(:)
! -- OUTPUTS --
real(krp), intent(out) :: dynvisc(size(temperature))

! -- internal variables
integer               :: i, n
real(krp)             :: mu0, T0, S

! -- BODY --

n = size(temperature)

select case(fluid%typ_visc)

case(visc_suth)
  mu0 = fluid%visc_dyn   ! dynamic viscosity at reference temperature
  T0  = fluid%tref       ! reference temperature
  S   = fluid%tsuth      ! Sutherland's constant
  do i = 1, n
    dynvisc(i) = mu0 * (temperature(i)/T0)**(1.5_krp)*(T0+S)/(temperature(i)+S)
  enddo

case(visc_dyncst)
  dynvisc(1:n) = fluid%visc_dyn

case(visc_kincst)
  dynvisc(1:n) = fluid%visc_kin * density(1:n)
  !print*,'visc:',sum(dynvisc(1:n))/n

case(visc_lin)
  dynvisc(1:n) = fluid%visc_dyn*temperature(1:n)

case default
  call erreur("viscosity computation","unknown kind of computation")
endselect

endsubroutine calc_viscosity



endmodule EQNS

!------------------------------------------------------------------------------!
! Changes history
!
! mai  2002 : creation du module
! sept 2003 : adaptation du module pour premiers developpements
! july 2004 : primitive variables calculation
! mar  2006 : nsetat transformed to array
!------------------------------------------------------------------------------!
