!------------------------------------------------------------------------------!
! MODULE : EQNS                           Auteur : J. Gressier
!                                         Date   : Mai 2002
! Fonction                                Modif  : cf historique
!   Bibliotheque de procedures et fonctions pour la definition des etats
!   dans les equations de Navier-Stokes
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

module EQNS

use TYPHMAKE   ! Definition de la precision
use GEO3D      ! Compilation conditionnelle ? avec GEO3D_dp

! -- DECLARATIONS -----------------------------------------------------------

!------------------------------------------------------------------------------!
! Definition de la structure ST_NSETAT : etat physique
!------------------------------------------------------------------------------!
type st_nsetat
!  real(krp), dimension(:), pointer &
!                  :: density    ! masses volumiques partielles (nesp)
  real(krp)       :: density    ! masse volumique
  real(krp)       :: pressure   ! pression
  type(v3d)       :: velocity   ! vitesse
endtype st_nsetat

!------------------------------------------------------------------------------!
! Definition de la structure ST_ESPECE : Definition d'une espece de gaz
!------------------------------------------------------------------------------!
type st_espece
  real(krp)    :: gamma         ! rapport de chaleurs specifiques
  real(krp)    :: r_const       ! constante du gaz
  real(krp)    :: prandtl       ! nombre de Prandtl
  real(krp)    :: visc_dyn      ! viscosite dynamique (faire evoluer en loi)
  real(krp)    :: tref          ! Sutherland's formula : reference temperature
  real(krp)    :: tsuth         ! Sutherland's formula : Sutherland's constant
endtype st_espece

! -- INTERFACES -------------------------------------------------------------

!interface new
!  module procedure new_mesh, new_field, new_block, new_zone
!endinterface

!interface delete
!  module procedure delete_mesh, delete_field, delete_block, delete_zone
!endinterface


! -- Fonctions et Operateurs ------------------------------------------------


! -- IMPLEMENTATION ---------------------------------------------------------
contains

!------------------------------------------------------------------------------!
! Fonction : conversion de variables conservatives en variables primitives
!------------------------------------------------------------------------------!
!type(st_nsetat) function cons2nspri(fluid, etat)
!implicit none

! -- declaration des entrees
!type(st_espece)          :: fluid
!type(st_ :: etat

!  cons2kdif%density  = etat(1)
!  cons2kdif%pressure = etat(1)/defkdif%materiau%Cp
!  cons2kdif%density = etat(1)/defkdif%materiau%Cp
!  cons2kdif%density = etat(1)/defkdif%materiau%Cp

!endfunction cons2kdif

!------------------------------------------------------------------------------!
! Fonction : conversion de parametres en variables primitives
!------------------------------------------------------------------------------!
type(st_nsetat) function rho_ps_vel2nspri(rho, ps, vel) result(nspri)
implicit none

! -- declaration des entrees
type(v3d)       :: vel
real(krp)       :: rho, ps

  nspri%pressure = ps
  nspri%density  = rho
  nspri%velocity = vel

endfunction rho_ps_vel2nspri


!------------------------------------------------------------------------------!
! Fonction : conversion de parametres en variables primitives
!------------------------------------------------------------------------------!
type(st_nsetat) function pi_ti_mach_dir2nspri(fluid, pi, ti, mach, dir) result(nspri)
implicit none

! -- declaration des entrees
type(st_espece) :: fluid
type(v3d)       :: dir
real(krp)       :: pi, ti, mach
! -- internal variables 
real(krp)       :: g1, fm, ts, a

  g1 = fluid%gamma -1._krp
  fm = 1._krp / (1._krp + .5_krp*g1*mach**2)
  ts = ti *fm
  a  = sqrt(fluid%gamma*fluid%r_const*ts)
  nspri%pressure = pi *(fm**(fluid%gamma/g1))
  nspri%density  = nspri%pressure / (fluid%r_const * ts)
  nspri%velocity = (mach*a)*dir       ! product of scalars before

endfunction pi_ti_mach_dir2nspri


!------------------------------------------------------------------------------!
! Fonction : conversion de parametres en variables primitives
!------------------------------------------------------------------------------!
type(st_nsetat) function pi_ti_ps_dir2nspri(fluid, pi, ti, ps, dir) result(nspri)
implicit none

! -- declaration des entrees
type(st_espece) :: fluid
type(v3d)       :: dir
real(krp)       :: pi, ti, ps
! -- internal variables 
real(krp)       :: g1, fm, ts, a, mach, m2

  g1   = fluid%gamma -1._krp
  fm   = (pi/ps)**(g1/fluid%gamma)
  mach = sqrt((fm-1._krp)*2._krp/g1)
  ts   = ti/fm
  a    = sqrt(fluid%gamma*fluid%r_const*ts)
  nspri%pressure = ps
  nspri%density  = ps / (fluid%r_const * ts)
  nspri%velocity = (mach*a)*dir       ! product of scalars before

endfunction pi_ti_ps_dir2nspri


!------------------------------------------------------------------------------!
! Fonction : conversion de parametres en variables primitives
!------------------------------------------------------------------------------!
subroutine nspri2pi_ti_mach_dir(fluid, nspri, pi, ti, mach, dir)
implicit none

! -- declaration des entrees
type(st_espece) :: fluid
type(st_nsetat) :: nspri
! -- declaration des sorties
real(krp)       :: pi, ti, mach
type(v3d)       :: dir
! -- internal variables 
real(krp)       :: g1, fm, ts, a2, v2, m2

  g1   = fluid%gamma -1._krp
  v2   = sqrabs(nspri%velocity)
  a2   = fluid%gamma*nspri%pressure/nspri%density
  m2   = v2/a2
  fm   = 1._krp + 0.5_krp*g1*m2
  pi   = nspri%pressure*fm**(fluid%gamma/g1)
  ti   = nspri%pressure/fluid%r_const/nspri%density*fm
  mach = sqrt(m2)
  dir  = nspri%velocity / sqrt(v2)

endsubroutine nspri2pi_ti_mach_dir


endmodule EQNS

!------------------------------------------------------------------------------!
! Modification history
!
! mai  2002 : creation du module
! sept 2003 : adaptation du module pour premiers developpements
! july 2004 : primitive variables calculation
!------------------------------------------------------------------------------!
