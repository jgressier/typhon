!------------------------------------------------------------------------------!
! MODULE : MENU_SOLVER                               Authors : J. Gressier
!                                                    Created : Aout 2002
! Fonction                                           
!   Definition des structures pour les entrees du programme TYPHON
!   Structures pour les options des solveurs 
!   - Definition du probleme (solveur, conditions limites, initiales)
!   - Parametres de calcul   (integration temporelle, spatiale, AMR...)
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
module MENU_SOLVER

use TYPHMAKE      ! Definition de la precision
use MENU_MESH
use MENU_NUM      ! Definition des parametres numeriques d'integration
use MENU_NS       ! Definition des solveurs type NS
use MENU_KDIF     ! Definition des solveurs type Equation de diffusion
use MENU_VORTEX   ! Definition des solveurs type VORTEX (lagrangien)
use MENU_BOCO     ! Definition des conditions limites
use MENU_INIT     ! Definition de l'initialisation
use MENU_PROBE    ! Definition des capteurs
use MENU_AMR      ! Definition des parametres de raffinement
use MENU_MPI      ! MPI parameters

implicit none

! -- Variables globales du module -------------------------------------------

! -- Definition des entiers caracteristiques pour le type de solveur -- CF VARCOM
!integer, parameter :: solNS     = 10    ! Equations de Navier-Stokes (EQNS)
!integer, parameter :: solKDIF   = 20    ! Equation  de la chaleur    (EQKDIF)
!integer, parameter :: solVORTEX = 30   ! Methode integrale et lagrangienne VORTEX

! -- Definition du type de quantite 
integer, parameter :: qs_density         = 001
integer, parameter :: qs_pressure        = 002
integer, parameter :: qs_temperature     = 003 
integer, parameter :: qs_density_tot     = 011
integer, parameter :: qs_pressure_tot    = 012
integer, parameter :: qs_temperature_tot = 013 
integer, parameter :: qs_energy_int      = 015 
integer, parameter :: qs_energy_tot      = 016 
integer, parameter :: qs_enthalpy_int    = 017 
integer, parameter :: qs_enthalpy_tot    = 018 
integer, parameter :: qs_soundspeed      = 020 
integer, parameter :: qs_entropy         = 021 
integer, parameter :: qs_mach            = 030
integer, parameter :: qv_velocity        = 101
integer, parameter :: qv_momentum        = 102
integer, parameter :: qv_dynalpy         = 103
integer, parameter :: qv_stress          = 105 


! -- DECLARATIONS -----------------------------------------------------------

!------------------------------------------------------------------------------!
! structure MNU_SOLVER : solver parameters
!------------------------------------------------------------------------------!
type mnu_solver
  integer            :: typ_solver      ! type de solveur (cf definitions VARCOM) 
  integer            :: nequat          ! nombre d'equations
  integer            :: nsca, nvec      ! scalar and vector numbers
  real(krp), pointer :: refsca(:)       ! reference value for scalars
  real(krp), pointer :: refvec(:)       ! reference value for vectors
  integer, pointer   :: idsca(:)        ! names for primitive scalars 
  integer, pointer   :: idvec(:)        ! names for primitive vectors 
  type(mnu_mesh)  :: defmesh         ! mesh / geometry parameters
  type(mnu_time)  :: deftime         ! parametres d'integration temporelle
  type(mnu_spat)  :: defspat         ! parametres d'integration spatiale
  type(mnu_ns)    :: defns           ! options si solveur NS
  type(mnu_kdif)  :: defkdif         ! options si solveur KDIF
  type(mnu_vort)  :: defvort         ! options si solveur VORTEX
  type(mnu_amr)   :: defamr          ! options si AMR
  type(mnu_mpi)   :: defmpi          ! options si MPI
  integer(kip)    :: ninit           ! number of initialization
  integer(kip)    :: nboco           ! number of boundary conditions
  integer(kip)    :: nconnect        ! number of connections
  integer(kip)    :: nprobe          ! nombre de capteurs
  type(mnu_init),    pointer :: init(:)      ! Initial conditions
  type(mnu_boco),    pointer :: boco(:)      ! BOundary COnditions
  type(mnu_connect), pointer :: connect(:)   ! Internal connections
  type(mnu_probe),   pointer :: probe(:)     ! Probes
endtype mnu_solver


! -- INTERFACES -------------------------------------------------------------

interface delete
  module procedure delete_mnu_solver
endinterface

! -- Fonctions et Operateurs ------------------------------------------------


! -- IMPLEMENTATION ---------------------------------------------------------
contains

!------------------------------------------------------------------------------!
! Procedure : initialisation d'une structure MNU_SOLVER
!------------------------------------------------------------------------------!
subroutine init_mnu_solver(defsolver)
implicit none
type(mnu_solver)  :: defsolver

  defsolver%nboco  = 0
  defsolver%nprobe = 0
  defsolver%ninit  = 0
  defsolver%defamr%nbcriter = 0     ! call init(defsolver%defamr)

endsubroutine init_mnu_solver


!------------------------------------------------------------------------------!
! Procedure : initialisation d'une structure MNU_SOLVER
!------------------------------------------------------------------------------!
subroutine define_solver(defsolver, nsca, nvec)
implicit none
type(mnu_solver)  :: defsolver
integer           :: nsca, nvec

  defsolver%nsca   = nsca
  defsolver%nvec   = nvec
  defsolver%nequat = nsca + 3*nvec
  allocate(defsolver%refsca(nsca))
  allocate(defsolver%refvec(nvec))
  if (nsca /= 0) defsolver%refsca(1:nsca) = 0._krp
  if (nvec /= 0) defsolver%refvec(1:nvec) = 0._krp
  allocate(defsolver%idsca(nsca))
  allocate(defsolver%idvec(nvec))

endsubroutine define_solver


!------------------------------------------------------------------------------!
! Procedure : desallocation d'une structure MNU_SOLVER
!------------------------------------------------------------------------------!
subroutine delete_mnu_solver(defsolver)
implicit none
type(mnu_solver)  :: defsolver
integer           :: ib

  if (associated(defsolver%refsca)) deallocate(defsolver%refsca)
  if (associated(defsolver%refvec)) deallocate(defsolver%refvec)
  if (associated(defsolver%idsca)) deallocate(defsolver%idsca)
  if (associated(defsolver%idvec)) deallocate(defsolver%idvec)

  call delete(defsolver%defamr)

  select case(defsolver%typ_solver)
  case(solKDIF)
    call delete(defsolver%defkdif%materiau%Kd)
  case(solNS)
  case(solVORTEX)
  endselect

  ! -- destruction des parametres d'initialisation --
  select case(defsolver%typ_solver)
  case(solKDIF)
    do ib = 1, defsolver%ninit
      if (defsolver%init(ib)%unif==init_nonunif) then
        deallocate(defsolver%init(ib)%kdif%coef)
      endif
    enddo
  case(solNS)
  case(solVORTEX)
  endselect
  if (defsolver%ninit >= 1) then
    deallocate(defsolver%init)
  endif
  
  ! -- destruction des parametres des capteurs --

  if (defsolver%nprobe >= 1) then
    do ib = 1, defsolver%nprobe
      call delete(defsolver%probe(ib))
    enddo
    deallocate(defsolver%probe)
  endif
  
  ! -- destruction of boundary condition paramters --

  if (defsolver%nboco >= 1) then
    ! DEV : definition d'un delete_mnu_boco
    do ib = 1, defsolver%nboco
      select case(defsolver%boco(ib)%boco_unif)
      case(nonuniform)

        select case(defsolver%typ_solver)
        case(solKDIF)
          ! non uniform Dirichlet condition
          if (defsolver%boco(ib)%boco_kdif%alloctemp) then
            deallocate(defsolver%boco(ib)%boco_kdif%temp)
            defsolver%boco(ib)%boco_kdif%alloctemp = .false.
          endif
          !  non uniform Neuman condition
          if (defsolver%boco(ib)%boco_kdif%allocflux) then
            deallocate(defsolver%boco(ib)%boco_kdif%flux_nunif)
            defsolver%boco(ib)%boco_kdif%allocflux = .false.
          endif
          !  non uniform Fourier condition
          if (defsolver%boco(ib)%boco_kdif%allochconv) then
            deallocate(defsolver%boco(ib)%boco_kdif%h_nunif)
            deallocate(defsolver%boco(ib)%boco_kdif%tconv_nunif)
            defsolver%boco(ib)%boco_kdif%allochconv = .false.
          endif
        case(solNS)
          !  non uniform Dirichlet condition
          if (defsolver%boco(ib)%boco_ns%alloctemp) then
            deallocate(defsolver%boco(ib)%boco_ns%temp)
            defsolver%boco(ib)%boco_ns%alloctemp = .false.
          endif
          !  non uniform Neuman condition
          if (defsolver%boco(ib)%boco_ns%allocflux) then
            deallocate(defsolver%boco(ib)%boco_ns%flux_nunif)
            defsolver%boco(ib)%boco_ns%allocflux = .false.
          endif
          !  non uniform Fourier condition
          if (defsolver%boco(ib)%boco_ns%allochconv) then
            deallocate(defsolver%boco(ib)%boco_ns%h_nunif)
            deallocate(defsolver%boco(ib)%boco_ns%tconv_nunif)
            defsolver%boco(ib)%boco_ns%allochconv = .false.
          endif
        endselect
      endselect
    enddo

    deallocate(defsolver%boco)

  endif

endsubroutine delete_mnu_solver


!------------------------------------------------------------------------------!
! Function : get id (integer) from quantity string
!------------------------------------------------------------------------------!
integer function quantity_id(str)
implicit none
character(len=*) str

  quantity_id = inull

  ! quantites scalaires
  if (samestring(str, "RHO" ))          quantity_id = qs_density
  if (samestring(str, "DENSITY" ))      quantity_id = qs_density
  if (samestring(str, "PS" ))           quantity_id = qs_pressure
  if (samestring(str, "PRESSURE" ))     quantity_id = qs_pressure
  if (samestring(str, "TS" ))           quantity_id = qs_temperature
  if (samestring(str, "TEMPERATURE" ))  quantity_id = qs_temperature
  if (samestring(str, "RHOI" ))         quantity_id = qs_density_tot
  if (samestring(str, "PI" ))           quantity_id = qs_pressure_tot
  if (samestring(str, "TI" ))           quantity_id = qs_temperature_tot
  if (samestring(str, "E" ))            quantity_id = qs_energy_int
  if (samestring(str, "H" ))            quantity_id = qs_enthalpy_int
  if (samestring(str, "EI" ))           quantity_id = qs_energy_tot
  if (samestring(str, "HI" ))           quantity_id = qs_enthalpy_tot
  if (samestring(str, "ASOUND" ))       quantity_id = qs_soundspeed
  if (samestring(str, "A" ))            quantity_id = qs_soundspeed
  if (samestring(str, "C" ))            quantity_id = qs_soundspeed
  if (samestring(str, "S" ))            quantity_id = qs_entropy
  if (samestring(str, "M" ))            quantity_id = qs_entropy
  if (samestring(str, "MACH" ))         quantity_id = qs_mach

  ! quantites vectorielles
  if (samestring(str, "VELOCITY" ))     quantity_id = qv_velocity
  if (samestring(str, "V" ))            quantity_id = qv_velocity
  if (samestring(str, "MOMENTUM" ))     quantity_id = qv_momentum
  if (samestring(str, "DYNALPY" ))      quantity_id = qv_dynalpy
  if (samestring(str, "STRESS" ))       quantity_id = qv_stress

  if (quantity_id == inull) call error_stop("(MENU_SOLVER) unknown quantity "//trim(str))

endfunction quantity_id

!------------------------------------------------------------------------------!
! Fonction : get "official" name from quantity id
!------------------------------------------------------------------------------!
 function quantity_name(id) result(strout)
implicit none
integer           :: id
character(len=30) :: strout

select case(id)
case(qs_density)
  strout = "RHO"
case(qs_pressure)
  strout = "PS"
case(qs_temperature)
  strout = "TS"
case(qs_density_tot)
  strout = "RHOI"
case(qs_pressure_tot)
  strout = "PI"
case(qs_temperature_tot)
  strout = "TI"
case(qs_energy_int)
  strout = "E"
case(qs_enthalpy_int)
  strout = "H"
case(qs_energy_tot)
  strout = "EI"
case(qs_enthalpy_tot)
  strout = "HI"
case(qs_soundspeed)
  strout = "ASOUND"
case(qs_entropy)
  strout = "S"
case(qs_mach)
  strout = "M"
case(qv_velocity)
  strout = "V"
case(qv_momentum)
  strout = "RHOV"
case(qv_dynalpy)
  strout = "DYNALPY"
case(qv_stress)
  strout = "TAUW"
case default
  call error_stop("(MENU_SOLVER) unknown quantity id="//trim(strof(id)))
endselect

endfunction quantity_name

!------------------------------------------------------------------------------!
! Fonction : get "official" name from quantity id
!------------------------------------------------------------------------------!
 function quantity_cgnsname(id) result(strout)
implicit none
integer           :: id
character(len=30) :: strout

select case(id)
case(qs_density)
  strout = "Density"
case(qs_pressure)
  strout = "Pressure"
case(qs_temperature)
  strout = "Temperature"
case(qs_density_tot)
  strout = "RHOI"
case(qs_pressure_tot)
  strout = "PI"
case(qs_temperature_tot)
  strout = "TI"
case(qs_energy_int)
  strout = "E"
case(qs_enthalpy_int)
  strout = "H"
case(qs_energy_tot)
  strout = "EI"
case(qs_enthalpy_tot)
  strout = "HI"
case(qs_soundspeed)
  strout = "ASOUND"
case(qs_entropy)
  strout = "S"
case(qs_mach)
  strout = "M"
case(qv_velocity)
  strout = "Velocity"
case(qv_momentum)
  strout = "RHOV"
case(qv_dynalpy)
  strout = "DYNALPY"
case(qv_stress)
  strout = "TAUW"
case default
  call error_stop("(MENU_SOLVER) unknown quantity id="//trim(strof(id)))
endselect

endfunction quantity_cgnsname


!------------------------------------------------------------------------------!
! Fonction : retourne l'index de condition limite correspondant au nom "str"
!------------------------------------------------------------------------------!
integer function indexboco(defsolver, str)
implicit none
type(mnu_solver) :: defsolver
character(len=*) :: str
integer          :: i

  indexboco = inull
  do i = 1, defsolver%nboco
    if (samestring(str, defsolver%boco(i)%family)) indexboco = i
  enddo

endfunction indexboco


!------------------------------------------------------------------------------!
! Fonction : retourne l'index de capteur correspondant au nom "str"
!------------------------------------------------------------------------------!
integer function indexcapteur(defsolver, str)
implicit none
type(mnu_solver) :: defsolver
character(len=*) :: str
integer          :: i

  indexcapteur = inull
  do i = 1, defsolver%nprobe
    if (samestring(str, defsolver%probe(i)%name)) indexcapteur = i
  enddo

endfunction indexcapteur

!------------------------------------------------------------------------------!
! Routine : define_refcons
!------------------------------------------------------------------------------!
subroutine define_refcons(defsolver)
implicit none
type(mnu_solver) :: defsolver


select case(defsolver%typ_solver)
case(solKDIF)
  ! nothing to do
case(solNS)
  ! momentum reference value is (rho_ref * V_ref)
  ! V_ref is sqrt(Energy/rho_ref)
  defsolver%refvec(1) = sqrt(defsolver%refsca(1)*defsolver%refsca(2))  ! sqrt(rho * rhoV2)
case default
  call error_stop("Internal error (define_refcons): unknown solver")
endselect 

endsubroutine DEFINE_REFCONS



endmodule MENU_SOLVER

!------------------------------------------------------------------------------!
! Changes history
!
! aout 2002 : creation du module
! mars 2003 : ajout des conditions aux limites
!             ajout des structures d'initialisation
! juil 2003 : procedure delete : Kd
! nov  2003 : tableau de parametres pour les capteurs
!             definition des quantites
!             index de conditions limites ou de capteurs en fonction du nom
! fev  2004 : definition du solveur VORTEX
! juin 2004 : procedure delete : condition limite Fourier non uniforme
! july 2004 : add AMR parameters
! dec  2006 : add reference values
! June 2009 : add quantity names
!------------------------------------------------------------------------------!




