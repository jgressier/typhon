!------------------------------------------------------------------------------!
! MODULE : MENU_SOLVER                    Auteur : J. Gressier
!                                         Date   : Aout 2002
! Fonction                                Modif  : (cf Historique)
!   Définition des structures pour les entrées du programme TYPHON
!   Structures pour les options des solveurs 
!   - Définition du problème (solveur, conditions limites, initiales)
!   - Paramètres de calcul   (intégration temporelle, spatiale, AMR...)
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
module MENU_SOLVER

use TYPHMAKE      ! Definition de la precision
use MENU_NS       ! Définition des solveurs type NS
use MENU_KDIF     ! Définition des solveurs type Equation de diffusion
use MENU_BOCO     ! Définition des conditions limites
use MENU_INIT     ! Définition de l'initialisation
use MENU_CAPTEURS ! Définition des capteurs

implicit none

! -- Variables globales du module -------------------------------------------

! -- Définition des entiers caractéristiques pour le type de solveur -- CF VARCOM
!integer, parameter :: solNS   = 10    ! Equations de Navier-Stokes (EQNS)
!integer, parameter :: solKDIF = 20    ! Equation  de la chaleur    (EQKDIF)

! -- Définition du type de quantite 
integer, parameter :: qs_temperature = 010 
integer, parameter :: qs_pressure    = 011
integer, parameter :: qs_density     = 012
integer, parameter :: qv_velocity    = 101
integer, parameter :: qv_stress      = 102 




! -- DECLARATIONS -----------------------------------------------------------

!------------------------------------------------------------------------------!
! structure MNU_SOLVER : options numériques les solveurs 
!------------------------------------------------------------------------------!
type mnu_solver
  integer         :: typ_solver      ! type de solveur (cf definitions VARCOM) 
  integer         :: nequat          ! nombre d'équations
  type(mnu_ns)    :: defns           ! options si solveur NS
  type(mnu_kdif)  :: defkdif         ! options si solveur KDIF
  integer         :: nboco           ! nombre de conditions aux limites
  type(mnu_boco), dimension(:), pointer &
                  :: boco            ! définitions des conditions aux limites
  integer         :: ninit           ! nombre de conditions aux limites
  type(mnu_init), dimension(:), pointer &
                  :: init            ! définitions des conditions aux limites
  integer         :: nprobe          ! nombre de capteurs
  type(mnu_capteur), dimension(:), pointer &
                  :: probe           ! définitions des capteurs
endtype mnu_solver


! -- INTERFACES -------------------------------------------------------------

interface delete
  module procedure delete_mnu_solver
endinterface

! -- Fonctions et Operateurs ------------------------------------------------


! -- IMPLEMENTATION ---------------------------------------------------------
contains

!------------------------------------------------------------------------------!
! Procédure : desallocation d'une structure MNU_SOLVER
!------------------------------------------------------------------------------!
subroutine delete_mnu_solver(defsolver)
implicit none
type(mnu_solver)  :: defsolver
integer           :: ib

  call delete(defsolver%defkdif%materiau%Kd)

  if (defsolver%ninit >= 1) then
    deallocate(defsolver%init)
  endif
  
  if (defsolver%nprobe >= 1) then
    deallocate(defsolver%probe)
  endif
  
  do ib = 1, defsolver%nboco
    select case(defsolver%boco(ib)%boco_unif)
    case(nonuniform)
      if (defsolver%boco(ib)%boco_kdif%alloctemp == .true.) then
        deallocate(defsolver%boco(ib)%boco_kdif%temp)
        defsolver%boco(ib)%boco_kdif%alloctemp = .false.
      endif
      if (defsolver%boco(ib)%boco_kdif%allocflux == .true.) then
        deallocate(defsolver%boco(ib)%boco_kdif%flux_nunif)
        defsolver%boco(ib)%boco_kdif%allocflux = .false.
      endif
    endselect
  enddo

  if (defsolver%nboco >= 1) then
    deallocate(defsolver%boco)
  endif

endsubroutine delete_mnu_solver


!------------------------------------------------------------------------------!
! Fonction : retourne le type entier de quantité physique
!------------------------------------------------------------------------------!
integer function quantity(str)
implicit none
character(len=*) str

  quantity = inull
  
  ! quantités scalaires
  if (samestring(str, "TEMPERATURE" ))  quantity = qs_temperature
  if (samestring(str, "PRESSURE" ))     quantity = qs_pressure
  if (samestring(str, "DENSITY" ))      quantity = qs_density

  ! quantités vectorielles
  if (samestring(str, "VELOCITY" ))     quantity = qv_velocity
  if (samestring(str, "STRESS" ))       quantity = qv_stress

endfunction quantity



endmodule MENU_SOLVER

!------------------------------------------------------------------------------!
! Historique des modifications
!
! aout 2002 : création du module
! mars 2003 : ajout des conditions aux limites
!             ajout des structures d'initialisation
! juil 2003 : procédure delete : Kd
! nov  2003 : tableau de paramètres pour les capteurs
!             définition des quantités
!------------------------------------------------------------------------------!




