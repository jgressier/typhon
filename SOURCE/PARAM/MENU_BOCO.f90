!------------------------------------------------------------------------------!
! MODULE : MENU_BOCO                      Auteur : J. Gressier
!                                         Date   : Novembre 2002
! Fonction                                Modif  : (cf Historique)
!   Definition des structures pour les entrees du programme TYPHON
!   Structures pour la definition des conditions aux limites
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
module MENU_BOCO

use STRING
use TYPHMAKE    ! Definition de la precision
use VARCOM      ! Definition des constantes
use MENU_NS     ! Definition des solveurs type NS
use MENU_KDIF   ! Definition des solveurs type Equation de diffusion
use MENU_VORTEX ! Definition des solveurs type VORTEX/singularites

implicit none


! -- Variables globales du module -------------------------------------------

! -- Definition des entiers caracteristiques pour l'uniformite de la CL --

integer, parameter :: uniform    = 10   
integer, parameter :: nonuniform = 20 

! -- Definition des entiers caracteristiques pour le type de solveur --


! -- DECLARATIONS -----------------------------------------------------------

!------------------------------------------------------------------------------!
! structure MNU_BOCO : options numeriques les solveurs 
!------------------------------------------------------------------------------!
type mnu_boco
  character(len=strlen) :: family        ! nom de famille de la condition aux limites
  integer               :: typ_boco      ! type physique de condition aux limites 
                                         !  (cf definitions VARCOM : bc_*) 
  integer               :: typ_calc      ! type de calcul de conditions aux limites
                                         !  (cf definitions VARCOM) 
                                         !   CONNECTION    : type de connection (GHOSTFACE,...)?!  
                                         !   COUPLING      : type de connection (GHOSTFACE,...)?!  
                                         !   EXTRAPOLATION : ordre d'extrapolation?!
  integer               :: boco_unif     ! condition aux limites uniforme ou non  
  integer               :: order_extrap  ! ordre d'extrapolation : A INCLURE dans typ_calc

  type(st_boco_kdif)    :: boco_kdif     ! condition aux limites propre au solveur KDIF
  type(st_boco_vort)    :: boco_vortex   ! condition aux limites propre au solveur VORTEX
  type(st_boco_ns)      :: boco_ns       ! condition aux limites propre au solveur NS

  !integer               :: np_int    ! nombre de parametres entiers
  !integer               :: np_real   ! nombre de parametres reels
  !integer, dimension(:), pointer &
  !                      :: iparam    ! parametres entiers
  !real(krp), dimension(:), pointer &
  !                      :: rparam    ! parametres reels
  !type(st_boco_ns)    :: boco_ns     ! condition aux limites propre au solveur NS
endtype mnu_boco


! -- INTERFACES -------------------------------------------------------------


! -- Fonctions et Operateurs ------------------------------------------------

!integer bocotype, bctype_of_boco

! -- IMPLEMENTATION ---------------------------------------------------------
contains

!------------------------------------------------------------------------------!
! Fonction : retourne le type entier de conditions aux limites
!------------------------------------------------------------------------------!
integer function bocotype(str)
implicit none
character(len=*) str

  bocotype = inull

  if (samestring(str, "CONNECTION" ))          bocotype = bc_connect_match
  if (samestring(str, "COUPLING" ))            bocotype = bc_wall_isoth

  if (samestring(str, "SYMMETRY" ))            bocotype = bc_geo_sym   
  if (samestring(str, "PERIODIC" ))            bocotype = bc_geo_period
  if (samestring(str, "EXTRAPOLATE" ))         bocotype = bc_geo_extrapol

  if (samestring(str, "ADIABATIC_WALL" ))      bocotype = bc_wall_adiab 
  if (samestring(str, "ISOTHERMAL_WALL" ))     bocotype = bc_wall_isoth 
  if (samestring(str, "FLUXSET_WALL" ))        bocotype = bc_wall_flux  
  if (samestring(str, "CONVECTION_WALL" ))     bocotype = bc_wall_hconv 
  if (samestring(str, "GEN_CONV_WALL" ))       bocotype = bc_wall_hgen

  if (samestring(str, "SUBSONIC_INLET" ))      bocotype = bc_inlet_sub
  if (samestring(str, "SUPERSONIC_INLET" ))    bocotype = bc_inlet_sup
  if (samestring(str, "SUBSONIC_OUTLET" ))     bocotype = bc_outlet_sub
  if (samestring(str, "SUPERSONIC_OUTLET" ))   bocotype = bc_outlet_sup

  if (samestring(str, "WALL" ))                bocotype = bc_wall
  if (samestring(str, "KUTTA" ))               bocotype = bc_kutta
  if (samestring(str, "FAR-FIELD" ))           bocotype = bc_farfield

endfunction bocotype


!------------------------------------------------------------------------------!
! Fonction : bctype_of_boco(isolver, itype)
!------------------------------------------------------------------------------!
integer function bctype_of_boco(isolver, itype)
implicit none
integer isolver, itype

  select case(itype)
  case(bc_geo_sym)
    bctype_of_boco = bc_calc_ghostcell
  case(bc_geo_period)
    call erreur("Developpement","'bc_geo_period' : Cas non implemente")
  case(bc_geo_extrapol)
    bctype_of_boco = bc_calc_ghostface
  case default    
    select case(isolver)
    case(solNS)
      bctype_of_boco = bctype_of_nsboco(itype)
    case(solKDIF)
      bctype_of_boco = bctype_of_kdifboco(itype)
    case(solVORTEX)
      bctype_of_boco = bctype_of_vortboco(itype)
    case default
      call erreur("incoherence interne (MENU_BOCO)","solveur inconnu")
    endselect
  endselect

endfunction bctype_of_boco


endmodule MENU_BOCO


!------------------------------------------------------------------------------!
! Historique des modifications
!
! nov  2002 : creation
! mars 2003 : definition of the different kinds of boundary conditions
! juin 2003 : "connection" types put together, "coupling" type added
! nov  2003 : (non-)uniformity of boundary conditions
! fev  2004 : boundary conditions specific of the VORTEX solver added
! nov  2006 : generalized convection condition added
!------------------------------------------------------------------------------!


