!------------------------------------------------------------------------------!
! Procedure : calcboco_ust                             Authors : J. Gressier
!                                                      Created : April 2003
! Fonction 
!   Calcul des conditions aux limites pour maillage non structure
!   Aiguillage des appels selon le type (general ou par solveur)
!
!------------------------------------------------------------------------------!
subroutine calcboco_ust(curtime, defsolver, defspat, grid)

use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_SOLVER
use USTMESH
use DEFFIELD
!use MENU_ZONECOUPLING
use DEFZONE

implicit none

! -- INPUTS --
real(krp)              :: curtime
type(mnu_solver)       :: defsolver        ! type d'equation a resoudre
type(mnu_spat)         :: defspat

! -- INPUTS/OUTPUTS --
type(st_grid)          :: grid             ! maillage en entree, champ en sortie

! -- Internal variables --
integer :: ib, ir                    ! index de conditions aux limites et de couplage
integer :: idef                      ! index de definitions des conditions aux limites
integer :: nrac                      ! numero de raccord

! ----------------------------------- BODY -----------------------------------

!-----------------------------------------------------------
! only compute physical boundary conditions (idef >= 1)

do ib = 1, grid%umesh%nboco

  idef = grid%umesh%boco(ib)%idefboco
  if (idef >= 1) then

  ! Traitement des conditions aux limites communes aux solveurs

  select case(defsolver%boco(idef)%typ_boco)

  case(bc_geo_sym)
    call calcboco_ust_sym(defsolver%boco(idef), grid%umesh%boco(ib), grid%umesh, grid%info%field_loc)
    !call erreur("Developpement","'bc_geo_sym' : Cas non implemente")
    
  case(bc_geo_period)
    call erreur("Developpement","'bc_geo_period' : Cas non implemente")
    
  case(bc_geo_extrapol)
    call calcboco_ust_extrapol(defsolver%boco(idef), grid%umesh%boco(ib), grid%umesh, grid%info%field_loc)

! PROVISOIRE : a retirer
  case(bc_connect_match)
    call erreur("Developpement","'bc_connection' : Cas non implemente")

  case default 

    select case(defsolver%typ_solver)
      case(solNS)
        call calcboco_ns(curtime, defsolver, defsolver%boco(idef), grid%umesh%boco(ib), grid)
      case(solKDIF)
        call calcboco_kdif(curtime, defsolver, defsolver%boco(idef), grid%umesh%boco(ib), grid, defspat)
      case(solVORTEX)
        ! rien a faire
      case default
         call erreur("incoherence interne (def_boco)","solveur inconnu")
    endselect

  endselect

  endif

enddo


endsubroutine calcboco_ust

!------------------------------------------------------------------------------!
! Changes history
!
! avril 2003 : creation de la procedure
! july  2004 : simplification of call tree (uniform or not boundary conditions)
! oct   2004 : field chained list
! Oct   2005 : add connection computation (idef <= 0)
!------------------------------------------------------------------------------!
