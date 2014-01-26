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
integer(kpp)           :: bccon_mode       ! data exchange mode for connection

! -- INPUTS/OUTPUTS --
type(st_grid)         :: grid             ! maillage en entree, champ en sortie
type(st_genericfield) :: fsend, frecv ! pointer of send or receive fields

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

  ! --- Common (geometrical) BOCO ---

  select case(defsolver%boco(idef)%typ_boco)

  case(bc_geo_sym)
    call calcboco_ust_sym(defsolver%boco(idef), defsolver%defale, defsolver%defmrf, &
           grid%umesh%boco(ib), grid%umesh, grid%info%field_loc, curtime)
    
  case(bc_geo_period)
    call error_stop("not expected to manage 'bc_geo_period' in this routine")
    
  case(bc_geo_extrapol)
    call calcboco_ust_extrapol(defsolver%boco(idef), grid%umesh%boco(ib), grid%umesh, grid%info%field_loc)

! PROVISOIRE : a retirer
  case(bc_connect_match)
    call error_stop("!!!DEV!!! 'bc_connection' : Cas non implemente")

  ! --- Solver dependent BOCO ---
  case default 

    select case(defsolver%typ_solver)
      case(solNS)
        call calcboco_ns(curtime, defsolver, defsolver%boco(idef), grid%umesh%boco(ib), grid)
      case(solKDIF)
        call calcboco_kdif(curtime, defsolver, defsolver%boco(idef), grid%umesh%boco(ib), grid, defspat)
      case(solVORTEX)
        ! rien a faire
      case default
        call error_stop("unknown solver (calcboco_ust)")
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
