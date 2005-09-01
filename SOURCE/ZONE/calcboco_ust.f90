!------------------------------------------------------------------------------!
! Procedure : calcboco_ust                Auteur : J. Gressier
!                                         Date   : Avril 2003
! Fonction                                Modif  : (cf Historique)
!   Calcul des conditions aux limites pour maillage non structure
!   Aiguillage des appels selon le type (general ou par solveur)
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine calcboco_ust(defsolver, grid, defspat)

use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_SOLVER
use USTMESH
use DEFFIELD
!use MENU_ZONECOUPLING
use DEFZONE

implicit none

! -- Declaration des entrees --
type(mnu_solver)       :: defsolver        ! type d'equation a resoudre
type(mnu_spat)         :: defspat

! -- Declaration des entree/sorties --
type(st_grid)          :: grid             ! maillage en entree, champ en sortie

! -- Declaration des variables internes --
integer :: ib, ir                    ! index de conditions aux limites et de couplage
integer :: idef                      ! index de definitions des conditions aux limites
integer :: nrac                      ! numero de raccord

! -- Debut de la procedure --

do ib = 1, grid%umesh%nboco

  idef = grid%umesh%boco(ib)%idefboco

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
  case(bc_connection)
    call erreur("Developpement","'bc_connection' : Cas non implemente")

  case default 

    select case(defsolver%typ_solver)
      case(solNS)
        call calcboco_ns(defsolver, defsolver%boco(idef), grid%umesh%boco(ib), grid)
      case(solKDIF)
        call calcboco_kdif(defsolver, defsolver%boco(idef), grid%umesh%boco(ib), grid, defspat)
      case(solVORTEX)
        ! rien a faire
      case default
         call erreur("incoherence interne (def_boco)","solveur inconnu")
    endselect

  endselect

enddo


endsubroutine calcboco_ust

!------------------------------------------------------------------------------!
! Historique des modifications
!
! avril 2003 : creation de la procedure
! july  2004 : simplification of call tree (uniform or not boundary conditions)
! oct   2004 : field chained list
!------------------------------------------------------------------------------!
