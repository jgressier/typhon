!------------------------------------------------------------------------------!
! Procedure : calcboco_ust                Auteur : J. Gressier
!                                         Date   : Avril 2003
! Fonction                                Modif  : (cf Historique)
!   Calcul des conditions aux limites pour maillage non structuré
!   Aiguillage des appels selon le type (général ou par solveur)
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine calcboco_ust(defsolver, grid)

use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_SOLVER
use USTMESH
use DEFFIELD
!use MENU_ZONECOUPLING
use DEFZONE

implicit none

! -- Declaration des entrées --
type(mnu_solver)       :: defsolver        ! type d'équation à résoudre

! -- Declaration des entrée/sorties --
type(st_grid)          :: grid             ! maillage en entrée, champ en sortie

! -- Declaration des variables internes --
integer :: ib, ir                    ! index de conditions aux limites et de couplage
integer :: idef                      ! index de definitions des conditions aux limites
integer :: nrac                      ! numéro de raccord

! -- Debut de la procedure --

do ib = 1, grid%umesh%nboco

  idef = grid%umesh%boco(ib)%idefboco

  ! Traitement des conditions aux limites communes aux solveurs

  select case(defsolver%boco(idef)%typ_boco)

  case(bc_geo_sym)
    call erreur("Développement","'bc_geo_sym' : Cas non implémenté")
    
  case(bc_geo_period)
    call erreur("Développement","'bc_geo_period' : Cas non implémenté")
    
  case(bc_geo_extrapol)
    call calcboco_ust_extrapol(defsolver%boco(idef), grid%umesh%boco(ib), grid%umesh, grid%field)

! PROVISOIRE : à retirer
  case(bc_connection)
    call erreur("Développement","'bc_connection' : Cas non implémenté")

  case default 

    select case(defsolver%typ_solver)
      case(solNS)
        call calcboco_ns(defsolver, defsolver%boco(idef), grid%umesh%boco(ib), grid)
      case(solKDIF)
        call calcboco_kdif(defsolver, defsolver%boco(idef), grid%umesh%boco(ib), grid)
      case(solVORTEX)
        ! rien à faire
      case default
         call erreur("incohérence interne (def_boco)","solveur inconnu")
    endselect

  endselect

enddo


endsubroutine calcboco_ust

!------------------------------------------------------------------------------!
! Historique des modifications
!
! avril 2003 : création de la procédure
! july  2004 : simplification of call tree (uniform or not boundary conditions)
!------------------------------------------------------------------------------!
