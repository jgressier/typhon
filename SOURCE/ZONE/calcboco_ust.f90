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
subroutine calcboco_ust(defsolver, ustdom, champ, ncoupling, zone, grid)

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
type(st_ustmesh)       :: ustdom           ! maillage non structuré
type(st_zone)          :: zone
!type(mnu_zonecoupling), dimension(ncoupling) :: zcoupling   ! couplages de la zone avec ses voisines
integer                :: ncoupling        ! nombre de couplages de la zone
type(st_grid)          :: grid

! -- Declaration des sorties --
type(st_field)         :: champ            ! champ des états

! -- Declaration des variables internes --
integer :: ib, ir                    ! index de conditions aux limites et de couplage
integer :: idef                      ! index de definitions des conditions aux limites
integer :: nrac                      ! numéro de raccord

! -- Debut de la procedure --

do ib = 1, ustdom%nboco

  idef = ustdom%boco(ib)%idefboco
  !print*,"!! DEBUG boco(",ib,") défini dans defboco(",idef,")"

  ! Traitement des conditions aux limites communes aux solveurs

  select case(defsolver%boco(idef)%typ_boco)

  case(bc_geo_sym)
    call erreur("Développement","'bc_geo_sym' : Cas non implémenté")
    
  case(bc_geo_period)
    call erreur("Développement","'bc_geo_period' : Cas non implémenté")
    
  case(bc_geo_extrapol)
    call calcboco_ust_extrapol(defsolver%boco(idef), ustdom%boco(ib), ustdom, champ)

! PROVISOIRE : à retirer
  case(bc_connection)
    call erreur("Développement","'bc_connection' : Cas non implémenté")
!  
!  case(bc_coupling)
!    ! -- détermination de l'indice du couplage
!    do ir = 1, ncoupling
!      if (samestring(zone%coupling(ir)%family, ustdom%boco(ib)%family)) then
!        nrac = ir
!      endif
!    enddo
!
!    call calcboco_ust_coupling(defsolver%boco(idef), ustdom%boco(ib), ustdom, champ, &
!    				zone%coupling(nrac)%zcoupling%cond_coupling, &
!                                zone%coupling(nrac)%zcoupling%solvercoupling, &
!                                zone%coupling(nrac)%zcoupling%bocotype)
!   
  case default 
    select case(defsolver%boco(idef)%boco_unif)
    case(uniform)
      call calcboco_ust_unif(defsolver%boco(idef), ustdom%boco(ib), ustdom, champ, &
                            defsolver%typ_solver, defsolver, grid)
    case(nonuniform)
      call calcboco_ust_nunif(defsolver%boco(idef), ustdom%boco(ib), ustdom, champ, &
                            defsolver%typ_solver, defsolver, grid)
 
    endselect

  endselect

enddo


endsubroutine calcboco_ust

!------------------------------------------------------------------------------!
! Historique des modifications
!
! avril 2003 : création de la procédure
!------------------------------------------------------------------------------!
