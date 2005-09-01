!------------------------------------------------------------------------------!
! Procedure : calcboco_kdif               Auteur : J. Gressier/E. Radenac
!                                         Date   : Avril 2003
! Fonction                                Modif  : (cf Historique)
!   Calcul des conditions aux limites non uniformes pour la conduction de la 
!   chaleur
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine calcboco_kdif(defsolver, defboco, ustboco, grid, defspat)

use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_SOLVER
use MENU_BOCO
use USTMESH
use DEFFIELD
use MENU_NUM 

implicit none

! -- Declaration des entrees --
type(mnu_boco)   :: defboco          ! parametres de conditions aux limites
type(st_ustboco) :: ustboco          ! lieu d'application des conditions aux limites
type(mnu_solver) :: defsolver        ! type d'equation a resoudre
type(mnu_spat)   :: defspat

! -- Declaration des sorties --
type(st_grid)    :: grid             ! mise a jour du champ (maillage en entree)

! -- Declaration des variables internes --
integer          :: ifb, if, ip      ! index de liste, index de face limite et parametres
integer          :: icell, ighost    ! index de cellule interieure, et de cellule fictive
type(st_genericfield), pointer :: pbcf

! -- Debut de la procedure --

select case(defboco%typ_boco) 

case(bc_wall_isoth)
  call setboco_kdif_isoth(defboco%boco_unif, ustboco, grid%umesh, grid%info%field_loc, defboco%boco_kdif)

case(bc_wall_flux)
  pbcf => newbocofield(grid,ustboco%nface,1,0,0)  
  call setboco_kdif_flux(defboco%boco_unif, ustboco, grid%umesh, grid%info%field_loc, pbcf%tabscal(1)%scal, &
                         defsolver, defboco%boco_kdif, defspat)
  ustboco%bocofield => pbcf

case(bc_wall_hconv)
  pbcf => newbocofield(grid,ustboco%nface,1,0,0) 
  call setboco_kdif_hconv(defboco%boco_unif, ustboco, grid%umesh, grid%info%field_loc, pbcf%tabscal(1)%scal, &
                          defsolver, defboco%boco_kdif, defspat)
  ustboco%bocofield => pbcf

case default
  call erreur("Developpement","Condition limite inconnu a ce niveau (calcboco_kdif)")

endselect


endsubroutine calcboco_kdif

!------------------------------------------------------------------------------!
! Historique des modifications
!
! avr  2003 : creation de la procedure
! juin 2003 : maj pour gestion variables conservatives et primitves
! nov  2003 : distinction entre conditions uniformes et non 
!             uniformes (ancien nom : calcboco_kdif_ust)
! july 2004 : merge of uniform or non-uniform boundary conditions
!             (old name: calc_boco_kdif_?unif)
! oct  2004 : field chained list
!------------------------------------------------------------------------------!
