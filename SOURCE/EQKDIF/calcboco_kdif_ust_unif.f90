!------------------------------------------------------------------------------!
! Procedure : calcboco_kdif_ust_unif      Auteur : J. Gressier/E. Radenac
!                                         Date   : Avril 2003
! Fonction                                Modif  : Novembre 2003 (cf Historique)
!   Calcul des conditions aux limites uniformes pour la conduction de la 
!   chaleur
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine calcboco_kdif_ust_unif(defboco, ustboco, ustdom, champ, defsolver, grid)

use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_SOLVER
use MENU_BOCO
use USTMESH
use DEFFIELD 

implicit none

! -- Declaration des entrées --
type(mnu_boco)   :: defboco          ! paramètres de conditions aux limites
type(st_ustboco) :: ustboco          ! lieu d'application des conditions aux limites
type(st_ustmesh) :: ustdom           ! maillage non structuré
type(mnu_solver) :: defsolver        ! type d'équation à résoudre
type(st_grid)    :: grid

! -- Declaration des sorties --
type(st_field)   :: champ            ! champ des états

! -- Declaration des variables internes --
integer          :: ifb, if, ip      ! index de liste, index de face limite et paramètres
integer          :: icell, ighost    ! index de cellule intérieure, et de cellule fictive
type(st_genericfield), pointer :: pbocofield

! -- Debut de la procedure --

select case(defboco%typ_boco)

case(bc_wall_adiab) 
  pbocofield => newbocofield(grid,ustboco%nface,1,0,0)  
  call setboco_kdif_flux_unif(ustboco, ustdom, champ, &
                              pbocofield%tabscal(1)%scal, defsolver, &
                              defboco%boco_kdif%flux)
  ustboco%bocofield => pbocofield

case(bc_wall_isoth)
  call setboco_kdif_isoth_unif(ustboco, ustdom, champ, &
                               defboco%boco_kdif%temp_wall)
case(bc_wall_flux)
  pbocofield => newbocofield(grid,ustboco%nface,1,0,0)
  call setboco_kdif_flux_unif(ustboco, ustdom, champ, &
                              pbocofield%tabscal(1)%scal, defsolver, &
                              defboco%boco_kdif%flux)
  ustboco%bocofield => pbocofield

case(bc_wall_hconv)
  pbocofield => newbocofield(grid,ustboco%nface,1,0,0)
  call setboco_kdif_hconv_unif(ustboco, ustdom, champ, &
                               pbocofield%tabscal(1)%scal, defsolver, &
                               defboco%boco_kdif%h_conv, &
                               defboco%boco_kdif%temp_conv)
  ustboco%bocofield => pbocofield

endselect


endsubroutine calcboco_kdif_ust_unif

!------------------------------------------------------------------------------!
! Historique des modifications
!
! avril 2003 (v0.0.1b) : création de la procédure
! juin  2003           : màj pour gestion variables conservatives et primitves
! nov 2003             : distinction entre conditions uniformes et non 
!                        uniformes (ancien nom : calcboco_kdif_ust)
!------------------------------------------------------------------------------!
