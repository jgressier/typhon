!------------------------------------------------------------------------------!
! Procedure : calcboco_kdif               Auteur : J. Gressier/E. Radenac
!                                         Date   : Avril 2003
! Fonction                                Modif  : (cf Historique)
!   Calcul des conditions aux limites non uniformes pour la conduction de la 
!   chaleur
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine calcboco_kdif(curtime, defsolver, defboco, ustboco, umesh, bccon, defspat)

use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_SOLVER
use MENU_BOCO
use USTMESH
use DEFFIELD
use MENU_NUM 
use MGRID

implicit none

! -- INPUTS --
real(krp)        :: curtime          ! current time
type(mnu_boco)   :: defboco          ! parametres de conditions aux limites
type(st_ustboco) :: ustboco          ! lieu d'application des conditions aux limites
type(mnu_solver) :: defsolver        ! type d'equation a resoudre
type(mnu_spat)   :: defspat
type(st_ustmesh) :: umesh

! -- OUTPUTS --
type(st_bccon)   :: bccon            ! cells, faces and ghost celles

! -- Internal variables --
integer          :: ifb, if, ip      ! index de liste, index de face limite et parametres
integer          :: icell, ighost    ! index de cellule interieure, et de cellule fictive

! -- BODY --

select case(defboco%typ_boco) 

case(bc_wall_isoth)
  call setboco_kdif_isoth(curtime, defboco%boco_unif, ustboco, umesh, bccon, defboco%boco_kdif)

case(bc_wall_flux)
  call init_genericfield(ustboco%bocofield, 0._krp, v3d(0._krp, 0._krp, 0._krp))
  call setboco_kdif_flux(curtime, defboco%boco_unif, ustboco, umesh, bccon, &
                         defsolver, defboco%boco_kdif, defspat)

case(bc_wall_hconv)
  call init_genericfield(ustboco%bocofield, 0._krp, v3d(0._krp, 0._krp, 0._krp))
  call setboco_kdif_hconv(curtime, defboco%boco_unif, ustboco, umesh, bccon, &
                          defsolver, defboco%boco_kdif, defspat)

case(bc_wall_hgen)
  call init_genericfield(ustboco%bocofield, 0._krp, v3d(0._krp, 0._krp, 0._krp))
  call setboco_kdif_flux(curtime, defboco%boco_unif, ustboco, umesh, bccon, &
                         defsolver, defboco%boco_kdif, defspat) 
  call setboco_kdif_hconv(curtime, defboco%boco_unif, ustboco, umesh, bccon, &
                          defsolver, defboco%boco_kdif, defspat)

case default
  call erreur("Internal error","unknown boundary condition (calcboco_kdif)")

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
! nov  2006 : generalized convection condition
!------------------------------------------------------------------------------!
