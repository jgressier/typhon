!------------------------------------------------------------------------------!
! Procedure : calcboco_ns                 Auteur : J. Gressier
!                                         Date   : July 2004
! Fonction                                Modif  : (see historique)
!   Computation of Navier-Stokes boundary conditions
!   Call of suited subroutines
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine calcboco_ns(curtime, defsolver, defboco, ustboco, umesh, bccon)

use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_SOLVER
use MENU_BOCO
use USTMESH
use DEFFIELD 

implicit none

! -- INPUTS --
real(krp)        :: curtime
type(mnu_boco)   :: defboco          ! parametres de conditions aux limites
type(st_ustboco) :: ustboco          ! lieu d'application des conditions aux limites
type(mnu_solver) :: defsolver        ! type d'equation a resoudre
type(st_ustmesh) :: umesh            ! maillage en entree, champ en sortie

! -- Inputs/Outputs --
type(st_bccon) :: bccon  ! pointer of send or receive fields

! -- Internal variables --
integer          :: ifb, if, ip      ! index de liste, index de face limite et parametres
integer          :: icell, ighost    ! index de cellule interieure, et de cellule fictive

! -- BODY --

select case(defboco%typ_boco) 

case(bc_inlet_sup)
  call setboco_ns_inlet_sup(defsolver, defboco%boco_unif, defboco%boco_ns, &
                            ustboco, umesh, bccon, curtime)

case(bc_inlet_sub)
  call setboco_ns_inlet_sub(defsolver, defboco%boco_unif, defboco%boco_ns, &
                            ustboco, umesh, bccon, curtime)

case(bc_outlet_sup)
  call setboco_ns_outlet_sup(defsolver%defns, defboco%boco_unif, defboco%boco_ns, &
                             ustboco, umesh, bccon, curtime)

case(bc_outlet_sub)
  call setboco_ns_outlet_sub(defsolver%defns, defboco%boco_unif, defboco%boco_ns, &
                             ustboco, umesh, bccon, curtime)

case(bc_wall_isoth)
  call setboco_ns_isoth(defsolver%defns, defsolver%defale, defsolver%defmrf, defboco%boco_unif, ustboco, &
                          umesh, bccon, defboco%boco_ns, curtime)

case(bc_wall_flux)
  call init_genericfield(ustboco%bocofield, 0._krp, v3d(0._krp, 0._krp, 0._krp))
  call setboco_ns_flux(defsolver%defns, defsolver%defale, defsolver%defmrf, defboco%boco_unif, ustboco, &
                          umesh, bccon, defboco%boco_ns, curtime)

case(bc_wall_hconv)
  call init_genericfield(ustboco%bocofield, 0._krp, v3d(0._krp, 0._krp, 0._krp))
  call setboco_ns_hconv(defsolver%defns, defsolver%defale, defsolver%defmrf, defboco%boco_unif, ustboco, &
                          umesh, bccon, defboco%boco_ns, curtime)

case(bc_wall_hgen)
  call init_genericfield(ustboco%bocofield, 0._krp, v3d(0._krp, 0._krp, 0._krp))
  call setboco_ns_flux(defsolver%defns, defsolver%defale, defsolver%defmrf, defboco%boco_unif, ustboco, &
                          umesh, bccon, defboco%boco_ns, curtime) 
  call setboco_ns_hconv(defsolver%defns, defsolver%defale, defsolver%defmrf, defboco%boco_unif, ustboco, &
                          umesh, bccon, defboco%boco_ns, curtime)

case default
  call error_stop("Internal error: unknown boundary condition (calcboco_ns)")

endselect

endsubroutine calcboco_ns
!------------------------------------------------------------------------------!
! Changes history
!
! july 2004 : creation (called)
! oct  2004 : field chained list
! feb  2011 : MRF parameters passed to setboco_ns_inlet_sub/sup
!------------------------------------------------------------------------------!
