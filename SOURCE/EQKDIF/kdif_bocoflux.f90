!------------------------------------------------------------------------------!
! Procedure : kdif_bocoflux                    Authors : J. Gressier/E. Radenac
!                                              Created : June 2004
! Fonction                                     Modif   : (cf History)
!  Flux aux faces limites quand necessaire
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine kdif_bocoflux(defsolver, domaine, flux, stprim)

use TYPHMAKE
use OUTPUT
use VARCOM
use MATERIAU
use EQKDIF
use MENU_SOLVER
use MENU_BOCO
use MENU_KDIF
use USTMESH
use DEFFIELD 

implicit none

! -- Inputs --
type(mnu_solver)        :: defsolver        ! type d'equation a resoudre
type(st_ustmesh)        :: domaine          ! domaine non structure a integrer
type(st_genericfield)   :: stprim           ! primitive state field

! -- Outputs --
type(st_genericfield)   :: flux             ! flux physiques

! -- Internal variables --
integer                 :: ifb, if, ib, idef ! index de liste, index de face limite et parametres

! -- Debut de la procedure --

do ib = 1, domaine%nboco

  idef = domaine%boco(ib)%idefboco

  !---------------------------------------------------------------------
  ! assign flux as already computed flux in bocofield

  select case(defsolver%boco(idef)%typ_boco)
  case(bc_wall_adiab, bc_wall_flux, bc_wall_hconv) 
    do ifb = 1, domaine%boco(ib)%nface
      if = domaine%boco(ib)%iface(ifb)
      flux%tabscal(1)%scal(if) = domaine%boco(ib)%bocofield%tabscal(1)%scal(ifb)
    enddo
  endselect

  !---------------------------------------------------------------------
  ! external radiative flux 

  select case(defsolver%boco(idef)%typ_boco)
  case(bc_wall_adiab, bc_wall_flux, bc_wall_hconv) 
    select case(defsolver%boco(idef)%boco_kdif%radiating)
    case(rad_none)
    case(rad_direct)
      call add_kdif_radiativeflux(defsolver, idef, domaine, flux, stprim)
    case(rad_coupled)
      call erreur("Development", "(coupled radiation) Feature not yet implemented")
    case default
      call erreur("Parameter bug", "unexpected parameter for radiating feature")
    endselect
  endselect

enddo

endsubroutine kdif_bocoflux

!------------------------------------------------------------------------------!
! Change history
!
! june 2004 : created
! apr  2005 : renamed (fluxlimite->kdif_bocoflux)
! apr  2005 : modification of fluxes by radiative transfer
!------------------------------------------------------------------------------!
