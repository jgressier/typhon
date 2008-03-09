!------------------------------------------------------------------------------!
! Procedure : kdif_bocoflux                    Authors : J. Gressier/E. Radenac
!                                              Created : June 2004
! Fonction                                     Modif   : (cf History)
!  Flux aux faces limites quand necessaire
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine kdif_bocoflux(defsolver, domaine, flux, stprim, calc_jac, jacL, jacR)

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
use MATRIX_ARRAY

implicit none

! -- Inputs --
type(mnu_solver)        :: defsolver        ! type of equation to solve
type(st_ustmesh)        :: domaine          ! unstructured domain to integrate
type(st_genericfield)   :: stprim           ! primitive state field
logical                 :: calc_jac         ! should compute jacobian mat. or not

! -- Outputs --
type(st_genericfield)   :: flux             ! physical fluxes
type(st_mattab)         :: jacL, jacR       ! jacobian matrix of the flux

! -- Internal variables --
integer    :: ifb, if, ib, idef ! index de liste, index de face limite et parametres
logical    :: coupled

! -- Body --

do ib = 1, domaine%nboco

  idef = domaine%boco(ib)%idefboco

  !---------------------------------------------------------------------
  ! assign flux as already computed flux in bocofield

  select case(defsolver%boco(idef)%typ_boco)
  case(bc_wall_adiab, bc_wall_flux, bc_wall_hconv,bc_wall_hgen) 
    do ifb = 1, domaine%boco(ib)%nface
      if = domaine%boco(ib)%iface(ifb)
      flux%tabscal(1)%scal(if) = domaine%boco(ib)%bocofield%tabscal(1)%scal(ifb)
    enddo
  endselect

enddo

!---------------------------------------------------------------------
! external radiative flux 

coupled = .false.

do ib = 1, domaine%nboco
  idef = domaine%boco(ib)%idefboco

  select case(defsolver%boco(idef)%typ_boco)
  case(bc_wall_adiab, bc_wall_flux, bc_wall_hconv, bc_wall_hgen) 

    select case(defsolver%boco(idef)%boco_kdif%radiating)
    case(rad_none)
      ! nothing to do
    case(rad_direct, rad_coupled)
      call add_kdif_radiativeflux(defsolver, idef, domaine, ib, flux, stprim)
      if (defsolver%boco(idef)%boco_kdif%radiating == rad_coupled) coupled = .true.
    case default
      call erreur("Parameter bug", "unexpected parameter for radiating feature")
    endselect
  endselect
enddo

if (coupled) call add_kdif_coupled_radflux(defsolver, domaine, flux, stprim)



endsubroutine kdif_bocoflux

!------------------------------------------------------------------------------!
! Change history
!
! june 2004 : created
! apr  2005 : renamed (fluxlimite->kdif_bocoflux)
! apr  2005 : modification of fluxes by radiative transfer
!------------------------------------------------------------------------------!
