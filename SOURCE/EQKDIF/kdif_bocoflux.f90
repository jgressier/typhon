!------------------------------------------------------------------------------!
! Procedure : kdif_bocoflux  
!                            
! Fonction                   
!  Flux aux faces limites quand necessaire
!
!------------------------------------------------------------------------------!
subroutine kdif_bocoflux(defsolver, umesh, flux, stprim, calc_jac, jacL, jacR)

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
type(st_ustmesh)        :: umesh            ! unstructured mesh
type(st_genericfield)   :: stprim           ! primitive state field
logical                 :: calc_jac         ! should compute jacobian mat. or not

! -- Outputs --
type(st_genericfield)   :: flux             ! physical fluxes
type(st_mattab)         :: jacL, jacR       ! jacobian matrix of the flux

! -- Internal variables --
integer    :: ifb, if, ib, idef ! index de liste, index de face limite et parametres
integer    :: dim
logical    :: coupled

! -- Body --

do ib = 1, umesh%nboco

  idef = umesh%boco(ib)%idefboco

  !---------------------------------------------------------------------
  ! assign flux as already computed flux in bocofield

  select case(defsolver%boco(idef)%typ_boco)
  case(bc_wall_adiab, bc_wall_flux, bc_wall_hconv,bc_wall_hgen) 
    do ifb = 1, umesh%boco(ib)%nface
      if = umesh%boco(ib)%iface(ifb)
      flux%tabscal(1)%scal(if) = umesh%boco(ib)%bocofield%tabscal(1)%scal(ifb)
    enddo
  endselect

  !---------------------------------------------------------------------
  ! JACOBIAN treatment

  if (calc_jac) then

    dim = jacL%dim
    select case(defsolver%boco(idef)%typ_boco)
    case(bc_wall_isoth)
      ! nothing to do 
    case(bc_wall_adiab, bc_wall_flux) 
      do ifb = 1, umesh%boco(ib)%nface
        if = umesh%boco(ib)%iface(ifb)
        jacL%mat(1:dim,1:dim,if) = jacL%mat(1:dim,1:dim,if) + jacR%mat(1:dim,1:dim,if)
        jacR%mat(1:dim,1:dim,if) = 0._krp
      enddo
    endselect

  endif

enddo

!---------------------------------------------------------------------
! external radiative flux 

coupled = .false.

do ib = 1, umesh%nboco
  idef = umesh%boco(ib)%idefboco

  select case(defsolver%boco(idef)%typ_boco)
  case(bc_wall_adiab, bc_wall_flux, bc_wall_hconv, bc_wall_hgen) 

    select case(defsolver%boco(idef)%boco_kdif%radiating)
    case(rad_none)
      ! nothing to do
    case(rad_direct, rad_coupled)
      call add_kdif_radiativeflux(defsolver, idef, umesh, ib, flux, stprim)
      if (defsolver%boco(idef)%boco_kdif%radiating == rad_coupled) coupled = .true.
    case default
      call erreur("Parameter bug", "unexpected parameter for radiating feature")
    endselect
  endselect
enddo

if (coupled) call add_kdif_coupled_radflux(defsolver, umesh, flux, stprim)



endsubroutine kdif_bocoflux

!------------------------------------------------------------------------------!
! Change history
!
! june 2004 : created
! apr  2005 : renamed (fluxlimite->kdif_bocoflux)
! apr  2005 : modification of fluxes by radiative transfer
! Apr  2008 : boco implicitation
!------------------------------------------------------------------------------!
