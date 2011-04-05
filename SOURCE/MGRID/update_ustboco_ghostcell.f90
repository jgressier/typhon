!------------------------------------------------------------------------------!
! Procedure : update_ustboco_ghostcell               Authors : JG + A. Gardi
!                                                    Created : Mars 2011
! Fonction  
!    Update the ghostcell parameters after mesh movement due to ALE
!
!------------------------------------------------------------------------------!
subroutine update_ustboco_ghostcell(ib, defboco, umesh)
!DEV: procedure vastly similar to init_ustboco_ghostcell, to be rearranged in future!

use TYPHMAKE
!use VARCOM
use OUTPUT
use USTMESH
use MENU_BOCO

implicit none

! -- INPUTS --
integer        :: ib                     ! numero de condition aux limites
type(mnu_boco) :: defboco                ! parametres du solveur

! -- INPUTS/OUTPUTS --
type(st_ustmesh) :: umesh                ! unstructured mesh

! -- OUTPUTS --

! -- Internal variables --
integer   :: if                          ! face index in boco list
integer   :: ic1, ic2, iface             ! cells and face indexes
type(v3d) :: fn, gc                      ! face normal vector, internal cell center

! -- Body --

do if = 1, umesh%boco(ib)%nface
  
  ! --  affectation de connectivite face limites -> cellules fictives --

  iface = umesh%boco(ib)%iface(if)            ! face index
  ic1   = umesh%facecell%fils(iface,1)        ! internal/reference cell index
  ic2   = umesh%facecell%fils(iface,2)        ! ghost cell index

  ! -- geometrical definition of ghost cell --

  select case(defboco%typ_boco)

  case(bc_geo_sym)
    fn = umesh%mesh%iface(iface,1,1)%normale
    gc = umesh%mesh%centre(ic1,1,1)
    umesh%mesh%volume(ic2,1,1) = umesh%mesh%volume(ic1,1,1)
    umesh%mesh%centre(ic2,1,1) = gc + (2._krp*((umesh%mesh%iface(iface,1,1)%centre-gc).scal.fn))*fn

  case(bc_wall_isoth, bc_wall_flux, bc_wall_adiab)
    umesh%mesh%volume(ic2,1,1) = umesh%mesh%volume(ic1,1,1)
    umesh%mesh%centre(ic2,1,1) = 2._krp*umesh%mesh%iface(iface,1,1)%centre - umesh%mesh%centre(ic1,1,1)

  case(bc_geo_period)
    call erreur("Development", "Initialization of periodic boundary conditions not yet implemented")

  case default
    call erreur("Internal error", "unknown type of boundary conditions with ghostcell implementation")
  endselect

enddo

endsubroutine update_ustboco_ghostcell
!------------------------------------------------------------------------------!
! Changes history
!
! Mar  2011 : created from init_ustboco_ghostcell
!------------------------------------------------------------------------------!
