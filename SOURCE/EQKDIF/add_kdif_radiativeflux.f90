!------------------------------------------------------------------------------!
! Procedure : add_kdif_radiativeflux                    Authors : J. Gressier
!                                              Created : April 2005
! Fonction                                     Modif   : (cf History)
!  Flux aux faces limites quand necessaire
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine add_kdif_radiativeflux(defsolver, ib, domain, flux, stprim)

use TYPHMAKE
use OUTPUT
use VARCOM
use MATERIAU
use EQKDIF
use MENU_SOLVER
use MENU_BOCO
use USTMESH
use DEFFIELD 

implicit none

! -- Inputs --
type(mnu_solver)        :: defsolver        ! solver parameters
integer                 :: ib               ! boundary condition parameters
type(st_ustmesh)        :: domain           ! unstructured domain (cell, face, connectivity)
type(st_genericfield)   :: stprim           ! primitive state

! -- Outputs --
type(st_genericfield)   :: flux             ! physical flux

! -- Internal variables --
integer                 :: ifb, if, ic       ! index de liste, index de face limite et parametres
real(krp)               :: cst, Tfar4

! -- Debut de la procedure --

cst   = stefan_cst * defsolver%boco(ib)%boco_kdif%emmissivity
Tfar4 = defsolver%boco(ib)%boco_kdif%rad_Tinf**4

!---------------------------------------------------------------------
! add radiative term to flux (should eventually be initialized)

do ifb = 1, domain%boco(ib)%nface
  if = domain%boco(ib)%iface(ifb)
  ic = domain%facecell%fils(if,2)    ! ghost cell is always right cell
  flux%tabscal(1)%scal(if) = flux%tabscal(1)%scal(if) + cst*(stprim%tabscal(1)%scal(ic)**4 -Tfar4)
enddo

endsubroutine add_kdif_radiativeflux

!------------------------------------------------------------------------------!
! Change history
!
! apr  2005 : created
!------------------------------------------------------------------------------!
