!------------------------------------------------------------------------------!
! Procedure : add_kdif_radiativeflux                   Authors : J. Gressier
!                                                      Created : June 2005
! Fonction                                        
!  Add radiative flux to boundary fluxes
! (coupled part between domaines BC faces)
!
!------------------------------------------------------------------------------!
subroutine add_kdif_coupled_radflux(defsolver, domain, flux, stprim)

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
type(st_ustmesh)        :: domain           ! unstructured domain (cell, face, connectivity)
type(st_genericfield)   :: stprim           ! primitive state

! -- Outputs --
type(st_genericfield)   :: flux             ! physical flux

! -- Internal variables --
integer         :: idef                ! boco definition (defsolver%boco)
integer         :: ib                  ! domain boco definition
integer         :: ifb                 ! face list index
integer         :: if1, if2, ic1, ic2  ! face index and corresponding ghost cell
real(krp)       :: cst, T1_4, T2_4, viewfact

! -- Debut de la procedure --

cst   = stefan_cst * 1._krp   ! default emmissivity

!---------------------------------------------------------------------
! add coupled_rad term to flux (should eventually be initialized)

do ifb = 1, defsolver%defkdif%viewfactor%ncouple
  if1       = defsolver%defkdif%viewfactor%couple%fils(ifb,1)
  if2       = defsolver%defkdif%viewfactor%couple%fils(ifb,2)
  viewfact  = defsolver%defkdif%viewfactor%value(ifb)
  ic1       = domain%facecell%fils(if1,2)    ! ghost cell is always right cell
  ic2       = domain%facecell%fils(if2,2)    ! ghost cell is always right cell
  T1_4      = stprim%tabscal(1)%scal(ic1)**4
  T2_4      = stprim%tabscal(1)%scal(ic2)**4
  flux%tabscal(1)%scal(if1) = flux%tabscal(1)%scal(if1) - viewfact*domain%mesh%iface(if2,1,1)%surface*cst*T2_4
  flux%tabscal(1)%scal(if2) = flux%tabscal(1)%scal(if2) - viewfact*domain%mesh%iface(if1,1,1)%surface*cst*T1_4
enddo

endsubroutine add_kdif_coupled_radflux

!------------------------------------------------------------------------------!
! Change history
!
! June 2005 : created
!------------------------------------------------------------------------------!
