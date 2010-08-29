!------------------------------------------------------------------------------!
! Procedure : integration_ns_ust  
!      
! Fonction  
!   NS flux computation 
!   - packet based computation
!   - high order interpolation
!
!------------------------------------------------------------------------------!
subroutine integration_ns_ust(defsolver, defspat, umesh, field, flux, &
                              calc_jac, jacL, jacR)

use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_SOLVER
use MENU_NUM
use USTMESH
use DEFFIELD
use EQNS
use MATRIX_ARRAY

implicit none

! -- Inputs --
type(mnu_solver) :: defsolver        ! type d'equation a resoudre
type(mnu_spat)   :: defspat          ! parametres d'integration spatiale
type(st_ustmesh) :: umesh            ! umesh non structure a integrer
logical          :: calc_jac         ! choix de calcul de la jacobienne

! -- Inputs/Outputs --
type(st_field)   :: field            ! champ des valeurs et residus

! -- Outputs --
type(st_genericfield)   :: flux        ! flux physiques
type(st_mattab)         :: jacL, jacR  ! jacobiennes associees (gauche et droite)

! -- Internal variables --
logical :: gradneeded           ! use gradients or not
logical :: theo_jac, num_jac    ! compute analytical or numerical jacobians
integer :: if                   ! face index
integer :: buf, dimbuf, dimbuf1 ! buffer size (current, regular and first)
integer :: ib, nblock           ! block index and number of blocks
integer :: ista, iend           ! starting and ending index
integer :: it                   ! index de tableau
integer :: icl, icr             ! index de cellule a gauche et a droite
type(st_nsetat)       :: QL, QR
type(st_genericfield) :: gradL, gradR         ! nblock size arrays of gradients
type(v3d), dimension(:), allocatable &
                      :: cg_l, cg_r           ! tableau des centres de cellules a gauche et a droite   

! -- BODY --

if (.not.field%allocqhres) call erreur("Internal error", "Face extrapolated states not defined")

call calc_buffer(umesh%nface, cell_buffer, nblock, dimbuf, dimbuf1)

allocate(  cg_l(dimbuf),   cg_r(dimbuf))
call new(gradL, dimbuf, field%gradient%nscal, field%gradient%nvect, field%gradient%ntens)
call new(gradR, dimbuf, field%gradient%nscal, field%gradient%nvect, field%gradient%ntens)

ista = 1
buf  = dimbuf1

do ib = 1, nblock

  iend = ista+buf-1

  ! pointers links
  QL%density  => field%cell_l%tabscal(1)%scal(ista:iend)
  QR%density  => field%cell_r%tabscal(1)%scal(ista:iend)
  QL%pressure => field%cell_l%tabscal(2)%scal(ista:iend)
  QR%pressure => field%cell_r%tabscal(2)%scal(ista:iend)
  QL%velocity => field%cell_l%tabvect(1)%vect(ista:iend)
  QR%velocity => field%cell_r%tabvect(1)%vect(ista:iend)

  !----------------------------------------------------------------------
  ! computation of INVISCID fluxes
  !----------------------------------------------------------------------

  theo_jac = (calc_jac).and.(defspat%jac_hyp /= jac_diffnum)
  num_jac  = (calc_jac).and.(defspat%jac_hyp == jac_diffnum)

  call calc_flux_inviscid(defsolver, defspat,                             &
                          buf, ista, umesh%mesh%iface(ista:iend, 1, 1), &
                          QL, QR, flux, theo_jac, jacL, jacR)

  if (num_jac) then
    call calc_jacnum_inviscid(defsolver, defspat,                             &
                            buf, ista, umesh%mesh%iface(ista:iend, 1, 1), &
                            QL, QR, flux, theo_jac, jacL, jacR)
  endif

  !----------------------------------------------------------------------
  ! computation of VISCOUS fluxes
  !----------------------------------------------------------------------
  select case(defsolver%defns%typ_fluid)

  case(eqEULER)
    ! nothing to do

  case(eqNSLAM)
    ! -- redirection of cell centers 
    cg_l(1:buf) = umesh%mesh%centre(umesh%facecell%fils(ista:iend,1), 1, 1)
    cg_r(1:buf) = umesh%mesh%centre(umesh%facecell%fils(ista:iend,2), 1, 1)

    ! -- redirection of gradients
    call distrib_field(field%gradient, umesh%facecell, ista, iend, &
                       gradL, gradR, 1)
    call calc_flux_viscous(defsolver, defspat,                        &
                           buf, ista, umesh%mesh%iface(ista:iend, 1, 1), &
                           cg_l, cg_r, QL, QR, gradL, gradR, flux,        &
                           calc_jac, jacL, jacR)
  case(eqRANS)
    call erreur("development", "turbulence modeling not implemented")   

  case default
    call erreur("viscous flux computation", "unknown model")
  endselect

  !----------------------------------------------------------------------
  ! end of nblock

  ista = ista + buf
  buf  = dimbuf         ! tous les nblocks suivants sont de taille dimbuf
  
enddo

!!$print*, '--L---------'
!!$do ib = 1, 5
!!$  print'(5e14.6)', jacL%mat(ib,:,100)
!!$enddo
!!$print*, '--R---------'
!!$do ib = 1, 5
!!$  print'(5e14.6)', jacR%mat(ib,:,100)
!!$enddo
!!$stop
!-------------------------------------------------------------
! flux assignment or modification on boundary conditions

call ns_bocoflux(defsolver, umesh, flux, field, defspat)

if (calc_jac) then
  call ns_bocojacobian(defsolver, defspat, umesh, flux, field%etatprim, field%gradient, jacL, jacR)
endif

deallocate(cg_l, cg_r)
call delete(gradL)
call delete(gradR)

endsubroutine integration_ns_ust

!------------------------------------------------------------------------------!
! Changes history
!
! Jul 2004: created, basic calls
! Nov 2004: high order interpolation
! Feb 2005: call to viscous flux computation
! nov 2007: add post limitation
! May 2009: split high order extrapolation to calc_hres_states
!------------------------------------------------------------------------------!
