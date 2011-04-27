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
                              calc_jac, jacL, jacR, curtime)

use OUTPUT
use PACKET
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
real(krp)        :: curtime          ! temps courant

! -- Inputs/Outputs --
type(st_field)   :: field            ! champ des valeurs et residus

! -- Outputs --
type(st_genericfield)   :: flux        ! flux physiques
type(st_mattab)         :: jacL, jacR  ! jacobiennes associees (gauche et droite)

! -- Internal variables --
logical :: gradneeded           ! use gradients or not
logical :: theo_jac, num_jac    ! compute analytical or numerical jacobians
integer :: if                   ! face index
integer :: buf                  ! buffer size
integer :: ib, nblock           ! block index and number of blocks
integer, pointer :: ista(:), iend(:)           ! starting and ending index
integer :: it                   ! index de tableau
integer :: icl, icr             ! index de cellule a gauche et a droite
type(st_nsetat)       :: QL, QR
type(st_genericfield) :: gradL, gradR         ! nblock size arrays of gradients
type(v3d), dimension(:), allocatable &
                      :: cg_l, cg_r           ! tableau des centres de cellules a gauche et a droite   

! -- BODY --

if (.not.field%allocqhres) call erreur("Internal error", "Face extrapolated states not defined")

call new_buf_index(umesh%nface, face_buffer, nblock, ista, iend)

!$OMP PARALLEL private(ib, QL, QR, cg_l, cg_r, gradL, gradR, buf) shared (flux, jacL, jacR)

allocate(  cg_l(face_buffer),   cg_r(face_buffer))
call new(gradL, face_buffer, field%gradient%nscal, field%gradient%nvect, field%gradient%ntens)
call new(gradR, face_buffer, field%gradient%nscal, field%gradient%nvect, field%gradient%ntens)

!$OMP DO 

do ib = 1, nblock

  buf = iend(ib)-ista(ib)+1

  ! pointers links
  QL%density  => field%cell_l%tabscal(1)%scal(ista(ib):iend(ib))
  QR%density  => field%cell_r%tabscal(1)%scal(ista(ib):iend(ib))
  QL%pressure => field%cell_l%tabscal(2)%scal(ista(ib):iend(ib))
  QR%pressure => field%cell_r%tabscal(2)%scal(ista(ib):iend(ib))
  QL%velocity => field%cell_l%tabvect(1)%vect(ista(ib):iend(ib))
  QR%velocity => field%cell_r%tabvect(1)%vect(ista(ib):iend(ib))

  !----------------------------------------------------------------------
  ! computation of INVISCID fluxes
  !----------------------------------------------------------------------

  theo_jac = (calc_jac).and.(defspat%jac_hyp /= jac_diffnum)
  num_jac  = (calc_jac).and.(defspat%jac_hyp == jac_diffnum)

  call calc_flux_inviscid(defsolver, defspat,                             &
                          buf, ista(ib), umesh%mesh%iface(ista(ib):iend(ib), 1, 1), &
                          QL, QR, flux, theo_jac, jacL, jacR)

  if (num_jac) then
    call calc_jacnum_inviscid(defsolver, defspat,                             &
                            buf, ista(ib), umesh%mesh%iface(ista(ib):iend(ib), 1, 1), &
                            QL, QR, flux, theo_jac, jacL, jacR)
  endif

  !----------------------------------------------------------------------
  ! computation of ALE fluxes
  !----------------------------------------------------------------------

  call calc_flux_ale(defsolver, buf, ista(ib), umesh, cg_l, cg_r, &
                          QL, QR, flux, theo_jac, jacL, jacR)

  !----------------------------------------------------------------------
  ! computation of VISCOUS fluxes
  !----------------------------------------------------------------------
  select case(defsolver%defns%typ_fluid)

  case(eqEULER, eqEULERaxi)
    ! nothing to do

  case(eqNSLAM)
    ! -- redirection of cell centers 
    cg_l(1:buf) = umesh%mesh%centre(umesh%facecell%fils(ista(ib):iend(ib),1), 1, 1)
    cg_r(1:buf) = umesh%mesh%centre(umesh%facecell%fils(ista(ib):iend(ib),2), 1, 1)

    ! -- redirection of gradients
    call distrib_field(field%gradient, umesh%facecell, ista(ib), iend(ib), &
                       gradL, gradR, 1)

    call calc_flux_viscous(defsolver, defspat,                        &
                           buf, ista(ib), umesh%mesh%iface(ista(ib):iend(ib), 1, 1), &
                           cg_l, cg_r, QL, QR, gradL, gradR, flux,        &
                           calc_jac, jacL, jacR)

  case(eqNSLAMaxi)
     call error_stop("development: axisymmetric NS not implemented")   

  case(eqRANS)
    call error_stop("development: turbulence modeling not implemented")   

  case default
    call error_stop("viscous flux computation: unknown model")
  endselect

  !----------------------------------------------------------------------
  ! end of nblock

enddo
!$OMP END DO

deallocate(cg_l, cg_r)
call delete(gradL)
call delete(gradR)

!$OMP END PARALLEL

deallocate(ista, iend)

! --- EXT source terms ---

call calc_source_ext(umesh, field, defsolver%defns, curtime)

! --- MRF source terms ---

call calc_source_mrf(umesh, field, defsolver%defmrf, curtime)

! --- AXISYM source terms ---

call calc_source_axisym(umesh, field, defsolver%defns, curtime)   !must be the last source to call

!-------------------------------------------------------------
! flux assignment or modification on boundary conditions

call ns_bocoflux(defsolver, umesh, flux, field, defspat)

if (calc_jac) then
  call ns_bocojacobian(defsolver, defspat, umesh, flux, field%etatprim, field%gradient, jacL, jacR)
endif

endsubroutine integration_ns_ust
!------------------------------------------------------------------------------!
! Changes history
!
! Jul 2004: created, basic calls
! Nov 2004: high order interpolation
! Feb 2005: call to viscous flux computation
! nov 2007: add post limitation
! May 2009: split high order extrapolation to calc_hres_states
! Dec 2010: call for Moving Reference Frame source terms calculation (A. Gardi)
!------------------------------------------------------------------------------!
