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
type(st_genericfield) :: cQL, cQR, gradL, gradR         ! nblock size arrays of gradients
type(v3d), dimension(:), allocatable &
                      :: cg_l, cg_r           ! tableau des centres de cellules a gauche et a droite   
logical :: allocgrad

! -- BODY --

allocgrad = .false.
if (.not.field%allocqhres) call error_stop("Internal error: Face extrapolated states not defined")

call new_buf_index(umesh%nface, face_buffer, nblock, ista, iend)

!$OMP PARALLEL private(ib, QL, QR, cg_l, cg_r, gradL, gradR, cQL, cQR, buf) shared (flux, jacL, jacR)

allocate(  cg_l(face_buffer),   cg_r(face_buffer))

fluidmodel0: select case(defsolver%defns%typ_fluid)
case(eqEuler, eqEulerAxi)
  ! nothing to allocate
case(eqNSLam, eqRANS)
  viscscheme: select case(defspat%sch_dis)
  case(dis_celldif2, dis_cellavg2, dis_cellfull)
    allocgrad = .true.
    call new_genfield(cQL,   face_buffer, field%etatcons%nscal, field%etatcons%nvect, 0)
    call new_genfield(cQR,   face_buffer, field%etatcons%nscal, field%etatcons%nvect, 0)
    call new_genfield(gradL, face_buffer, 0, field%etatcons%nscal, field%etatcons%nvect)
    call new_genfield(gradR, face_buffer, 0, field%etatcons%nscal, field%etatcons%nvect)
  case(dis_facecentered, dis_facepenalty)
    ! nothing to do
  case default
    call error_stop("internal error: unknown dissipative flux scheme (integration_ns_ust:1)")
  endselect viscscheme
case default
  call error_stop("viscous flux computation: unknown model (integration_ns_ust:1)")
endselect fluidmodel0

!$OMP DO 

do ib = 1, nblock

  buf = iend(ib)-ista(ib)+1

  !----------------------------------------------------------------------
  ! computation of INVISCID fluxes
  !----------------------------------------------------------------------

  theo_jac = (calc_jac).and.(defspat%jac_hyp /= jac_diffnum)
  num_jac  = (calc_jac).and.(defspat%jac_hyp == jac_diffnum)

  ! pointers links to face extrapolated values
  QL%density  => field%cell_l%tabscal(1)%scal(ista(ib):iend(ib))
  QR%density  => field%cell_r%tabscal(1)%scal(ista(ib):iend(ib))
  QL%pressure => field%cell_l%tabscal(2)%scal(ista(ib):iend(ib))
  QR%pressure => field%cell_r%tabscal(2)%scal(ista(ib):iend(ib))
  QL%velocity => field%cell_l%tabvect(1)%vect(ista(ib):iend(ib))
  QR%velocity => field%cell_r%tabvect(1)%vect(ista(ib):iend(ib))

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
  fluidmodel: select case(defsolver%defns%typ_fluid)

  case(eqEULER, eqEULERaxi)
    ! nothing to do

  case(eqNSLAM)
 
    viscousscheme: select case(defspat%sch_dis)
    case(dis_celldif2, dis_cellavg2, dis_cellfull)

      ! -- redirection of cell centers 
      cg_l(1:buf) = umesh%mesh%centre(umesh%facecell%fils(ista(ib):iend(ib),1), 1, 1)
      cg_r(1:buf) = umesh%mesh%centre(umesh%facecell%fils(ista(ib):iend(ib),2), 1, 1)

      ! -- redirection of states (cell to face)
      call distrib_field(field%etatprim, umesh%facecell, ista(ib), iend(ib), &
                         cQL, cQR, 1)
      ! pointers links to cell values
      QL%density  => cQL%tabscal(1)%scal(1:buf)
      QR%density  => cQR%tabscal(1)%scal(1:buf)
      QL%pressure => cQL%tabscal(2)%scal(1:buf)
      QR%pressure => cQR%tabscal(2)%scal(1:buf)
      QL%velocity => cQL%tabvect(1)%vect(1:buf)
      QR%velocity => cQR%tabvect(1)%vect(1:buf)
      ! -- redirection of gradients (cell to face)
      call distrib_field(field%gradient, umesh%facecell, ista(ib), iend(ib), &
                       gradL, gradR, 1)

      call calc_flux_viscous_cell(defsolver, defspat,                     &
                           buf, ista(ib), umesh%mesh%iface(ista(ib):iend(ib), 1, 1), &
                           cg_l, cg_r, QL, QR, gradL, gradR, flux,        &
                           calc_jac, jacL, jacR)

    case(dis_facecentered, dis_facepenalty)

      ! pointers links to face extrapolated values
      QL%density  => field%cell_l%tabscal(1)%scal(ista(ib):iend(ib))
      QR%density  => field%cell_r%tabscal(1)%scal(ista(ib):iend(ib))
      QL%pressure => field%cell_l%tabscal(2)%scal(ista(ib):iend(ib))
      QR%pressure => field%cell_r%tabscal(2)%scal(ista(ib):iend(ib))
      QL%velocity => field%cell_l%tabvect(1)%vect(ista(ib):iend(ib))
      QR%velocity => field%cell_r%tabvect(1)%vect(ista(ib):iend(ib))

      call calc_flux_viscous_face(defsolver, defspat,                        &
                           buf, ista(ib), umesh%mesh%iface(ista(ib):iend(ib), 1, 1), &
                           cg_l, cg_r, QL, QR, gradL, gradR, flux) 

    case default
      call error_stop("internal error: unknown dissipative flux scheme (integration_ns_ust:2)")
    endselect viscousscheme
              
  case(eqNSLAMaxi)
     call error_stop("development: axisymmetric NS not implemented")   

  case(eqRANS)
    call error_stop("development: turbulence modeling not implemented")   

  case default
    call error_stop("viscous flux computation: unknown model (integration_ns_ust:2)")
  endselect fluidmodel

  !----------------------------------------------------------------------
  ! end of nblock

enddo
!$OMP END DO

deallocate(cg_l, cg_r)
if (allocgrad) then
  call delete(cQL)
  call delete(cQR)
  call delete(gradL)
  call delete(gradR)
endif

!$OMP END PARALLEL

deallocate(ista, iend)

! --- EXT source terms ---

call calc_source_ext(defsolver, umesh, field, curtime)
!print*,field%residu%tabvect(1)%vect

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
