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
integer :: if                   ! face index
integer :: buf, dimbuf, dimbuf1 ! buffer size (current, regular and first)
integer :: ib, nblock           ! block index and number of blocks
integer :: ista, iend           ! starting and ending index
integer :: it                   ! index de tableau
integer :: icl, icr             ! index de cellule a gauche et a droite
type(st_genericfield) :: cell_l, cell_r       ! buffer sized, state extrapolation at face
type(st_genericfield) :: gradL, gradR         ! nblock size arrays of gradients
type(v3d), dimension(:), allocatable &
                      :: cg_l, cg_r           ! tableau des centres de cellules a gauche et a droite   

! -- Body --

call calc_buffer(umesh%nface, cell_buffer, nblock, dimbuf, dimbuf1)

call new(cell_l, umesh%nface, field%etatprim%nscal, field%etatprim%nvect, field%etatprim%ntens)
call new(cell_r, umesh%nface, field%etatprim%nscal, field%etatprim%nvect, field%etatprim%ntens)

allocate(  cg_l(dimbuf),   cg_r(dimbuf))
call new(gradL, dimbuf, field%gradient%nscal, field%gradient%nvect, field%gradient%ntens)
call new(gradR, dimbuf, field%gradient%nscal, field%gradient%nvect, field%gradient%ntens)

!!$! Limit gradient using minmax limiter
!!$! All other limiters are applied below.
!!$select case(defspat%muscl%limiter)
!!$case(lim_minmax)
!!$  call minmax_limiter(defspat, umesh, field%etatprim, field%gradient, cell_l, cell_r)
!!$end select

ista = 1
buf  = dimbuf1

do ib = 1, nblock

  iend = ista+buf-1

  select case(defspat%method)

  case(hres_none)
    
    ! -- no extrapolation, only direct copy of cell values --

    call distrib_field(field%etatprim, umesh%facecell, ista, iend, &
                       cell_l, cell_r, ista)
  
 
  !----------------------------------------------------------------------
  ! HIGH ORDER states interpolation
  !----------------------------------------------------------------------
  case(hres_muscl)

    call hres_ns_muscl(defspat, buf, ista, umesh,      &
                       field%etatprim, field%gradient,   &
                       cell_l, cell_r, ista)

  case(hres_musclfast)

    call hres_ns_musclfast(defspat, buf, ista, umesh,      &
                           field%etatprim, field%gradient,   &
                           cell_l, cell_r, ista)

  case(hres_muscluns)

    call hres_ns_muscluns(defspat, buf, ista, umesh,      &
                          field%etatprim, field%gradient,   &
                          cell_l, cell_r, ista)

  case(hres_svm)

    call hres_ns_svm(defspat, buf, ista, umesh, field%etatprim, &
                     cell_l, cell_r, ista)

  case default
    call erreur("flux computation","unknown high resolution method")
  endselect


  !----------------------------------------------------------------------
  ! end of nblock

  ista = ista + buf
  buf  = dimbuf         ! tous les nblocks suivants sont de taille dimbuf
  
enddo

!----------------------------------------------------------------------
! POST-LIMITATION
!----------------------------------------------------------------------

select case(defspat%postlimiter)
case(postlim_none)
  ! NOTHING TO DO

case(postlim_barth)
  call postlimit_barth(defspat, umesh, field%etatprim, cell_l, cell_r)

case(postlim_monotonic0, postlim_monotonic1, postlim_monotonic2)
  call postlimit_monotonic(defspat, umesh, field%etatprim, cell_l, cell_r)

case default
  call erreur("flux computation","unknown POST-LIMITATION method")
endselect


ista = 1
buf  = dimbuf1

do ib = 1, nblock

  iend = ista+buf-1

  !----------------------------------------------------------------------
  ! computation of INVISCID fluxes
  !----------------------------------------------------------------------

  call calc_flux_inviscid(defsolver, defspat,                             &
                          buf, ista, umesh%mesh%iface(ista:iend, 1, 1), &
                          cell_l, cell_r, flux, calc_jac, jacL, jacR)

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
                           cg_l, cg_r,                                &
                           cell_l, cell_r, gradL, gradR, flux,        &
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

!-------------------------------------------------------------
! flux assignment or modification on boundary conditions

call ns_bocoflux(defsolver, umesh, flux, field, defspat)

call delete(cell_l)
call delete(cell_r)
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
!------------------------------------------------------------------------------!
