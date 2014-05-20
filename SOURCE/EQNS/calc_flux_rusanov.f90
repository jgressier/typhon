!------------------------------------------------------------------------------!
! Procedure : calc_flux_rusanov                 Authors : J. Gressier
!
!> @brief Computation of RUSANOV flux for Euler equations
!------------------------------------------------------------------------------!
subroutine calc_flux_rusanov(defsolver, defspat, nflux, fn, &
                          cell_l, cell_r, flux, ideb,      &
                          calc_jac, jacL, jacR)
use TYPHMAKE
use OUTPUT
use MENU_SOLVER
use MENU_NUM
use MESHBASE
use DEFFIELD
use EQNS
use GEO3D
use MATRIX_ARRAY

implicit none

! -- Inputs --
type(mnu_solver)      :: defsolver        ! solver parameters
type(mnu_spat)        :: defspat          ! space integration parameters
integer               :: nflux            ! number of fluxes
integer               :: ideb             ! index of first flux
type(v3d)             :: fn(1:nflux)      ! face normals
type(st_nsetat)       :: cell_l, cell_r   ! primitive variables array
logical               :: calc_jac         ! jacobian calculation boolean

! -- Inputs/Outputs --

! -- Outputs --
type(st_genericfield) :: flux
type(st_mattab)       :: jacL, jacR       ! flux jacobian matrices

! -- Internal variables --
integer                     :: if
real(krp), dimension(nflux) :: ray, vnl, vnr
real(krp)                   :: g, ig1, al, ar, rel, rer

! -- Body --

g   = defsolver%defns%properties(1)%gamma
ig1 = 1._krp/(g - 1._krp)

! -- Pre-processing --

!-------------------------------
! Flux computation

vnl(1:nflux) = cell_l%velocity(1:nflux).scal.fn(1:nflux)       ! face normal velocity (left  state)
vnr(1:nflux) = cell_r%velocity(1:nflux).scal.fn(1:nflux)       !                      (right state)

do if = 1, nflux

  al  = sqrt(g*cell_l%pressure(if)/cell_l%density(if)) ! sound speed          (left state)
  ar  = sqrt(g*cell_r%pressure(if)/cell_r%density(if)) !                      (right state)
  ray(if) = max(abs(vnl(if))+al, abs(vnr(if))+ar )

  ! volumic total energy (left and right)
  rel = ig1*cell_l%pressure(if) + .5_krp*cell_l%density(if)*sqrabs(cell_l%velocity(if))
  rer = ig1*cell_r%pressure(if) + .5_krp*cell_r%density(if)*sqrabs(cell_r%velocity(if))

  ! mass flux
  flux%tabscal(1)%scal(ideb-1+if) = .5_krp*( (vnl(if)+ray(if))*cell_l%density(if) + (vnr(if)-ray(if))*cell_r%density(if) )
  ! energy flux
  flux%tabscal(2)%scal(ideb-1+if) = .5_krp*( (vnl(if)+ray(if))*rel + (vnr(if)-ray(if))*rer &
                                  + vnl(if)*cell_l%pressure(if) + vnr(if)*cell_r%pressure(if) )
  ! momentum flux
  flux%tabvect(1)%vect(ideb-1+if) = .5_krp*( ((vnl(if)+ray(if))*cell_l%density(if))*cell_l%velocity(if) &
                                  + ((vnr(if)-ray(if))*cell_r%density(if))*cell_r%velocity(if) &
                                  + (cell_l%pressure(if) + cell_r%pressure(if))*fn(if) )
enddo

!--------------------------------------------------------------
! Jacobian calculation
!--------------------------------------------------------------
if (calc_jac) then

  select case(defspat%jac_hyp)
  case(jac_efm)
    call erreur("Development", "EFM jacobian matrices not available with RUSANOV flux")
    !call calc_jac_eqns(defsolver, defspat, nflux, face,        &
    !                   cell_l, cell_r, ideb, jacL, jacR))
  case(jac_rusanov)
    call calc_jac_rusanov(defsolver, defspat, nflux, fn,          &
                      cell_l, cell_r, ray, vnl, vnr, ideb, jacL, jacR)
  case(jac_hll)
    call error_stop("Development: HLL jacobian matrices not available with RUSANOV flux")
  case(jac_hlldiag)
    call error_stop("Development: HLL diagonal jacobian matrices not available with RUSANOV flux")
  case default
    call error_stop("Internal error: unknown jacobian expression for Euler hyperbolic fluxes")
  endselect

endif


endsubroutine calc_flux_rusanov

!------------------------------------------------------------------------------!
! Changes history
!
! Apr 2008 : creation, RUSANOV flux
!------------------------------------------------------------------------------!
