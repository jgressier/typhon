!------------------------------------------------------------------------------!
! Procedure : treelevel_rungekutta  
!                                   
! Fonction                          
!   Time Integration by Runge Kutta routine
!
!------------------------------------------------------------------------------!
subroutine treelevel_rungekutta(dt, info, defsolver, gridlist, coupling, ncoupling)

use TYPHMAKE
use OUTPUT
use VARCOM
use MGRID
use MENU_SOLVER
use MODINFO
use MENU_ZONECOUPLING

implicit none

! -- Inputs --
real(krp)         :: dt              ! timestep for this level
type(st_infozone) :: info            ! zone information structure
type(mnu_solver)  :: defsolver       ! solver parameters
type(st_gridlist) :: gridlist        ! list of grids
integer           :: ncoupling        ! nombre de couplages de la zone

! -- Outputs --
type(mnu_zonecoupling), dimension(1:ncoupling) &
                 :: coupling ! donnees de couplage
! retour des residus a travers le champ field de la structure zone

! -- Internal variables --
type(st_grid), pointer             :: pgrid
integer                            :: istage, nstage, is
integer                            :: igrid,  ngrid
type(st_genericfield), allocatable :: u0(:)       ! initial state (Un) (1:ngrid)
type(st_genericfield), allocatable :: rhs(:,:)    ! RHS(1:nstage, 1:ngrid)

! -- BODY --

ngrid  = gridlist%nbgrid
nstage = defsolver%deftime%rk%nstage

allocate(rhs(1:nstage, 1:ngrid))
allocate( u0(1:ngrid))

!--------------------------------------------
! copy U0 states

pgrid => gridlist%first
do igrid = 1, ngrid
  call new(u0(igrid), pgrid%field%etatcons)
  call transfer_gfield(u0(igrid), pgrid%field%etatcons)
  pgrid => pgrid%next
enddo

!--------------------------------------------
! N stage Runge-Kutta

rkstage: do istage = 1, nstage

  ! --- compute U(istage-1) ---

  if (istage >= 2) then   ! otherwise, etatcons is already defined

    pgrid => gridlist%first
    do igrid = 1, ngrid
      call transfer_gfield(pgrid%field%etatcons, u0(igrid))
      do is = 1, istage-1
        call xeqxpay(pgrid%field%etatcons, defsolver%deftime%rk%coef(istage-1, is), rhs(is, igrid))
      enddo
      pgrid => pgrid%next
    enddo
  endif

  ! --- compute RHS(istage-1) ---

  call calc_rhs(dt, info, defsolver, gridlist, coupling, ncoupling)

  ! --- get RHS(istage-1) ---

  pgrid => gridlist%first

  do igrid = 1, ngrid
    rhs(istage, igrid) = pgrid%field%residu                  ! copy pointers (move allocation)
    call new(pgrid%field%residu, pgrid%field%residu)         ! reallocate residu field
    pgrid => pgrid%next
  enddo

enddo rkstage

! --- redefine generic fields in mgrid structure ---

! --- compute RHS(nstage) ---

pgrid => gridlist%first
do igrid = 1, ngrid
  call transfer_gfield(pgrid%field%residu, rhs(1, igrid))
  call scale(pgrid%field%residu, defsolver%deftime%rk%coef(nstage, 1))
  do is = 2, nstage
    call xeqxpay(pgrid%field%residu, defsolver%deftime%rk%coef(nstage, is), rhs(is, igrid))
  enddo
  pgrid => pgrid%next
enddo

! --- reset U0 ---

pgrid => gridlist%first
do igrid = 1, ngrid
  call transfer_gfield(pgrid%field%etatcons, u0(igrid))
  pgrid => pgrid%next
enddo

! --- delete all temporary fields ---

do igrid = 1, ngrid
  call delete(u0(igrid))
  do istage = 1, nstage
    call delete(rhs(istage, igrid))
  enddo
enddo

deallocate(u0)
deallocate(rhs)

!-----------------------------
endsubroutine treelevel_rungekutta

!------------------------------------------------------------------------------!
! Changes history
!
! Mar  2006: created from integzone_tstep_usttree
! Nov  2007: choice of time integration method
!------------------------------------------------------------------------------!
