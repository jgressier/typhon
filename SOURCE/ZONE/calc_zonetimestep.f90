!------------------------------------------------------------------------------!
! Procedure : calc_zonetimestep           Auteur : J. Gressier
!                                         Date   : Septembre 2003
! Fonction 
!   Calcul du pas de temps local et global par zone selon solveur
!
! Defauts/Limitations/Divers :
!   ATTENTION : a ce moment, les variables primitives ne sont pas calculees
!------------------------------------------------------------------------------!
subroutine calc_zonetimestep(lzone, dt, wres_ref, wcur_res, dtmax)

use TYPHMAKE
use OUTPUT
use VARCOM
use MODWORLD
use MGRID
use MENU_NUM

implicit none

! -- INPUTS --
type(st_zone) :: lzone
real(krp)     :: wres_ref ! world reference residual
real(krp)     :: wcur_res ! world current residual
real(krp)     :: dtmax

! -- OUTPUTS --
real(krp)      :: dt

! -- Internal variables --
type(st_grid), pointer               :: pgrid     ! pointeur sur grille
real(krp), dimension(:), allocatable :: dtloc     ! tableau de pas de temps local
real(krp)                            :: cfl
integer                              :: ncell, nc ! cell number, cell counter

! -- BODY --

! -----------------------------------------------------------------
! COMPUTATION of LOCAL TIMESTEPS for all ZONES
! -----------------------------------------------------------------

select case(lzone%defsolver%typ_solver)

!--------------------------------------------------------
! methode VOLUMES FINIS
!--------------------------------------------------------
case(solKDIF, solNS)

  pgrid => lzone%gridlist%first  !!! DEV !!! should handle tree structure of grids
  dt    =  huge(dt)

  do while (associated(pgrid))   ! BOUCLE sur les grilles (liste chainee)

    ncell = pgrid%umesh%ncell_int
    if (.not.associated(pgrid%dtloc)) allocate(pgrid%dtloc(pgrid%umesh%ncell))

    select case(lzone%defsolver%deftime%stab_meth)

    !--------------------------------------------------------
    case(given_dt)   ! -- Pas de temps impose --
      pgrid%dtloc(1:ncell) = lzone%defsolver%deftime%dt

    !--------------------------------------------------------
    case(stab_cond, loc_stab_cond)  ! -- Calcul par condition de stabilite (deftim%stabnb) --
      select case(lzone%defsolver%typ_solver)
      case(solNS)
        select case(lzone%defsolver%deftime%time_model)
        case(time_steady)
          cfl = lzone%defsolver%deftime%stabnb*max(1._krp, 1._krp-log10(lzone%info%cur_res/ &
                                      lzone%info%residu_ref)- &
                                      log10(wcur_res/ wres_ref) )
          cfl = min(cfl, lzone%defsolver%deftime%stabnb_max)
        case(time_unsteady)
          cfl = lzone%defsolver%deftime%stabnb
        case default
          call error_stop("internal error (calc_zonetimestep): unknown time model")
        endselect
        call calc_ns_timestep(cfl, lzone%defsolver%defns%properties(1), &
                              pgrid%umesh, pgrid%info%field_loc, pgrid%dtloc(1:ncell), ncell)
      case(solKDIF)
        call calc_kdif_timestep(lzone%defsolver%deftime, lzone%defsolver%defkdif%materiau, &
                                pgrid%umesh, pgrid%info%field_loc, pgrid%dtloc(1:ncell), ncell)
      case default
        call error_stop("internal error (calc_zonetimestep): unknown solver")
      endselect

    case default
      call error_stop("internal error (calc_zonetimestep): unknown timestep method")
    endselect  

    ! -- need to compute minimum time step for all grids (only if global time step) --
    dt = min(dt, minval(pgrid%dtloc(1:ncell)))  ! (!) only for internal cells

    ! -- check time steps are not NAN numbers --
    !pgrid%dtloc(1:ncell) = sqrt(-pgrid%dtloc(1:ncell))
    !print*,sum(pgrid%dtloc(1:ncell))
    nc = count(pgrid%dtloc(1:ncell) /= pgrid%dtloc(1:ncell))
    if (nc > 0) call erreur("time step computation", &
                            "found some NAN timesteps ("//trim(strof(nc))//" cells)")

    ! next grid
    pgrid => pgrid%next

  enddo

  ! -- MPI comparison of minimum timesteps

  call exchange_zonal_timestep(lzone, dt)

  dt = min(dt, dtmax)

  ! -- if global time stepping: reset dtloc to minimum time step --

  if (lzone%defsolver%deftime%stab_meth == stab_cond) then
    pgrid => lzone%gridlist%first  !!! DEV !!! should handle tree structure
    do while (associated(pgrid))   ! BOUCLE sur les grilles (liste chainee)
      pgrid%dtloc(1:ncell) = dt
      pgrid => pgrid%next          ! next grid
    enddo
  endif

!--------------------------------------------------------
case default
  call error_stop("internal error: unknown solver (calc_zonetimestep)")
endselect


endsubroutine calc_zonetimestep
!------------------------------------------------------------------------------!
! change history
!
! sept 2003 : creation, appel des procedures specifiques aux solveurs
! mars 2003 : calcul de pas de temps pour methodes lagrangiennes
! avr  2004 : calcul KDIF sur liste chainee de grilles
! july 2004 : NS solver call
! oct  2004 : field chained list
! sept 2005 : local time stepping
! oct  2005 : bound local time step to dtmax only if global time step
! nov  2005 : merge minimal time step with other procs
!------------------------------------------------------------------------------!
