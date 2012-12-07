!------------------------------------------------------------------------------!
! Procedure : update_field
!
! Fonction
!   Update conservative variables with residual fiels
!   (compute residuals)
!
!------------------------------------------------------------------------------!
subroutine update_field(info, defsolver, gridlist)

use TYPHMAKE
use MODINFO
use VARCOM
use OUTPUT
use MENU_SOLVER
use MENU_GEN
use MGRID

implicit none

! -- INPUTS --
type(st_infozone) :: info            ! champ d'etat et de gradients
type(mnu_solver)  :: defsolver       ! solver parameters
type(st_gridlist) :: gridlist        ! list of grids

! -- Private Data --
type(st_field), pointer :: pfield
type(st_grid),  pointer :: pgrid
integer                 :: nc, ip, ic

! -- BODY --

pgrid => gridlist%first
do while (associated(pgrid))

  pfield => pgrid%info%field_loc

  call xeqxpy(pfield%etatcons, pfield%residu)

  ! -- Compute residuals if steady --

  if (info%time_model == time_steady) then

    nc           = pgrid%umesh%ncell_int
    info%cur_res = 0._krp

    !! DEV: put in DEFFIELD and OMP parallelize
    
    do ip = 1, pfield%nscal
      info%cur_res = info%cur_res + sum(abs(pfield%residu%tabscal(ip)%scal(1:nc))) &
                                    / defsolver%refsca(ip)
    enddo

    do ip = 1, pfield%nvect
      do ic = 1, nc
        ! ATTENTION : le residu est calcule sur la norme du vecteur, non ses composantes
        info%cur_res = info%cur_res + abs(pfield%residu%tabvect(ip)%vect(ic)) &
                                      / defsolver%refvec(ip)
      enddo
    enddo

    ! -- merge residual for all procs --

    call exchange_zonal_residual(info)

  endif

  pgrid => pgrid%next
enddo

endsubroutine update_field

!------------------------------------------------------------------------------!
! Changes history
!
! may  2003 : created
! sept 2003 : Residual computation
! oct  2005 : merge residual for all procs
! June 2009 : loop on grids, normalize residuals
!------------------------------------------------------------------------------!
