!------------------------------------------------------------------------------!
! Procedure : calcboco_connect
!                             
! Fonction 
!   Computation & exchange of boundary conditions for connection conditions
!
!------------------------------------------------------------------------------!
subroutine calcboco_connect(imode, defsolver, defspat, umesh, bccon)

use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_SOLVER
use MENU_NUM
use USTMESH
use GRID_CONNECT
use DEFFIELD

implicit none

! -- Inputs --
integer(kpp)           :: imode
type(mnu_solver)       :: defsolver 
type(mnu_spat)         :: defspat
type(st_ustmesh)       :: umesh             ! maillage en entree, champ en sortie

! -- Inputs/Outputs --
type(st_bccon)         :: bccon

! -- Internal variables --
integer :: ib, ir                    ! index de conditions aux limites et de couplage
integer :: idef, nf                  ! index de definitions des conditions aux limites
integer :: nrac                      ! numero de raccord

! ----------------------------------- BODY -----------------------------------

! only compute connections (idef <= 0)

do ib = 1, umesh%nboco

  idef = umesh%boco(ib)%idefboco

  if (idef <= 0) then

    nf       = umesh%boco(ib)%nface
    bccon%nf = nf

    select case(umesh%boco(ib)%gridcon%contype)

    case(gdcon_match)
      ! send and receive data of this grid (from other grids
#ifdef MPICOMPIL
      call calcboco_connect_match(imode, defsolver, umesh, umesh%boco(ib), bccon)
#else /* MPICOMPIL */
      call error_stop("Development: matching connection only implemented with MPI communicator")
#endif /* MPICOMPIL */

    case(gdcon_nomatch)
      call error_stop("Development: non matching connection not implemented")

    case(gdcon_per_match)
      ! compute periodic matching conditions (only available for periodic def. inside a grid)
      call calcboco_connect_per_match(imode, defsolver, umesh, umesh%boco(ib), bccon)

    case(gdcon_per_nomatch)
      call error_stop("Development: periodic non matching connection not implemented")

    case(gdcon_coarse_fine)
      call error_stop("Development: coarse/fine connection not implemented")

    case(gdcon_fine_coarse)
      call error_stop("Development: fine/coarse connection not implemented")

    case default
      call error_stop("Internal error: unknown connection parameter")

    endselect

  deallocate(bccon%isend, bccon%irecv)

  endif
enddo

endsubroutine calcboco_connect

!------------------------------------------------------------------------------!
! Changes history
!
! Oct  2005: Created
! May  2009: integrate loop on boco for one grid dummmy argument
!------------------------------------------------------------------------------!
