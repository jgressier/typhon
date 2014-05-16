!------------------------------------------------------------------------------!
! Procedure : zone_preproc
!
! Fonction
!   Mesh pre-processing
!------------------------------------------------------------------------------!
subroutine zone_preproc(zone)

use TYPHMAKE
use OUTPUT
use DEFZONE
use MESHPARAMS
use MPICOMM

implicit none

! -- INPUTS/OUTPUTS --
type(st_zone) :: zone

! -- Private Data --
integer                :: izone
type(st_grid), pointer :: pgrid

! -- BODY --

!--------------------------------------------------------------------
! grid MPI distribution

select case(zone%defsolver%defmesh%ipart)
case(ipart_file)
  call print_info(5,"* Grid distribution")
  pgrid => zone%gridlist%first
  do while (associated(pgrid))
    !> @dev implement ustmesh direct distribution using partition file
    call error_stop("not yet implemented (zone_preproc)")
    !call grid_distribution(zone%defsolver, pgrid) !?
    pgrid => pgrid%next
  enddo
case(ipart_metis)
  ! nothing to do: partition is not known yet
case default
  call error_stop("internal error: unknown partition method (zone_preproc)")
endselect

!--------------------------------------------------------------------
! grid preprocessing: internal faces ---

call print_info(5,"* Computing internal connectivity")
pgrid => zone%gridlist%first
do while (associated(pgrid))
  call grid_preproc(zone%defsolver, pgrid)
  pgrid => pgrid%next
enddo

!--------------------------------------------------------------------
! Initialisation des connectivites cellules/faces/sommets des conditions aux limites

call print_info(5,"* Computing and Initializing connectivities of boundary conditions")
call init_connect(zone)

!--------------------------------------------------------------------
! Split and distribute grids

#ifdef MPICOMPIL
select case(zone%defsolver%defmesh%ipart)
case(ipart_file)
  ! already done
case(ipart_metis)
if (zone%info%nbproc > 1) then
  call print_info(5,"* Splitting grids")
  call split_zone(zone)
endif
case default
  call error_stop("internal error: unknown partition method (zone_preproc)")
endselect
#endif /* MPICOMPIL */

!--------------------------------------------------------------------
! sum volume and ndof on grids over all mpi threads

zone%info%totvolume = 0._krp
zone%info%totndof   = 0

pgrid => zone%gridlist%first
do while (associated(pgrid))
  zone%info%totvolume = zone%info%totvolume + pgrid%info%volume
  zone%info%totndof   = zone%info%totndof   + pgrid%info%ndof
  pgrid => pgrid%next
enddo

! -- reduce (sum) on all threads
#ifdef MPICOMPIL
call allreduce_sum_real(zone%info%totvolume)
call allreduce_sum_int(zone%info%totndof)
#endif /*MPICOMPIL*/

endsubroutine zone_preproc
!------------------------------------------------------------------------------!
! Change history
!
! Nov  2002: creation
! June 2010: lecture_maillage.f90 -> readallmesh.f90, add internal TYPHON format
! Dec  2010: from readallmesh, all mesh pre-processing
! Apr  2011: loop on grids and split to grid_preproc
! Feb  2013: moved (called by) to init_world
! Jan  2014: reorder preprocessing, include metis split
!------------------------------------------------------------------------------!
