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
#ifdef MPICOMPIL
use MPICOMM
#endif

implicit none

! -- INPUTS/OUTPUTS --
type(st_zone) :: zone

! -- Private Data --
integer                :: izone
type(st_grid), pointer :: pgrid

! -- BODY --

!--------------------------------------------------------------------
! grid preprocessing: internal faces ---

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
! Split grids
if (zone%info%nbproc > 1) then
  call print_info(5,"* Splitting grids")
  call split_zone(zone)
endif

!--------------------------------------------------------------------
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
