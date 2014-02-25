!------------------------------------------------------------------------------!
! Procedure : prb_zone_vol   
!
! Fonction 
!   Computation of volumic probe
!
!------------------------------------------------------------------------------!
subroutine prb_zone_vol(curtime, zone, probe)

use TYPHMAKE
use OUTPUT
use VARCOM
use DEFZONE
use DEFPROBE
use PROBECALC
#ifdef MPICOMPIL
use MPICOMM
#endif

implicit none

! -- INPUTS --
real(krp)     :: curtime
type(st_zone) :: zone

! -- OUTPUTS --
type(st_defprobe) :: probe

! -- Internal variables --
type(st_grid), pointer  :: pgrid     ! grid pointer

! -- BODY --

call prb_vol_init(probe)

pgrid => zone%gridlist%first  

do while (associated(pgrid)) 
  call prb_vol_calc(curtime, probe, pgrid%umesh, pgrid%info%field_loc%etatprim)
  pgrid => pgrid%next          ! next grid
enddo
  
! -- MPI reduce --

#ifdef MPICOMPIL
select case(probe%type)
case(vol_min)
  call allreduce_min_real(probe%result)
case(vol_max)
  call allreduce_max_real(probe%result)
case(vol_average)
  print*,'vol',myprocid,probe%volume, probe%result
  call allreduce_volavg(probe%volume, probe%result)
case default
  call error_stop("Internal error (prb_zone_vol): unknown probe type")
endselect
#endif /*MPICOMPIL*/

endsubroutine prb_zone_vol
!------------------------------------------------------------------------------!
! change history
!
! June 2009: created
!------------------------------------------------------------------------------!
