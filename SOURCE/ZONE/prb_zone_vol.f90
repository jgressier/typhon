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

!call exchange_zonal_timestep(lzone, dt)
if (mpi_run) call error_stop("Internal limitation: cannot use PROBE in parallel computations")

endsubroutine prb_zone_vol
!------------------------------------------------------------------------------!
! change history
!
! June 2009: created
!------------------------------------------------------------------------------!
