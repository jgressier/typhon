!------------------------------------------------------------------------------!
! Procedure : calc_refcons 
! 
! Fonction
!   Computation of average conservative values
!
!------------------------------------------------------------------------------!
subroutine calc_refcons(zone)

use OUTPUT
use PACKET
use DEFZONE

implicit none

! -- INPUTS/OUTPUTS --
type(st_zone)   :: zone

! -- Private Data --
type(st_grid),         pointer :: pgrid 
type(st_genericfield), pointer :: pfield
integer                        :: i, nsca, nvec, ncell, nb, maxbuf, buf, ifirst, iend, ib

! -- BODY --

!---------------------------------------------------
! initialization

nsca = zone%defsolver%nsca
nvec = zone%defsolver%nvec

if (nsca /= 0) zone%defsolver%refsca(1:nsca) = 0._krp
if (nvec /= 0) zone%defsolver%refvec(1:nvec) = 0._krp

!---------------------------------------------------
! sum initial conservative variable with loop on grids

pgrid => zone%gridlist%first

do while (associated(pgrid))

  ncell = pgrid%umesh%ncell_int                             ! only use internal cells
  call calc_buffer(ncell, cell_buffer, nb, maxbuf, buf)     ! block by block computation
  ifirst = 1 

  pfield => pgrid%info%field_loc%etatcons

  do ib = 1, nb
  
    iend = ifirst+buf-1

    do i = 1, nsca
      zone%defsolver%refsca(i) = zone%defsolver%refsca(i) + &
                                 sum(pfield%tabscal(i)%scal(ifirst:iend)*pgrid%umesh%mesh%volume(ifirst:iend, 1, 1))
    enddo

    do i = 1, nvec
      zone%defsolver%refvec(i) = zone%defsolver%refvec(i) + &
                                 sum(abs(pfield%tabvect(i)%vect(ifirst:iend))*pgrid%umesh%mesh%volume(ifirst:iend, 1, 1))
    enddo

    ifirst = ifirst + buf
    buf    = maxbuf
  enddo ! blocks
  
  pgrid => pgrid%next

enddo ! loop on grids

!---------------------------------------------------
! sum on all threads and divide by tot volume

if (nsca /= 0) then
  do i = 1, nsca
    call allreduce_sum(zone%defsolver%refsca(i))
  enddo
  zone%defsolver%refsca(1:nsca) = zone%defsolver%refsca(1:nsca) / zone%info%totvolume
endif

if (nvec /= 0) then
  do i = 1, nvec
    call allreduce_sum(zone%defsolver%refvec(i))
  enddo
  zone%defsolver%refvec(1:nvec) = zone%defsolver%refvec(1:nvec) / zone%info%totvolume
endif

!---------------------------------------------------
! Specific computation of REF VALUES according to solver

call define_refcons(zone%defsolver)


!-----------------------------
endsubroutine calc_refcons

!------------------------------------------------------------------------------!
! Changes history
!
! June 2009: creation
!------------------------------------------------------------------------------!
