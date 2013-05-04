!------------------------------------------------------------------------------!
! Procedure : calc_refcons 
! 
! Fonction
!   Computation of average conservative values
!
!------------------------------------------------------------------------------!
subroutine calc_refcons(zone)

use VARCOM
use OUTPUT
use PACKET
use DEFZONE
!$ use OMP_LIB

implicit none

! -- INPUTS/OUTPUTS --
type(st_zone)   :: zone

! -- Private Data --
type(st_grid),         pointer :: pgrid 
type(st_genericfield), pointer :: pfield
integer                        :: i, nsca, nvec, ncell, ithread
integer                        :: ib, nblock, buf            ! block index and number of blocks
integer, pointer               :: ista(:), iend(:)           ! starting and ending index
real(krp), allocatable         :: refsca(:,:), refvec(:,:)

! -- BODY --

!---------------------------------------------------
! initialization

nsca = zone%defsolver%nsca
nvec = zone%defsolver%nvec

if (nsca /= 0) then
  allocate(refsca(nsca, nthread))
  refsca (1:nsca, 1:nthread) = 0._krp
endif
if (nvec /= 0) then
  allocate(refvec(nvec, nthread))
  refvec (1:nvec, 1:nthread) = 0._krp
endif

!---------------------------------------------------
! sum initial conservative variable with loop on grids

pgrid => zone%gridlist%first

do while (associated(pgrid))

  ncell = pgrid%umesh%ncell_int                             ! only use internal cells
  call new_buf_index(ncell, cell_buffer, nblock, ista, iend, nthread)

  pfield => pgrid%info%field_loc%etatcons

  !$OMP PARALLEL & 
  !$OMP   private(ib, i, ithread) &
  !$OMP   shared (nsca, nvec, pfield, pgrid, refsca, refvec)
  ithread = 1
  !$ ithread = OMP_GET_THREAD_NUM()+1

  !$OMP DO
  do ib = 1, nblock

    do i = 1, nsca
      refsca(i, ithread) = refsca(i, ithread) + &
                           sum(pfield%tabscal(i)%scal(ista(ib):iend(ib))*pgrid%umesh%mesh%volume(ista(ib):iend(ib), 1, 1))
    enddo
    do i = 1, nvec
      refvec(i, ithread) = refvec(i, ithread) + &
                           sum(abs(pfield%tabvect(i)%vect(ista(ib):iend(ib)))*pgrid%umesh%mesh%volume(ista(ib):iend(ib), 1, 1))
    enddo

  enddo ! blocks
  !$OMP END PARALLEL
  deallocate(ista, iend)
  
  pgrid => pgrid%next

enddo ! loop on grids

!---------------------------------------------------
! sum on all threads and procs and divide by tot volume

if (nsca /= 0) then
  do i = 1, nsca
    zone%defsolver%refsca(i) = sum(refsca(i, 1:nthread))
    call allreduce_sum(zone%defsolver%refsca(i))
  enddo
  zone%defsolver%refsca(1:nsca) = zone%defsolver%refsca(1:nsca) / zone%info%totvolume
  deallocate(refsca)
endif

if (nvec /= 0) then
  do i = 1, nvec
    zone%defsolver%refvec(i) = sum(refvec(i, 1:nthread))
    call allreduce_sum(zone%defsolver%refvec(i))
  enddo
  zone%defsolver%refvec(1:nvec) = zone%defsolver%refvec(1:nvec) / zone%info%totvolume
  deallocate(refvec)
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
