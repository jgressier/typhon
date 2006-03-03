!------------------------------------------------------------------------------!
! Procedure : split_zone                   Authors : J. Rodriguez, J. Gressier
!                                          Created : March 2005
! Fonction     
!   Split grids into a zone
!
!------------------------------------------------------------------------------!
subroutine split_zone(zone)

use TYPHMAKE
use OUTPUT
use VARCOM
use DEFZONE
use MODWORLD
use MGRID

implicit none

! -- INPUTS/OUTPUTS --
type(st_zone) :: zone

! -- Internal variables --
integer(kip)              :: npart, ipart  ! number of part (and index)
integer(kip)              :: nci           ! number internal cells
type(st_grid)             :: partgrid      ! intermediate grid
type(st_grid), pointer    :: pgrid, next   ! grid pointer
integer(kip), allocatable :: partition(:)  ! result of partition

! -- BODY --

npart = zone%info%nbproc

!-- loop on all grids --

pgrid => zone%grid

do while (associated(pgrid))

  ipart = index_int(myprocid, zone%info%proc(1:npart))   ! 1 to nproc
  if (ipart == 0) call erreur("developement", "not concerned by computation")

  ! -------------------------------------------------
  ! compute partition of a grid

  nci = pgrid%umesh%ncell_int
  allocate(partition(nci))

  print*,"> compute partition: ",npart," parts"
  call getpart_grid(pgrid, npart, nci, partition)

  ! -------------------------------------------------
  ! extract partition of a grid & create connectivity

  print*,"> extract part", ipart," over ",npart

  call new(partgrid, ipart)
  call extractpart_grid(pgrid, ipart, nci, partition, partgrid)

  ! -------------------------------------------------
  ! replace original grid by partitioned grid 

  next => pgrid%next
  print*,"a"
  call delete(pgrid)
  print*,"b"
  pgrid = partgrid
  print*,"c"
  pgrid%next => next

  print*,"  done"

  ! -- next grid --

  deallocate(partition)
  pgrid => next
enddo


endsubroutine split_zone

!------------------------------------------------------------------------------!
! Change history
!
! Mar  2005 : Created
! Sept 2005 : integrated to main branch (split all grids)
! Oct  2005 : restructuration
!------------------------------------------------------------------------------!
