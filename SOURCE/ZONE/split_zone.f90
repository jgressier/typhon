!------------------------------------------------------------------------------!
!> @ingroup mpi
!> @brief split grid list from a zone and distribute to other proc
!------------------------------------------------------------------------------!
subroutine split_zone(zone)

use OUTPUT
use VARCOM
use DEFZONE
use MODWORLD
use MGRID
use MESHPART

implicit none

! -- INPUTS/OUTPUTS --
type(st_zone) :: zone

! -- Internal variables --
integer(kip)              :: npart, ipart    ! number of part (and index)
integer(kip)              :: npartcluster    ! number of part per distribution process
integer(kip)              :: ipart1, ipart2  ! range of parts to distribute
integer(kip)              :: nci             ! number internal cells
type(st_grid)             :: partgrid        ! intermediate grid
type(st_grid), pointer    :: pgrid, next     ! grid pointer
integer(kip), allocatable :: partition(:)    ! result of partition

! -- BODY --

npart        = zone%info%nbproc
npartcluster = zone%defsolver%defmesh%partcluster

!-- loop on all grids --

pgrid => zone%gridlist%first

do while (associated(pgrid))  ! assume a dummy grid has been initialized on each other proc

  !ipart = index_int(myprocid, zone%info%proc(1:npart))   ! 1 to nproc
  !if (ipart == 0) call error_stop("internal error: grid part not associated")

  if (mod(myprocid-1, npartcluster)+1 == 1) then  ! only "grid" master reads the mesh
    ! -------------------------------------------------
    ! compute partition of a grid

    nci = pgrid%umesh%ncell_int
    allocate(partition(nci))

    call print_info(10, "> compute partition: "//trim(strof(npart))//" parts")

    call ustmesh_partition(part_auto, pgrid%umesh, npart, nci, partition)  ! no check that each master computes the same

    ipart1 = (myprocid-1) * npartcluster + 1
    ipart2 = max(myprocid * npartcluster, npart)
    
    do ipart = ipart1, ipart2   ! extract part and send to MPI thread
  
      ! -------------------------------------------------
      ! extract part-grid of a grid & create connectivity
      call print_info(10, "> extract and send part"//trim(strof(ipart))//" over "//trim(strof(npart)))

      call init_grid(partgrid, ipart)
      call extractpart_grid(pgrid, ipart, nci, partition, partgrid)

      if (myprocid == ipart) then  ! do not send
        pgrid = partgrid
      else
        ! send grid to other proc
        !call send_grid(ipart, partgrid)
      endif
      call delete(partgrid)

    enddo ! part distribution

    deallocate(partition)

  else
    !call receive_grid(myprocid, pgrid)  
  endif
  
  call print_info(10, "  recompute COLOR groups")
  call grid_postproc(pgrid)  ! compute volume, ndof and color groups
  pgrid%next => next         ! ensure the memorized link to next grid

  ! -- next grid --

  pgrid => next
enddo

endsubroutine split_zone
!------------------------------------------------------------------------------!
!> Change history
!
!! Mar  2005 : Created
!! Sept 2005 : integrated to main branch (split all grids)
!! Oct  2005 : restructuration
!! May  2014 : grid send and receive to avoid all procs preprocessing whole grid 
!------------------------------------------------------------------------------!