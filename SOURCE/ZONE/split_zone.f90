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
integer(kip)           :: npart, ipart
type(st_grid)          :: partgrid    ! intermediate grid
type(st_grid), pointer :: pgrid       ! grid pointer

! -- BODY --

npart = zone%info%nbproc

!-- loop on all grids --

pgrid => zone%grid

do while (associated(pgrid))

  ipart = index_int(myprocid, zone%info%proc(1:npart))

  call getpart_grid(pgrid, ipart, npart, partgrid)

  ! -- next grid --
  pgrid => pgrid%next
enddo


endsubroutine split_zone

!------------------------------------------------------------------------------!
! Change history
!
! Mar  2005 : Created
! Sept 2005 : integrated to main branch (split all grids)
!------------------------------------------------------------------------------!
