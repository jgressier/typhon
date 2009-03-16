!------------------------------------------------------------------------------!
! Procedure : writecgns_zone                         Authors : J. Gressier
!  
! Fonction 
!   Write a TYPHON ZONE (structure) to a CGNS BASE
!
!------------------------------------------------------------------------------!
subroutine writecgns_zone(cgnsunit, zone) 

use TYPHMAKE
use OUTPUT
use VARCOM
use DEFZONE
use MGRID

implicit none

include 'cgnslib_f.h'

! -- INPUTS --
integer               :: cgnsunit           ! unit number for cgns file
type(st_zone)         :: zone

! -- OUPUTS --

! -- Internal variables --
integer                :: ibase, izone     ! CGNS base and zone index
integer                :: isize(3,3)       ! tableau d'informations de la zone
integer                :: info
type(st_genericfield)  :: vfield
type(st_grid), pointer :: pgrid

! -- BODY --


pgrid => zone%gridlist%first 
izone = 0

call cg_base_write_f(cgnsunit, zone%name, dimgeo(pgrid%umesh), 3, ibase, info)

if (zone%gridlist%nbgrid /= 1) call erreur("Write CGNS","only one grid allowed")

do while (associated(pgrid))

  izone      = izone + 1
  isize(1,1) = pgrid%umesh%nvtex       ! vertex size 
  isize(1,2) = pgrid%umesh%ncell_int   ! cell size
  isize(1,3) = 0                       ! boundary vertex size (zero if elements not sorted)

  ! -- create CGNS zone --

  call cg_zone_write_f(cgnsunit, ibase, "USTMESH", isize, Unstructured, izone, info)

  ! -- write TYPHON ustmesh to CGNS zone --

  call writecgns_ustmesh(cgnsunit, ibase, izone, pgrid%umesh)

  !call writecgns_cell(cgnsunit, ibase, izone, &
  !                    zone%defsolver, pgrid%info%field_loc)

  pgrid => pgrid%next


enddo



endsubroutine writecgns_zone
!------------------------------------------------------------------------------!
! Changes history
!
! Mar  2009 : created (JG)
!------------------------------------------------------------------------------!
