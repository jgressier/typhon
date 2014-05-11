!------------------------------------------------------------------------------!
!> @brief initialization of fields per zone
!> send to specific initialization according to mesh data structure
!> computes references values for normalization (residuals, ...)
!------------------------------------------------------------------------------!
subroutine initzone_field(zone)

use TYPHMAKE
use OUTPUT
use DEFZONE

implicit none

! -- INPUTS --

! -- OUTPUTS --

! -- INPUTS/OUTPUTS --
type(st_zone) :: zone

! -- Internal variables --
type(st_grid), pointer :: pgrid 
integer                :: id  

! -- BODY --

pgrid => zone%gridlist%first

do while (associated(pgrid))
  allocate(pgrid%field)

  call init_gridfield_ust(zone%defsolver, pgrid%umesh, pgrid)

  pgrid%info%field_loc => pgrid%field
  pgrid => pgrid%next
enddo

call calc_refcons(zone)

endsubroutine initzone_field
!------------------------------------------------------------------------------!
! Changes history
!
! mars 2003 : creation de la procedure
! avr  2004 : application a liste chainee de MGRID
! oct  2004 : field chained list
!------------------------------------------------------------------------------!
