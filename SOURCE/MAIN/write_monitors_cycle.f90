!------------------------------------------------------------------------------!
! Procedure : write_monitors_cycle
!
! Fonction
!   Calcul des quantites definies par les write_monitors
!
!------------------------------------------------------------------------------!
subroutine write_monitors_cycle(icycle, zone)

use TYPHMAKE
use OUTPUT
use VARCOM
use DEFZONE
use MENU_GEN
use STRING

implicit none

! -- Inputs --
type(st_zone) :: zone            ! zone
integer       :: icycle

! -- Outputs --

! -- Internal variables --
integer                :: ic                 ! index de capteur
character(len=200)     :: str
type(st_grid), pointer :: pgrid


! -- BODY --

! ----------------------------------------------
! Computation 

str=''

if (zone%defsolver%nprobe >= 1) then

! -- update primitive data --

pgrid => zone%gridlist%first
do while (associated(pgrid))
  call calc_varprim(zone%defsolver, pgrid%info%field_loc)     ! calcul des var. primitives
  pgrid => pgrid%next
enddo

do ic = 1, zone%defsolver%nprobe

  select case(zone%defsolver%probe(ic)%type)
  case(probe_cell)
    call error_stop("Development: type PROBE non implemente")
  case(boco_field)
    call prb_boco_field(zone)
  case(vol_min, vol_max, vol_average)
    call prb_zone_vol(zone, zone%defsolver%probe(ic))
  case(boco_integral)
    call error_stop("Development: type BOCO_INTEGRAL non implemente")
  case(residuals)
    call error_stop("Development: type RESIDUALS non implemente")
  endselect

  ! ----------------------------------------------
  ! write data

  write(zone%defsolver%probe(ic)%unit,'(i5,e16.8)') icycle, zone%defsolver%probe(ic)%result

enddo

endif
!-----------------------------
endsubroutine write_monitors_cycle
!------------------------------------------------------------------------------!
! Changes history
!
! June 2009: extracted from write_monitors
!------------------------------------------------------------------------------!
