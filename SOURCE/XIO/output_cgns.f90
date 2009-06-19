!------------------------------------------------------------------------------!
! Procedure : output_cgns 
!                         
! Function 
!   Write the field of each zone in a CGNS file
!
!------------------------------------------------------------------------------!
 subroutine output_cgns(nom, defio, zone)

use TYPHMAKE
use OUTPUT
use VARCOM
use MODWORLD
use DEFZONE
use MENU_GEN

implicit none

include 'cgnslib_f.h'

! -- INPUTS --
character(len=longname) :: nom       ! filename
type(mnu_output)      :: defio     ! output parameter
type(st_zone)         :: zone      ! zone

! -- OUPUTS --

! -- Internal variables --
integer               :: dim, ufc, ir
integer               :: info
type(st_genericfield) :: vfield
character(len=10)     :: suffix

! -- BODY --

if (mpi_run) then
  suffix = "_p"//strof_full_int(myprocid,3)//".cgns"
else
  suffix = ".cgns"
endif

call cg_open_f(trim(nom)//trim(suffix), MODE_WRITE, ufc, info)

call writecgns_zone(ufc, zone)

call cg_close_f(ufc, info)


endsubroutine output_cgns
!------------------------------------------------------------------------------!
! Changes history
!
! Mar  2009 : created
!------------------------------------------------------------------------------!
