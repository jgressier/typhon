!------------------------------------------------------------------------------!
! Procedure : importtyphon_mesh
!
! Fonction: MESH reading
!
!------------------------------------------------------------------------------!
subroutine importtyphon_mesh(zone)

use TYPHMAKE
use OUTPUT
use VARCOM
use DEFZONE
use XBIN_IO
use TYPHON_FMT
use MENU_GEN

implicit none

! -- OUTPUTS --
type(st_zone) :: zone

! -- Internal variables --
integer                       :: iunit
type(st_defxbin)              :: defxbin
type(st_xbindatasection)      :: xbindata

! -- BODY --

!------------------------------
! open xbin file

iunit = getnew_io_unit()
call xbin_openread(iunit, trim(zone%defsolver%defmesh%filename), defxbin)

call xbin_readdatahead(defxbin, xbindata)
call xbin_skipdata(defxbin, xbindata)

stop

!!$!------------------------------------------------------------------------
!!$! read and convert CGNS mesh to typhon zone
!!$!------------------------------------------------------------------------
!!$
!!$call 
!!$call readcgnsfile(zone%defsolver%defmesh, cgnsworld)
!!$call print_info(2, "* CGNS -> TYPHON CONVERSION")
!!$if (cgnsworld%nbase /= 1) call error_stop("CGNS -> TYPHON: too many CGNS bases in structure CGNS")
!!$
!!$! -- Definition minimale du maillage --
!!$!  coordonnees de sommets
!!$!  connectivites face->cellules
!!$!  connectivites face->sommets
!!$call cgns2typhon_zone(zone%defsolver, zone%defsolver%defmesh, cgnsworld%base(1), zone)
!!$
!!$call delete_cgns_world(cgnsworld)

endsubroutine importtyphon_mesh
!------------------------------------------------------------------------------!
! Change history
!
! June 2010: creation (read TYPHON internal format mesh file)
!------------------------------------------------------------------------------!
