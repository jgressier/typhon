!------------------------------------------------------------------------------!
! Procedure : importcgns_mesh
!
! Fonction: MESH reading
!
!------------------------------------------------------------------------------!
subroutine importcgns_mesh(zone)

use TYPHMAKE
use OUTPUT
use VARCOM
use DEFZONE
use CGNS_STRUCT
use MENU_GEN

implicit none

! -- OUTPUTS --
type(st_zone) :: zone

! -- Internal variables --
type(st_cgns_zone)     :: cgnszone      ! structure des donnees CGNS
type(st_grid), pointer :: pgrid
integer                :: unit, ier, i

! -- BODY --

!------------------------------------------------------------------------
! read CGNS mesh
!------------------------------------------------------------------------

call print_info(5, "* READING CGNS MESH: "//trim(zone%defsolver%defmesh%filename))

! --- open CGNS file ---
call cg_open_f(trim(zone%defsolver%defmesh%filename), MODE_READ, unit, ier)

if (ier /= 0) call erreur("CGNS IO","cannot open CGNS file "//trim(zone%defsolver%defmesh%filename))
   
call readcgnszone(unit, zone%defsolver%defmesh%icgnsbase, &
                        zone%defsolver%defmesh%icgnszone, cgnszone)

! --- CGNS close ---
call cg_close_f(unit, ier)

!------------------------------------------------------------------------
! convert CGNS mesh to typhon zone
!------------------------------------------------------------------------

call print_info(2, "* CGNS -> TYPHON CONVERSION")

! -- Definition minimale du maillage --
!  coordonnees de sommets
!  connectivites face->cellules
!  connectivites face->sommets

pgrid => add_grid(zone%gridlist)
call cgns2typhon_ustmesh(zone%defsolver%defmesh, cgnszone, pgrid%umesh)

call delete_cgns_zone(cgnszone)

endsubroutine importcgns_mesh
!------------------------------------------------------------------------------!
! Change history
!
! nov  2002: creation (read CGNS file)
! fev  2004: read TYPHMSH format mesh
! June 2010: lectzone_mesh.f90->importcgns_mesh.f90
! Dec  2010: inline readcgnsfile, simplify to unique base reading
!------------------------------------------------------------------------------!
