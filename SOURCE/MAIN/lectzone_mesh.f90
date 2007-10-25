!------------------------------------------------------------------------------!
! Procedure : lectzone_mesh               Auteur : J. Gressier
!                                         Date   : Novembre 2002
! Fonction                                Modif  : (cf historique)
!   Lecture des maillages
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine lectzone_mesh(zone)

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
type(st_cgns_world) :: cgnsworld      ! structure des donnees CGNS

! -- BODY --

!------------------------------------------------------------------------
! read and convert mesh to typhon zone
!------------------------------------------------------------------------

select case(zone%defmesh%format)

case(fmt_CGNS) ! Format de fichier CGNS

  ! DEV : numero d'unite inutile
  call readcgnsfile(15, zone%defmesh%fichier, cgnsworld)
  call print_info(2, "* CGNS -> TYPHON CONVERSION")
  if (cgnsworld%nbase /= 1) call erreur("CGNS -> TYPHON",&
                                        "too many CGNS bases in structure CGNS")

  ! -- Definition minimale du maillage --
  !  coordonnees de sommets
  !  connectivites face->cellules
  !  connectivites face->sommets
  call cgns2typhon_zone(zone%defsolver, zone%defmesh, cgnsworld%base(1), zone)
  call delete_cgns_world(cgnsworld)

case(fmt_TYPHMSH) ! Format de fichier CGNS

  call readtyphmshfile(15, zone%defmesh%fichier, zone)

case default
  call erreur("reading mesh","unknown mesh format")
endselect


endsubroutine lectzone_mesh

!------------------------------------------------------------------------------!
! Change history
!
! nov  2002: creation (read CGNS file)
! fev  2004: read TYPHMSH format mesh
!------------------------------------------------------------------------------!
