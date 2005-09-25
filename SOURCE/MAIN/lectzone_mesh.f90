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

implicit none

! -- Declaration des entrees/sorties --
type(st_zone) :: zone

! -- Declaration des entrees --

! -- Declaration des sorties --

! -- Declaration des variables internes --
type(st_cgns_world) :: cgnsworld      ! structure des donnees CGNS

! -- Debut de la procedure --

select case(zone%defmesh%format)

case(fmt_CGNS) ! Format de fichier CGNS

  ! DEV : numero d'unite inutile
  call readcgnsfile(15, zone%defmesh%fichier, cgnsworld)
  call print_info(2, "* CONVERSION DES DONNEES CGNS -> TYPHON")
  if (cgnsworld%nbase /= 1) call erreur("CGNS -> TYPHON",&
                                        "trop de bases dans la structure CGNS")

  ! -- Definition minimale du maillage --
  !  coordonnees de sommets
  !  connectivites face->cellules
  !  connectivites face->sommets
  call cgns2typhon_zone(cgnsworld%base(1), zone)
  ! DEV : call delete(cgnsworld)

case(fmt_TYPHMSH) ! Format de fichier CGNS

  call readtyphmshfile(15, zone%defmesh%fichier, zone)

case default
  call erreur("Lecture de maillage","format de maillage inconnu")
endselect


endsubroutine lectzone_mesh

!------------------------------------------------------------------------------!
! Historique des modifications
!
! nov  2002 : creation de la procedure (lecture de maillage CGNS)
! fev  2004 : lecture de maillage TYPHMSH 
!------------------------------------------------------------------------------!
