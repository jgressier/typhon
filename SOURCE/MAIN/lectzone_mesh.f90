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

! -- Declaration des entrées/sorties --
type(st_zone) :: zone

! -- Declaration des entrées --

! -- Declaration des sorties --

! -- Declaration des variables internes --
type(st_cgns_world) :: cgnsworld      ! structure des données CGNS

! -- Debut de la procedure --

select case(zone%defmesh%format)

case(fmt_CGNS) ! Format de fichier CGNS

  ! DEV : numéro d'unité inutile
  call readcgnsfile(15, zone%defmesh%fichier, cgnsworld)
  call print_info(2, "* CONVERSION DES DONNEES CGNS -> TYPHON")
  if (cgnsworld%nbase /= 1) call erreur("CGNS -> TYPHON",&
                                        "trop de bases dans la structure CGNS")

  ! -- Définition minimale du maillage --
  !  coordonnées de sommets
  !  connectivités face->cellules
  !  connectivités face->sommets
  call cgns2typhon_zone(cgnsworld%base(1), zone)
  ! DEV : call delete(cgnsworld)

case(fmt_TYPHMSH) ! Format de fichier CGNS

  call readtyphmshfile(15, zone%defmesh%fichier, zone)
  !call print_info(2, "* C TYPHON")
  !if (cgnsworld%nbase /= 1) call erreur("CGNS -> TYPHON",&
  !                                      "trop de bases dans la structure CGNS")

  !call cgns2typhon_zone(cgnsworld%base(1), zone)

case default
  call erreur("Lecture de maillage","format de maillage inconnu")
endselect


endsubroutine lectzone_mesh

!------------------------------------------------------------------------------!
! Historique des modifications
!
! nov  2002 : création de la procédure (lecture de maillage CGNS)
! fev  2004 : lecture de maillage TYPHMSH 
!------------------------------------------------------------------------------!
