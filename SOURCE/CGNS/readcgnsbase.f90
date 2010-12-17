!------------------------------------------------------------------------------!
! Procedure : readcgnsbase                Auteur : J. Gressier
!                                         Date   : Novembre 2002
! Fonction                                Modif  :
!   Lecture d'une base d'un fichier CGNS
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

subroutine readcgnsbase(unit, ib, base, iz) 

use CGNS_STRUCT   ! Definition des structures CGNS
use OUTPUT        ! Sorties standard TYPHON

implicit none 

! -- Entrees --
integer             :: unit       ! numero d'unite pour la lecture
integer             :: ib         ! numero de base
integer       :: iz               ! indice courant de zone

! -- Sorties --
type(st_cgns_base)  :: base       ! structure CGNS : base

! -- Variables internes --
integer       :: ier              ! code d'erreur
character(len=7), dimension(2:3), parameter &
              :: type_maillage = (/ "surface", "volumic" /)

! -- Debut de procedure
   
! --- Lecture des infos de la base ---

call cg_base_read_f(unit, ib, base%nom, base%imesh, base%igeo, ier)

!! BUG : test desactive car ier /= 0 meme si tout est correct
!if (ier /= 0) call erreur("Lecture CGNS","Probleme a la lecture de la base")

call print_info(5,"- BASE "//trim(base%nom)//" : "//type_maillage(base%imesh)//" mesh")
   
! --- Allocation et Lecture des zones ---

base%nzone     = 1
base%nzone_str = 0
base%nzone_ust = 0
allocate(base%zone(base%nzone))

! les maillages (2D ou 3D) sont de meme type que la base

base%zone(1)%imesh = base%imesh   
call readcgnszone(unit, ib, iz, base%zone(1))

! Denombrement des zones structurees et non structurees

select case(base%zone(iz)%type)
case(Structured)
  base%nzone_str = base%nzone_str + 1
case(Unstructured)
  base%nzone_ust = base%nzone_ust + 1
case default
  call erreur("Developpement","Type de maillage non prevu")
endselect

!-------------------------
endsubroutine readcgnsbase
