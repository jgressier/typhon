!------------------------------------------------------------------------------!
! Procedure : cgns2typhon_zone            Auteur : J. Gressier
!                                         Date   : Novembre 2002
! Fonction                                Modif  : (cf historique)
!   Conversion d'une base CGNS en ZONE Typhon
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

subroutine cgns2typhon_zone(cgnsbase, typhonzone) 

use CGNS_STRUCT   ! Définition des structures CGNS
use DEFZONE       ! Définition des structures TYPHON
use OUTPUT        ! Sorties standard TYPHON
use VARCOM        ! Variables globales et définition de constantes

implicit none 

! -- Entrées --
type(st_cgns_base) :: cgnsbase        ! structure des données CGNS

! -- Sorties --
type(st_zone)      :: typhonzone      ! structure des données TYPHON

! -- Variables internes --
type(st_grid), pointer :: pgrid
integer                :: i, ist         ! indices courants

! -- Début de procédure

call print_info(5, "- conversion de la zone "//trim(cgnsbase%nom))

!typhonzone%ndom      = cgnsbase%nzone       ! (!) une zone CGNS est un domaine de maillage
!typhonzone%nmesh_str = cgnsbase%nzone_str
!typhonzone%nmesh_ust = cgnsbase%nzone_ust

if (cgnsbase%nzone_ust > 1) then
  call erreur("Conversion CGNS/TYPHON","Un seul domaine non structuré admis")
endif

ist = 0

do i = 1, cgnsbase%nzone

  pgrid => newgrid(typhonzone)
  
  select case(cgnsbase%zone(i)%type)
  case(Structured)
    call erreur("Développement","traitement de maillage structuré non implémenté")
    ist = ist + 1
    typhonzone%typ_mesh = mshSTR
    !call cgns2typhon_strmesh(cgnsbase%zone(i), typhonzone%str_mesh(ist))

  case(Unstructured)
    typhonzone%typ_mesh = mshUST
    call cgns2typhon_ustmesh(cgnsbase%zone(i), pgrid%umesh)

  case default
    call erreur("Développement","Type de maillage non prévu")
  endselect
  
enddo

call print_info(8, "Fin de la conversion CGNS -> TYPHON")


!-------------------------
endsubroutine cgns2typhon_zone

!------------------------------------------------------------------------------!
! Historique des modifications
!
! nov  2002 : création de la procédure
! avr  2004 : suppression des maillages structurés 
!             création de structures MGRID
!------------------------------------------------------------------------------!
