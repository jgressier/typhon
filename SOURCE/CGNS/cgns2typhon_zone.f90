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

use CGNS_STRUCT   ! Definition des structures CGNS
use DEFZONE       ! Definition des structures TYPHON
use OUTPUT        ! Sorties standard TYPHON
use VARCOM        ! Variables globales et definition de constantes

implicit none 

! -- Entrees --
type(st_cgns_base) :: cgnsbase        ! structure des donnees CGNS

! -- Sorties --
type(st_zone)      :: typhonzone      ! structure des donnees TYPHON

! -- Variables internes --
type(st_grid), pointer :: pgrid
integer                :: i, ist         ! indices courants

! -- Debut de procedure

call print_info(5, "- conversion de la zone "//trim(cgnsbase%nom))

!typhonzone%ndom      = cgnsbase%nzone       ! (!) une zone CGNS est un domaine de maillage
!typhonzone%nmesh_str = cgnsbase%nzone_str
!typhonzone%nmesh_ust = cgnsbase%nzone_ust

if (cgnsbase%nzone_ust > 1) then
  call erreur("Conversion CGNS/TYPHON","Un seul domaine non structure admis")
endif

ist = 0

do i = 1, cgnsbase%nzone

  pgrid => add_grid(typhonzone%gridlist)
  
  select case(cgnsbase%zone(i)%type)
  case(Structured)
    call erreur("Developpement","traitement de maillage structure non implemente")
    ist = ist + 1
    typhonzone%typ_mesh = mshSTR
    !call cgns2typhon_strmesh(cgnsbase%zone(i), typhonzone%str_mesh(ist))

  case(Unstructured)
    typhonzone%typ_mesh = mshUST
    call cgns2typhon_ustmesh(cgnsbase%zone(i), pgrid%umesh)

  case default
    call erreur("Developpement","Type de maillage non prevu")
  endselect
  
enddo

call print_info(8, "Fin de la conversion CGNS -> TYPHON")


!-------------------------
endsubroutine cgns2typhon_zone

!------------------------------------------------------------------------------!
! Changes history
!
! nov  2002 : creation de la procedure
! avr  2004 : suppression des maillages structures 
!             creation de structures MGRID
!------------------------------------------------------------------------------!
