!------------------------------------------------------------------------------!
! Procedure : cgns2typhon_zone            Auteur : J. Gressier
!                                         Date   : Novembre 2002
! Fonction                                Modif  : (cf historique)
!   Conversion of CGNS base into TYPHON ZONE
!
!------------------------------------------------------------------------------!

subroutine cgns2typhon_zone(defsolver, defmesh, cgnsbase, typhonzone) 

use CGNS_STRUCT   ! Definition des structures CGNS
use DEFZONE       ! Definition des structures TYPHON
use OUTPUT        ! Sorties standard TYPHON
use VARCOM        ! Variables globales et definition de constantes
use MENU_SOLVER
use MENU_MESH

implicit none 

! -- INPUTS --
type(mnu_solver)   :: defsolver       ! solver parameters
type(mnu_mesh)     :: defmesh         ! mesh parameters
type(st_cgns_base) :: cgnsbase        ! CGNS data

! -- OUTPUTS --
type(st_zone)      :: typhonzone      ! Typhon ZONE

! -- Internal variables --
type(st_grid), pointer :: pgrid
integer                :: i, ist         ! indices courants

! -- BODY --

call print_info(5, "- converting CGNS mesh "//trim(cgnsbase%nom)//" to TYPHON zone")

!typhonzone%ndom      = cgnsbase%nzone       ! (!) une zone CGNS est un domaine de maillage
!typhonzone%nmesh_str = cgnsbase%nzone_str
!typhonzone%nmesh_ust = cgnsbase%nzone_ust

if (cgnsbase%nzone_ust > 1) then
  call erreur("CGNS/TYPHON conversion","Only one domain authorized")
endif

ist = 0

do i = 1, cgnsbase%nzone

  pgrid => add_grid(typhonzone%gridlist)
  
  select case(cgnsbase%zone(i)%type)
  case(Structured)
    call erreur("Development","CGNS Structured mesh not implemented")
    ist = ist + 1
    !typhonzone%typ_mesh = mshSTR
    !call cgns2typhon_strmesh(cgnsbase%zone(i), typhonzone%str_mesh(ist))

  case(Unstructured)
    call print_info(5, "  unstructured mesh")
    !typhonzone%typ_mesh = mshUST

    call cgns2typhon_ustmesh(defmesh, cgnsbase%zone(i), pgrid%umesh)

  case default
    call erreur("Development","Unknown type of mesh")
  endselect
  
enddo

call print_info(8, "End of CGNS->TYPHON conversion")


!-------------------------
endsubroutine cgns2typhon_zone

!------------------------------------------------------------------------------!
! Changes history
!
! nov  2002 : creation de la procedure
! avr  2004 : suppression des maillages structures 
!             creation de structures MGRID
! oct  2007 : create SVM mesh
!------------------------------------------------------------------------------!
