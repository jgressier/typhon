!------------------------------------------------------------------------------!
! Procedure : trait_zoneparam             Auteur : J. Gressier
!                                         Date   : Juillet 2002
! Fonction                                Modif  : Juin 2003 (cf historique)
!   Traitement des paramètres de définition des zones
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine trait_zoneparam(block, solver, zone)

use RPM
use VARCOM
use OUTPUT
use DEFZONE
use MENU_BOCO

implicit none

! -- Declaration des entrées --
type(rpmblock), target  :: block    ! blocks RPM (paramètres de la zone à lire)
integer                 :: solver  ! type de solveur

! -- Declaration des sorties --
type(st_zone)          :: zone

! -- Declaration des variables internes --
type(rpmblock), pointer :: pblock, pcour  ! pointeur de bloc RPM
integer                 :: nkey           ! nombre de clefs
integer                 :: nboco          ! nombre de conditions limites
integer                 :: nzr            ! nombre de couplages avec d'autres
                                          ! zones
integer                 :: ib             ! indice de parcours des boco
character(len=dimrpmlig):: str            ! chaîne RPM intermédiaire

! -- Debut de la procedure --

! -------------------------
! définition de la modélisation

zone%defsolver%typ_solver = solver

select case(solver)
case(solKDIF)
  call def_model_kdif(block, zone%defsolver)
endselect

! -------------------------
! Définition du maillage

call def_mesh(block, zone%defmesh)

! -------------------------
! Définition des paramètres de simulation

call def_time(block, zone%defsolver)

call def_spat(block, zone%defsolver)

! -------------------------
! Définition des conditions aux limites et paramètres de couplage

! -- Détermination du nombre de couplages avec d'autres zones
call print_info(5,"- Nombre de zones couplées")

pblock => block
call seekrpmblock(pblock, "BOCO", 0, pcour, nboco)

if (nboco < 1) call erreur("lecture de menu", &
                           "Pas de définition de conditions aux limites (BOCO)")
! Initialisation du nombre de couplages avec d'autres zones
nzr = 0 

do ib = 1, nboco

  call seekrpmblock(pblock, "BOCO", ib, pcour, nkey)
  
  ! -- Détermination du type de condition aux limites 

  call rpmgetkeyvalstr(pcour, "TYPE", str)
  
  select case(bocotype(str))
  case(bc_coupling)
  nzr = nzr+1
  endselect

enddo

! Allocation de coupling
zone%ncoupling = nzr
!if (zone%ncoupling > 0) then
  allocate(zone%coupling(zone%ncoupling))
!endif

print*, "!DEBUG", zone%nom , zone%ncoupling
! Conditions aux limites

call def_boco(block, solver, zone%defsolver, zone%coupling, zone%ncoupling)

! -------------------------
! Définition de l'initialisation

call def_init(block, solver, zone%defsolver)

call def_other(block, solver, zone%defsolver)



endsubroutine trait_zoneparam
