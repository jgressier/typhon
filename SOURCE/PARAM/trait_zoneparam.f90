!------------------------------------------------------------------------------!
! Procedure : trait_zoneparam             Auteur : J. Gressier
!                                         Date   : Juillet 2002
! Fonction                                Modif  : (cf historique)
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
  if (.not.pass_kdif) call erreur("restriction","solveur Conduction indisponible")
  call def_model_kdif(block, zone%defsolver)
case(solNS)
  if (.not.pass_ns) call erreur("restriction","solveur Navier-Stokes indisponible")
  call def_model_ns(block, zone%defsolver)
case(solVORTEX)
  if (.not.pass_vort) call erreur("restriction","solveur Vortex indisponible")
  call def_model_vortex(block, zone%defsolver)
case default
  call erreur("lecture de menu","solveur inconnu")
endselect

! -------------------------
! Définition du maillage

call def_mesh(block, zone%defmesh)

! -------------------------
! Définition des paramètres de simulation

call def_time(block, solver, zone%deftime)

call def_spat(block, solver, zone%defspat)

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

  if(samestring(str,"COUPLING")) nzr = nzr +1

enddo

! Allocation de coupling
zone%ncoupling = nzr
!if (zone%ncoupling > 0) then
  allocate(zone%coupling(zone%ncoupling))
!endif

! Conditions aux limites

call def_boco(block, solver, zone%defsolver, zone%coupling, zone%ncoupling)

! -------------------------
! Définition de l'initialisation

call def_init(block, solver, zone%defsolver)

! -------------------------
! Définition des capteurs

call def_capteurs(block, solver, zone%defsolver)

! -------------------------
! Définition des autres paramètres

call def_other(block, solver, zone%defsolver)



endsubroutine trait_zoneparam

!------------------------------------------------------------------------------!
! Historique des modifications
!
! Juil 2002 : création de la procédure
! Sept 2003 : appel à la définition de solveur NS
! Nov  2003 : appel à la définition de capteurs
! Fev  2004 : appel à la définition de solveur VORTEX
!------------------------------------------------------------------------------!
