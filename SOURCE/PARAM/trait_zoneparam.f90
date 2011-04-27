!------------------------------------------------------------------------------!
! Procedure : trait_zoneparam
!                            
! Fonction                   
!   Traitement des parametres de definition des zones
!
!------------------------------------------------------------------------------!
subroutine trait_zoneparam(prj, block, solver, zone)

use RPM
use VARCOM
use OUTPUT
use DEFZONE
use MENU_GEN
use MENU_BOCO

implicit none

! -- INPUTS --
type(mnu_project)       :: prj
type(rpmblock), target  :: block    ! blocks RPM (parametres de la zone a lire)
integer                 :: solver  ! type de solveur

! -- OUTPUTS --
type(st_zone)          :: zone

! -- Internal variables --
type(rpmblock), pointer :: pblock, pcour  ! pointeur de bloc RPM
integer                 :: nkey           ! nombre de clefs
integer                 :: nboco          ! nombre de conditions limites
integer                 :: nzr            ! nombre de couplages avec d'autres
                                          ! zones
integer                 :: ib             ! indice de parcours des boco
character(len=dimrpmlig):: str            ! chaine RPM intermediaire

! -- BODY --

! -------------------------
! MESH definition

call def_ale (block, zone%defsolver%defale)
call def_mrf (block, zone%defsolver%defmrf)
call def_mesh(prj, block, zone%defsolver%defmesh)

! -------------------------
! MODEL definition

call init_mnu_solver(zone%defsolver)
zone%defsolver%typ_solver = solver

select case(solver)
case(solKDIF)
  if (.not.pass_kdif) call erreur("restriction","solveur Conduction indisponible")
  call def_model_kdif(block, zone%defsolver)
case(solNS)
  if (.not.pass_ns) call erreur("restriction","solveur Navier-Stokes indisponible")
  call def_model_ns(prj, block, zone%defsolver)
case(solVORTEX)
  if (.not.pass_vort) call erreur("restriction","solveur Vortex indisponible")
  call def_model_vortex(block, zone%defsolver)
case default
  call erreur("lecture de menu","solveur inconnu")
endselect

! -------------------------
! SCHEME/NUMERICAL parameters

call def_time(prj, block, solver, zone%defsolver%deftime)

call def_spat(block, zone%defsolver, zone%defsolver%defspat, zone%defsolver%defmesh)

call def_amr(block, solver, zone%defsolver%defamr)

if (mpi_run) call def_mpi(block, solver, zone%defsolver%defmpi)

! -------------------------
! Definition des conditions aux limites et parametres de couplage

! -- Determination du nombre de couplages avec d'autres zones
call print_info(5,"- Number of coupled zones")

pblock => block
call seekrpmblock(pblock, "BOCO", 0, pcour, nboco)

nzr = 0 

do ib = 1, nboco

  call seekrpmblock(pblock, "BOCO", ib, pcour, nkey)
  
  ! -- Determination du type de condition aux limites 

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
! CONNECTION definition

call def_connect(block, zone%defsolver)

! -------------------------
! Definition de l'initialisation

call def_init(block, solver, zone%defsolver)

! -------------------------
! Definition des capteurs

call def_probe(block, solver, zone%defsolver)

! -------------------------
! Definition des autres parametres

call def_other(block, solver, zone%defsolver)

! -------------------------
! Final check

call def_check(zone)



endsubroutine trait_zoneparam
!------------------------------------------------------------------------------!
! Changes history
!
! Juil 2002 : creation de la procedure
! Sept 2003 : appel a la definition de solveur NS
! Nov  2003 : appel a la definition de capteurs
! Fev  2004 : appel a la definition de solveur VORTEX
!------------------------------------------------------------------------------!
