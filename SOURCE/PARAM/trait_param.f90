!------------------------------------------------------------------------------!
! Procedure : trait_param                 Auteur : J. Gressier
!                                         Date   : Juillet 2002
! Fonction                                Modif  : (cf historique)
!   Traitement des paramètres du fichier menu principal
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine trait_param(block, world)

use RPM
use TYPHMAKE
use VARCOM
use OUTPUT
use MODWORLD
use MENU_SOLVER

implicit none

! -- Declaration des entrées --
type(rpmblock), target :: block

! -- Declaration des sorties --
type(st_world) :: world

! -- Declaration des variables internes --
type(rpmblock), pointer  :: pblock, pcour, pzone  ! pointeurs de bloc RPM
integer                  :: nkey           ! nombre de clefs
logical                  :: localzone      ! déclaration locale d'une zone
integer                  :: izone          ! numéro local de zone
integer                  :: icoupl         ! numéro local de raccord
integer                  :: solver         ! type de solveur
integer                  :: info           ! etat de l'ouverture de fichier
character(len=dimrpmlig) :: str, fic       ! chaînes RPM intermédiaire

! -- Debut de la procedure --

! -- Recherche du BLOCK:PROJECT et traitement

call def_project(block, world%prj)
! -- Recherche des BLOCK:ZONE 

pblock => block
call seekrpmblock(pblock, "ZONE", 0, pcour, nkey)
if (nkey == 0) call erreur("Lecture de menu","définition de ZONE manquante")

! initialisation de WORLD et allocation des zones

call new(world, nkey, 0,  world%prj%ncoupling)  

! -- Recherche du BLOCK:OUTPUT et traitement

call def_output(block, world)

! -- Lecture des zones

do izone = 1, world%prj%nzone

  call seekrpmblock(pblock, "ZONE", izone, pcour, nkey)
  
  call new(world%zone(izone), izone)   ! intialisation de la zone

  call rpmgetkeyvalstr(pcour, "NAME", str, "NONAME")
  world%zone(izone)%nom = str

  call rpmgetkeyvalstr(pcour, "SOLVER", str)

  solver = 0
  if (samestring(str, "HEAT" ))  solver = solKDIF 
  if (samestring(str, "EULER"))  solver = solNS
  if (samestring(str, "FLUID"))  solver = solNS
  if (samestring(str, "NS"))     solver = solNS
  if (samestring(str, "VORTEX")) solver = solVORTEX
  if (solver == 0) call erreur("Lecture du menu", &
                               "Type de solveur incorrect : "//trim(str))
  
  if (rpm_existkey(pcour, "FILE")) then

    call rpmgetkeyvalstr(pcour, "FILE", fic)
    open(unit=uf_menu, file=trim(fic), iostat=info)
    if (info /= 0) call erreur("Lecture du menu","fichier "//trim(fic)// &
                               " introuvable ou interdit en lecture")
    call readrpmblock(uf_menu, uf_log, 1, pzone)
    close(uf_menu) 

  else

    call print_info(10,"Poursuite de la lecture des paramètres ZONE")
    pzone => pblock
    
  endif

  call trait_zoneparam(pzone, solver, world%zone(izone))

enddo

! -- Recherche des BLOCK:COUPLING et traitement
if (world%prj%ncoupling > 0) then

do icoupl = 1, world%prj%ncoupling

!  print*,"!!! TRAITEMENT DES BLOCK:COUPLING à développer"
  call def_coupling(block, world%coupling(icoupl), world%zone,&
                   world%prj%nzone, icoupl)

enddo

endif

endsubroutine trait_param

!------------------------------------------------------------------------------!
! Historique des modifications
!
! Juil 2002 : création de la procédure
! Juin 2003 : définition des couplages
! Fev  2004 : ajout de solveur VORTEX
!------------------------------------------------------------------------------!
