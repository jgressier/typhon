!------------------------------------------------------------------------------!
! Procedure : readtyphmshfile             Auteur : J. Gressier
!                                         Date   : Fevrier 2004
! Fonction                                Modif  : (cf historique)
!   Lecture d'un fichier de maillage TYPHMSH
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

subroutine readtyphmshfile(unit, nom, zone)

use DEFZONE       ! structure ZONE
use OUTPUT        ! Sorties standard TYPHON
use DEFZONE
use MGRID
use MESHBASE

implicit none 

! -- Entrees --
integer             :: unit       ! numero d'unite pour la lecture
character(len=*)    :: nom

! -- Sorties --
type(st_zone) :: zone      ! structure de ZONE

! -- Variables internes --
integer                :: ier              ! code d'erreur
integer                :: i                ! indice courant
integer                :: ndom             ! nombre de domaine
character(len=60)      :: str              ! chaine
integer                :: mode             ! mode 1:ASCII 2:BINARY
character              :: typ_geo          ! type de geometrie
type(st_grid), pointer :: pgrid

! -- Debut de procedure
   
! --- Lecture du nom de fichier ---
   
! --- Ouverture du fichier ---

call print_info(5, "* LECTURE DU MAILLAGE TYPHMSH : "//trim(nom))

zone%typ_mesh = mshUST

call print_info(8, "Ouverture du fichier "//trim(nom))

open(unit=unit, file=nom, form="formatted", action="read", iostat=ier)
if (ier /= 0) call erreur("Lecture TYPHMSH","Probleme a l'ouverture du fichier")

! -- verification du format TYPHMSH --

read(unit,*) str
if (.not.samestring(str,"TYPHMSH")) then 
  call erreur("Lecture TYPHMSH","Entete de fichier incorrecte")
endif

! -- mode de lecture --

read(unit,*) str
mode = inull
if (samestring(str,"ASCII"))  mode = 1
if (samestring(str,"BINARY")) mode = 2
if (mode == inull)  call erreur("Lecture TYPHMSH","Entete de fichier incorrecte")

! -- type de repere --

read(unit,*) str
typ_geo = cnull
if (samestring(str,"1DC")) typ_geo = msh_1dcurv
if (samestring(str,"2D"))  typ_geo = msh_2dplan
if (samestring(str,"2DC")) typ_geo = msh_2dcurv
if (samestring(str,"3D"))  typ_geo = msh_3d
if (typ_geo == cnull) call erreur("Lecture TYPHMSH","type de geometrie incorrect")

! -- nombre de courbes (domaines de maillage) --

read(unit,*) ndom

if (ndom /= 1) call erreur("Developpement","nombre de domaines limite a 1")

pgrid => newgrid(zone)

do i = 1, ndom

  call readtyphmsh_dom(unit, pgrid%umesh, typ_geo)

!  ! -- creation des conditions limites --
!
!  call typhmsh_createboco(pgrid)

enddo
   
! --- fermeture du fichier ---

close(ier)
call print_info(8, "Fermeture du fichier "//trim(nom))



!-------------------------
endsubroutine readtyphmshfile

!------------------------------------------------------------------------------!
! Historique des modifications
!
! fev  2004 : creation de la procedure
! mars 2004 : affectation des maillages aux structures "grid"
!------------------------------------------------------------------------------!
