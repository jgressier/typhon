!------------------------------------------------------------------------------!
! Procedure : def_model_vortex            Auteur : J. Gressier
!                                         Date   : Fevrier 2004
! Fonction                                Modif  : (cd historique)
!   Traitement des parametres du fichier menu principal
!   Parametres de definition du modele d'integration des VORTEX
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine def_model_vortex(block, defsolver)

use RPM
use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_SOLVER
use MENU_VORTEX

implicit none

! -- Declaration des entrees --
type(rpmblock), target :: block

! -- Declaration des sorties --
type(mnu_solver)       :: defsolver

! -- Declaration des variables internes --
type(rpmblock), pointer  :: pblock, pcour  ! pointeur de bloc RPM
integer                  :: nkey           ! nombre de clefs
integer                  :: i
character(len=dimrpmlig) :: str            ! chaine RPM intermediaire

! -- Debut de la procedure --

call print_info(5,"- Definition du modele de singularites tourbillonaires")

! -- Recherche du BLOCK:MODEL

pblock => block
call seekrpmblock(pblock, "MODEL", 0, pcour, nkey)

if (nkey /= 1) call erreur("lecture de menu", &
                           "bloc MODEL inexistant ou surnumeraire")

! -- lecture du type de singularite VORTEX

defsolver%defvort%typ_vortex = cnull

call rpmgetkeyvalstr(pcour, "VORTEX", str)

if (samestring(str,"SINGULARITY")) defsolver%defvort%typ_vortex = vort_sng
if (samestring(str,"BLOB"))        defsolver%defvort%typ_vortex = vort_blob
if (samestring(str,"RING"))        defsolver%defvort%typ_vortex = vort_ring
if (samestring(str,"FILAMENT"))    defsolver%defvort%typ_vortex = vort_fil

select case(defsolver%defvort%typ_vortex)
case(vort_sng)
  call print_info(10,"    singularites VORTEX brutes")
case(vort_blob)
  call erreur("lecture de menu", "singularite BLOB non implementee")
case(vort_ring)
  call erreur("lecture de menu", "singularite RING non implementee")
case(vort_fil)
  call erreur("lecture de menu", "singularite FILAMENT non implementee")
endselect


endsubroutine def_model_vortex

!------------------------------------------------------------------------------!
! Historique des modifications
!
! fev  2004 : creation de la procedure
!------------------------------------------------------------------------------!
