!------------------------------------------------------------------------------!
! Procedure : def_time                    Auteur : J. Gressier
!                                         Date   : Novembre 2002
! Fonction                                Modif  : cf historique
!   Traitement des paramètres du fichier menu principal
!   Paramètres d'intégration temporelle
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine def_time(block, solver, deftime)

use VARCOM
use RPM
use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_NUM

implicit none

! -- Declaration des entrées --
type(rpmblock), target :: block
integer                :: solver

! -- Declaration des sorties --
type(mnu_time) :: deftime

! -- Declaration des variables internes --
type(rpmblock), pointer  :: pblock, pcour  ! pointeur de bloc RPM
integer                  :: nkey           ! nombre de clefs
integer                  :: i
character(len=dimrpmlig) :: str            ! chaîne RPM intermédiaire

! -- Debut de la procedure --

call print_info(5,"- Définition des paramètres d'intégration temporelle")

! -- Recherche du BLOCK:TIME_PARAM

pblock => block
call seekrpmblock(pblock, "TIME_PARAM", 0, pcour, nkey)

if (nkey /= 1) call erreur("lecture de menu", &
                           "bloc TIME_PARAM inexistant ou surnuméraire")

! -- type de calcul du pas de temps, et paramètre associés

call rpmgetkeyvalstr(pcour, "DTCALC", str, "STABILITY_CONDITION")
deftime%stab_meth = cnull

if (samestring(str,"STABILITY_CONDITION")) deftime%stab_meth = stab_cond
if (samestring(str,"GIVEN"))               deftime%stab_meth = given_dt

if (deftime%stab_meth == cnull) &
  call erreur("lecture de menu","methode de calcul DTCALC inconnue")

select case(deftime%stab_meth)
case(given_dt)
  call rpmgetkeyvalreal(pcour, "DT", deftime%dt)
case(stab_cond)
  select case(solver)
  case(solKDIF)
    call rpmgetkeyvalreal(pcour, "FOURIER", deftime%stabnb)
  case(solNS)
    call rpmgetkeyvalreal(pcour, "CFL", deftime%stabnb)
  case default
    call erreur("lecture de menu","solveur inconnu (définition temporelle)")
  endselect
endselect

! -- type d'intégration --

deftime%local_dt = .false.



endsubroutine def_time

!------------------------------------------------------------------------------!
! Historique des modifications
! nov  2002 : création (vide) pour lien avec l'arborescence
! sept 2003 : lecture des paramètres de calcul du pas de temps
!------------------------------------------------------------------------------!
