!------------------------------------------------------------------------------!
! Procedure : def_time                    Auteur : J. Gressier
!                                         Date   : Novembre 2002
! Fonction                                Modif  : (cf historique)
!   Traitement des parametres du fichier menu principal
!   Parametres d'integration temporelle
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine def_time(prj, block, solver, deftime)

use VARCOM
use RPM
use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_GEN
use MENU_NUM

implicit none

! -- Declaration des entrees --
type(mnu_project)      :: prj
type(rpmblock), target :: block
integer                :: solver

! -- Declaration des sorties --
type(mnu_time) :: deftime

! -- Declaration des variables internes --
type(rpmblock), pointer  :: pblock, pcour  ! pointeur de bloc RPM
integer                  :: nkey           ! nombre de clefs
integer                  :: i
character(len=dimrpmlig) :: str            ! chaine RPM intermediaire

! -- Debut de la procedure --

call print_info(5,"- Definition des parametres d'integration temporelle")

! -- Recherche du BLOCK:TIME_PARAM

pblock => block
call seekrpmblock(pblock, "TIME_PARAM", 0, pcour, nkey)

if (nkey /= 1) call erreur("lecture de menu", &
                           "bloc TIME_PARAM inexistant ou surnumeraire")

! -- type de calcul du pas de temps, et parametre associes

call rpmgetkeyvalstr(pcour, "DTCALC", str, "STABILITY_CONDITION")
deftime%stab_meth = inull

if (samestring(str,"STABILITY_CONDITION")) deftime%stab_meth = stab_cond
if (samestring(str,"GIVEN"))               deftime%stab_meth = given_dt

if (deftime%stab_meth == inull) &
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
    call erreur("lecture de menu","solveur inconnu (definition temporelle)")
  endselect
endselect

! -- si stationnaire, critere d'arret --

call  rpmgetkeyvalreal(pcour, "RESIDUALS", deftime%maxres, prj%residumax)

! -- type d'integration temporelle --

deftime%local_dt = .false.

! -- type de schema temporel --

call rpmgetkeyvalstr(pcour, "METHOD", str, "EXPLICIT")
deftime%tps_meth = inull

if (samestring(str,"EXPLICIT"))    deftime%tps_meth = tps_expl
if (samestring(str,"RUNGE-KUTTA")) deftime%tps_meth = tps_rk
if (samestring(str,"IMPLICIT"))    deftime%tps_meth = tps_impl
if (samestring(str,"DUAL-TIME"))   deftime%tps_meth = tps_dualt

if (deftime%tps_meth == inull) &
  call erreur("lecture de menu","type d'integration temporelle inconnu")

select case(deftime%tps_meth)
case(tps_expl)
  
case(tps_rk)
  call rpmgetkeyvalint(pcour, "ORDER", deftime%rk%order, 2_kpp)

case(tps_impl)
  call rpmgetkeyvalstr(pcour, "INVERSION", str, "SOR")

  if (samestring(str,"LU"))           deftime%implicite%methode = alg_lu
  if (samestring(str,"JACOBI"))       deftime%implicite%methode = alg_jac
  if (samestring(str,"GAUSS-SEIDEL")) deftime%implicite%methode = alg_gs
  if (samestring(str,"GS"))           deftime%implicite%methode = alg_gs
  if (samestring(str,"SOR"))          deftime%implicite%methode = alg_sor

  select case(deftime%implicite%methode)
  case(alg_lu)
    ! pas de parametre supplementaire

  case(alg_jac)
    call rpmgetkeyvalint (pcour, "MAX_IT",  deftime%implicite%max_it, 10_kpp)
    call rpmgetkeyvalreal(pcour, "INV_RES", deftime%implicite%maxres, 1.e-4_krp)

  case(alg_gs)
    call rpmgetkeyvalint (pcour, "MAX_IT",  deftime%implicite%max_it, 10_kpp)
    call rpmgetkeyvalreal(pcour, "INV_RES", deftime%implicite%maxres, 1.e-4_krp)

  case(alg_sor)
    call rpmgetkeyvalint (pcour, "MAX_IT",    deftime%implicite%max_it, 10_kpp)
    call rpmgetkeyvalreal(pcour, "INV_RES",   deftime%implicite%maxres, 1.e-4_krp)
    call rpmgetkeyvalreal(pcour, "OVERRELAX", deftime%implicite%maxres, 1.e-4_krp)

  case default
    call erreur("algebre","methode d'inversion inconnue")
  endselect

case(tps_dualt)

endselect



endsubroutine def_time

!------------------------------------------------------------------------------!
! Historique des modifications
! nov  2002 : creation (vide) pour lien avec l'arborescence
! sept 2003 : lecture des parametres de calcul du pas de temps
! avr  2004 : lecture des parametres d'integration (implicitation)
!------------------------------------------------------------------------------!
