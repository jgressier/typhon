!------------------------------------------------------------------------------!
! Procedure : def_boco_vortex             Auteur : J. Gressier
!                                         Date   : Fevrier 2004
! Fonction                                Modif  : (cf historique)
!   Traitement des parametres du fichier menu principal
!   Parametres de definitions des conditions limites
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine def_boco_vortex(block, type, boco, unif)

use RPM
use TYPHMAKE
use VARCOM
use OUTPUT
use MENU_VORTEX
use MENU_BOCO
use PAN2D_LIN

implicit none

! -- Declaration des entrees --
type(rpmblock), target :: block    ! bloc RPM contenant les definitions
integer                :: type     ! type de condition aux limites
integer                :: unif     ! uniformite de la condition limite

! -- Declaration des sorties --
type(st_boco_vort) :: boco

! -- Declaration des variables internes --
type(rpmblock), pointer  :: pblock, pcour  ! pointeur de bloc RPM
integer                  :: ib, nkey, i, info
character(len=dimrpmlig) :: str            ! chaine RPM intermediaire

! -- Debut de la procedure --

pblock => block

select case(type)

case(bc_farfield)
  
  call rpmgetkeyvalstr(pblock, "VELOCITY", str)

  boco%vect = v3d_of(str, info)
 
  if (info /= 0) then
    call erreur("lecture de menu","probleme a la lecture du vecteur VELOCITY") 
  endif

case(bc_wall)
  
  call rpmgetkeyvalstr(pblock, "ELEMENT", str, "VORTEX_LIN")
  boco%element = inull

  if (samestring(str, "VORTEX_LIN" )) boco%element = sng_vortexlin

  select case(boco%element)
  case(sng_vortexlin)
    call print_info(10,"      panneau de vorticite, distribution lineaire")
  case default
    call erreur("lecture de menu","type de singularite inconnu") 
  endselect

case(bc_kutta)
  
  call rpmgetkeyvalstr(pblock, "MODE", str, "EQUILIBRIUM")
  boco%mode = cnull

  if (samestring(str, "EQUILIBRIUM" )) boco%mode = kt_equilibrium
  if (samestring(str, "SHEDDING" ))    boco%mode = kt_shedding

  select case(boco%mode)
  case(kt_equilibrium)
  case(kt_shedding)
    call erreur("developpement","mode de calcul de condition KUTTA en developpement") 
  case default
    call erreur("lecture de menu","mode de calcul de condition KUTTA inconnu") 
  endselect

case default
  call erreur("Lecture de menu","type de conditions aux limites non reconnu&
              & pour le solveur VORTEX")
endselect


endsubroutine def_boco_vortex


!------------------------------------------------------------------------------!
! Historique des modifications
!
! fev  2004 : creation de la routine
! mars 2004 : lecture de champ infini uniforme
!------------------------------------------------------------------------------!


