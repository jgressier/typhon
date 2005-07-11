!------------------------------------------------------------------------------!
! Procedure : def_boco_ns                 Auteur : J. Gressier
!                                         Date   : Novembre 2003
! Fonction                                Modif  : (cf historique)
!   Traitement des parametres du fichier menu principal
!   Parametres principaux du projet
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine def_boco_ns(block, type, boco)

use RPM
use TYPHMAKE
use VARCOM
use OUTPUT
use MENU_NS

implicit none

! -- Declaration des entrees --
type(rpmblock), target :: block    ! bloc RPM contenant les definitions
integer                :: type     ! type de condition aux limites

! -- Declaration des sorties --
type(st_boco_ns) :: boco

! -- Declaration des variables internes --
type(rpmblock), pointer  :: pblock, pcour  ! pointeur de bloc RPM
integer                  :: ib, nkey
character(len=dimrpmlig) :: str            ! chaine RPM intermediaire
integer                  :: info

! -- Debut de la procedure --

pblock => block

select case(type)

case(bc_wall_adiab)
  !call erreur("Developpement","'bc_wall_adiab' : Cas non implemente")

case(bc_wall_isoth)
  call rpmgetkeyvalreal(pblock, "WALL_TEMP", boco%temp_wall)
  !call erreur("Developpement","'bc_wall_isoth' : Cas non implemente")

case(bc_wall_flux)
  call rpmgetkeyvalreal(pblock, "WALL_FLUX", boco%flux)
  !call erreur("Developpement","'bc_wall_isoth' : Cas non implemente")

case(bc_inlet_sub)
   call rpmgetkeyvalreal(pblock, "PI",        boco%ptot)
   call rpmgetkeyvalreal(pblock, "TI",        boco%ttot)
   call rpmgetkeyvalstr (pblock, "DIRECTION", str)
   boco%direction = v3d_of(str, info)
   if (info /= 0) &
     call erreur("lecture de menu","probleme a la lecture du vecteur DIRECTION") 
   boco%direction = boco%direction / abs(boco%direction)
   !call erreur("Developpement","'bc_inlet_sub' : Cas non implemente")

case(bc_inlet_sup)
   call rpmgetkeyvalreal(pblock, "PI",        boco%ptot)
   call rpmgetkeyvalreal(pblock, "TI",        boco%ttot)
   call rpmgetkeyvalreal(pblock, "MACH",      boco%mach)
   call rpmgetkeyvalstr (pblock, "DIRECTION", str)
   boco%direction = v3d_of(str, info)
   if (info /= 0) &
     call erreur("lecture de menu","probleme a la lecture du vecteur DIRECTION") 
   boco%direction = boco%direction / abs(boco%direction)
   !call erreur("Developpement","'bc_inlet_sup' : Cas non implemente")

case(bc_outlet_sub)
  call rpmgetkeyvalreal(pblock, "P",         boco%pstat)
  !call erreur("Developpement","'bc_outlet_sub' : Cas non implemente")

case(bc_outlet_sup)
  !call erreur("Developpement","'bc_outlet_sup' : Cas non implemente")
  ! pas de lecture de parametre

case default
  call erreur("Lecture de menu","type de conditions aux limites non reconnu&
              & pour le solveur Navier-Stokes")
endselect


endsubroutine def_boco_ns

!------------------------------------------------------------------------------!
! Historique des modifications
!
! nov  2003 : creation de la routine
! juin 2004 : definition et lecture de conditions limites (inlet/outlet)
!------------------------------------------------------------------------------!


