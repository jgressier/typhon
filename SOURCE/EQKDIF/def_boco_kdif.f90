!------------------------------------------------------------------------------!
! Procedure : def_boco_kdif                    Auteur : J. Gressier
!                                         Date   : Mars 2003
! Fonction                                Modif  : 
!   Traitement des paramètres du fichier menu principal
!   Paramètres principaux du projet
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine def_boco_kdif(block, type, boco, unif)

use RPM
use TYPHMAKE
use VARCOM
use OUTPUT
use MENU_KDIF
use MENU_BOCO

implicit none

! -- Declaration des entrées --
type(rpmblock), target :: block    ! bloc RPM contenant les définitions
integer                :: type     ! type de condition aux limites
integer                :: unif     ! uniformité de la condition limite

! -- Declaration des sorties --
type(st_boco_kdif) :: boco

! -- Declaration des variables internes --
type(rpmblock), pointer  :: pblock, pcour  ! pointeur de bloc RPM
integer                  :: ib, nkey,i
character(len=dimrpmlig) :: str            ! chaîne RPM intermédiaire

! -- Debut de la procedure --

pblock => block

select case(type)

case(bc_wall_adiab)
  boco%flux=0._krp

case(bc_wall_isoth)
  
  select case(unif)
  
  case(uniform)
    call rpmgetkeyvalreal(pblock, "WALL_TEMP", boco%temp_wall)

  case(nonuniform)
    boco%alloctemp = .true.
    call rpmgetkeyvalstr(pblock, "TEMP_FILE", str)
    boco%tempfile = str

  endselect

case(bc_wall_flux)
  
  select case(unif)
  
  case(uniform)
    call rpmgetkeyvalreal(pblock, "WALL_FLUX", boco%flux)
    boco%flux = - boco%flux ! convention flux sortant dans le code
                            ! CL : convention flux entrant pour utilisateur

  case(nonuniform)
    boco%allocflux = .true.
    call rpmgetkeyvalstr(pblock, "FLUX_FILE", str)
    boco%fluxfile = str

  endselect

case(bc_wall_hconv)
  
  select case(unif)
  
  case(uniform)
    call rpmgetkeyvalreal(pblock, "H", boco%h_conv)
    boco%h_conv = - boco%h_conv ! convention flux sortant dans le code
                                ! CL : convention flux entrant pour utilisateur
    call rpmgetkeyvalreal(pblock, "T_CONV", boco%temp_conv)

  case(nonuniform)
    boco%allochconv = .true.
    call rpmgetkeyvalstr(pblock, "H_FILE", str)
    boco%hfile = str
    call rpmgetkeyvalstr(pblock, "TCONV_FILE", str)
    boco%tconvfile = str

  endselect

case default
  call erreur("Lecture de menu","type de conditions aux limites non reconnu&
              & pour le solveur de conduction")
endselect


endsubroutine def_boco_kdif


!------------------------------------------------------------------------------!
! Historique des modifications
!
! mars 2003 (v0.0.1b): création de la routine
! juin 2004 : conditions de Neumann et de convection
!------------------------------------------------------------------------------!


