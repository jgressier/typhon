!------------------------------------------------------------------------------!
! Procedure : def_model_kdif              Auteur : J. Gressier
!                                         Date   : Avril 2003
! Fonction                                Modif  : (cd historique)
!   Traitement des paramètres du fichier menu principal
!   Paramètres de définition du modèle de conduction de la chaleur
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine def_model_kdif(block, defsolver)

use RPM
use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_SOLVER

implicit none

! -- Declaration des entrées --
type(rpmblock), target :: block

! -- Declaration des sorties --
type(mnu_solver)       :: defsolver

! -- Declaration des variables internes --
type(rpmblock), pointer  :: pblock, pcour  ! pointeur de bloc RPM
integer                  :: nkey           ! nombre de clefs
integer                  :: i
character(len=dimrpmlig) :: str            ! chaîne RPM intermédiaire

! -- Debut de la procedure --

call print_info(5,"- Définition du modèle de conduction de la chaleur")

! -- Recherche du BLOCK:MODEL

pblock => block
call seekrpmblock(pblock, "MODEL", 0, pcour, nkey)

if (nkey /= 1) call erreur("lecture de menu", &
                           "bloc MODEL inexistant ou surnuméraire")

defsolver%nequat = 1

! -- lecture du type de matériau

call rpmgetkeyvalstr(pcour, "MATERIAL", str)

if (samestring(str,"DEFINITION")) then

  call rpmgetkeyvalstr(pcour, "MAT_TYPE", str, "LIN")
  if (samestring(str, "LIN"))  defsolver%defkdif%materiau%type   = mat_LIN
  if (samestring(str, "KNL"))  defsolver%defkdif%materiau%type   = mat_KNL
  if (samestring(str, "XMAT")) defsolver%defkdif%materiau%type   = mat_XMAT

  select case(defsolver%defkdif%materiau%type)
  case(mat_LIN)
    call print_info(10,"    materiau linéaire")
    defsolver%defkdif%materiau%Kd%type = LOI_CST
    call rpmgetkeyvalreal(pcour, "CONDUCT",  defsolver%defkdif%materiau%Kd%valeur)
    call rpmgetkeyvalreal(pcour, "HEATCAPA", defsolver%defkdif%materiau%Cp)

  case(mat_KNL)
    call print_info(10,"    materiau à conductivité non constante")
    call rpmgetkeyvalstr(pcour, "CONDUCT_TYPE", str)
    if (samestring(str, "CST"))  defsolver%defkdif%materiau%Kd%type = LOI_CST
    if (samestring(str, "POLY")) defsolver%defkdif%materiau%Kd%type = LOI_POLY
    if (samestring(str, "PTS"))  defsolver%defkdif%materiau%Kd%type = LOI_PTS
    
    select case(defsolver%defkdif%materiau%Kd%type)
    case(LOI_CST)
      call print_info(10,"    conductivité constante")
      call rpmgetkeyvalreal(pcour, "CONDUCT",  defsolver%defkdif%materiau%Kd%valeur)

    case(LOI_POLY)
      call print_info(10,"    conductivité définie sous forme polynomiale")
      call rpmgetkeyvalint(pcour, "POLY_ORDER",  defsolver%defkdif%materiau%Kd%poly%ordre)
      allocate(defsolver%defkdif%materiau%Kd%poly%coef(defsolver%defkdif%materiau%Kd%poly%ordre+1))
      call rpmgetkeyvalstr(pcour, "COEFFILE", str)
      open(unit=1001, file = str, form="formatted")
      read(1001,*),  (defsolver%defkdif%materiau%Kd%poly%coef(i),i = 1, &
                      defsolver%defkdif%materiau%Kd%poly%ordre+1) 
      close(1001)

    case(LOI_PTS)
      call print_info(10,"    conductivité définie pt par pt")

    case default
      call erreur("lecture de menu", "type de conductivité inconnu")

    endselect

    call rpmgetkeyvalreal(pcour, "HEATCAPA", defsolver%defkdif%materiau%Cp)
  
  case(mat_XMAT)
    call print_info(10,"    materiau non linéaire")

  case default
  call erreur("lecture de menu", "type de matériau inconnu")
  endselect


else
  call erreur("lecture de menu","Définition de MATERIAL inconnue")
endif


endsubroutine def_model_kdif

!------------------------------------------------------------------------------!
! Historique des modifications
!
! avril 2003 (v0.0.1b): création de la procédure
!                       définition interne de matériau à propriétés constantes
! juillet 2003        : conductivité polynomiale
!------------------------------------------------------------------------------!
