!------------------------------------------------------------------------------!
! Procedure : def_model_kdif              Auteur : J. Gressier
!                                         Date   : Avril 2003
! Fonction                                Modif  : (cd historique)
!   Traitement des parametres du fichier menu principal
!   Parametres de definition du modele de conduction de la chaleur
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

call print_info(5,"- Definition of Heat Transfer model")

! -- Recherche du BLOCK:MODEL

pblock => block
call seekrpmblock(pblock, "MODEL", 0, pcour, nkey)

if (nkey /= 1) call erreur("lecture de menu", &
                           "bloc MODEL inexistant ou surnumeraire")

defsolver%nequat = 1
defsolver%nsca   = 1
defsolver%nvec   = 0
allocate(defsolver%namesca(defsolver%nsca))
allocate(defsolver%namevec(defsolver%nvec))
defsolver%namesca(1) = 'Temperature'

! -- lecture du type de materiau

call rpmgetkeyvalstr(pcour, "MATERIAL", str)

if (samestring(str,"DEFINITION")) then

  call rpmgetkeyvalstr(pcour, "MAT_TYPE", str, "LIN")
  if (samestring(str, "LIN"))  defsolver%defkdif%materiau%type   = mat_LIN
  if (samestring(str, "KNL"))  defsolver%defkdif%materiau%type   = mat_KNL
  if (samestring(str, "XMAT")) defsolver%defkdif%materiau%type   = mat_XMAT

  select case(defsolver%defkdif%materiau%type)
  case(mat_LIN)
    call print_info(10,"    material with constant properties")
    defsolver%defkdif%materiau%Kd%type = LOI_CST
    call rpmgetkeyvalreal(pcour, "CONDUCT",  defsolver%defkdif%materiau%Kd%valeur)
    call rpmgetkeyvalreal(pcour, "HEATCAPA", defsolver%defkdif%materiau%Cp)

  case(mat_KNL)
    call print_info(10,"    material with constant capacity and variable conductivity")
    call rpmgetkeyvalstr(pcour, "CONDUCT_TYPE", str)
    if (samestring(str, "CST"))  defsolver%defkdif%materiau%Kd%type = LOI_CST
    if (samestring(str, "POLY")) defsolver%defkdif%materiau%Kd%type = LOI_POLY
    if (samestring(str, "PTS"))  defsolver%defkdif%materiau%Kd%type = LOI_PTS
    
    select case(defsolver%defkdif%materiau%Kd%type)
    case(LOI_CST)
      call print_info(10,"    constant conductivity")
      call rpmgetkeyvalreal(pcour, "CONDUCT",  defsolver%defkdif%materiau%Kd%valeur)

    case(LOI_POLY)
      call print_info(10,"    polynomial conductivity")
      call rpmgetkeyvalint(pcour, "POLY_ORDER",  defsolver%defkdif%materiau%Kd%poly%ordre)
      allocate(defsolver%defkdif%materiau%Kd%poly%coef(defsolver%defkdif%materiau%Kd%poly%ordre+1))
      call rpmgetkeyvalstr(pcour, "COEFFILE", str)
      open(unit=1001, file = str, form="formatted")
      read(1001,*) (defsolver%defkdif%materiau%Kd%poly%coef(i),i = 1, &
                    defsolver%defkdif%materiau%Kd%poly%ordre+1) 
      close(1001)

    case(LOI_PTS)
      call print_info(10,"    point definition conductivity")

    case default
      call erreur("parameter reading", "unknown type of conductivity")

    endselect

    call rpmgetkeyvalreal(pcour, "HEATCAPA", defsolver%defkdif%materiau%Cp)
  
  case(mat_XMAT)
    call print_info(10,"    non linear material")
    call erreur("development", "not implemented function")

  case default
    call erreur("parameter reading", "unknown material")
  endselect

  !-- anisotropic definition --

  call rpmgetkeyvalstr(pcour, "OPTION", str, "ISOTROPIC")
  defsolver%defkdif%materiau%isotropic = ' '
  if (samestring(str, "ISOTROPIC"))    defsolver%defkdif%materiau%isotropic = matiso_ISO
  if (samestring(str, "ANISOTROPIC"))  defsolver%defkdif%materiau%isotropic = matiso_ANISO
  if (samestring(str, "UDF"))          defsolver%defkdif%materiau%isotropic = matiso_UDF
  select case(defsolver%defkdif%materiau%isotropic)
  case(matiso_ISO)
    call print_info(10,"    isotropic conductivity")
  case(matiso_ANISO)
    call print_info(10,"    uniform anisotropic conductivity")
  case(matiso_UDF)
    call print_info(10,"    user defined direction of anisotropic conductivity")
  case default
    call erreur("parameter reading", "unknown type of anisotropic definition")
  endselect

  !-- radiation definition --

  call rpmgetkeyvalreal(pcour, "TOLERANCE", defsolver%defkdif%tolerance, 1.E-4_krp)

else
  call erreur("parameter reading","unknown definition of MATERIAL")
endif


endsubroutine def_model_kdif

!------------------------------------------------------------------------------!
! Change history
!
! apr  2003 : creation
!             definition interne de materiau a proprietes constantes
! july 2003 : conductivite polynomiale
! june 2005 : anisotropic tensor
!------------------------------------------------------------------------------!
