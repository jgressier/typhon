!------------------------------------------------------------------------------!
! Procedure : def_capteurs                Auteur : J. Gressier
!                                         Date   : Novembre 2003
! Fonction                                Modif  : (cf historique)
!   Traitement des paramètres du fichier menu principal
!   Paramètres de définition des capteurs
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine def_capteurs(block, isolver, defsolver)

use RPM
use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_SOLVER
use MENU_BOCO

implicit none

! -- Declaration des entrées --
type(rpmblock), target :: block
integer                :: isolver

! -- Declaration des sorties --
type(mnu_solver)                             :: defsolver

! -- Declaration des variables internes --
type(rpmblock), pointer  :: pblock, pcour  ! pointeur de bloc RPM
integer                  :: nprobe         ! nombre   de capteurs
integer                  :: ip, nkey
character(len=dimrpmlig) :: str            ! chaîne RPM intermédiaire

! -- Debut de la procedure --

call print_info(5,"- Définition des capteurs")

! -- Recherche du BLOCK:BOCO

pblock => block
call seekrpmblock(pblock, "PROBE", 0, pcour, nprobe)
defsolver%nprobe = nprobe

if (nprobe < 1) then

  call print_info(5,"  pas de capteur défini")

else

  allocate(defsolver%probe(nprobe))

  do ip = 1, nprobe

    call seekrpmblock(pblock, "PROBE", ip, pcour, nkey)

    ! -- Détermination du type et du nom

    call rpmgetkeyvalstr(pcour, "TYPE", str)

    defsolver%probe(ip)%type = cnull

    if (samestring(str, "PROBE" ))         defsolver%probe(ip)%type = probe
    if (samestring(str, "BOCO_FIELD" ))    defsolver%probe(ip)%type = boco_field
    if (samestring(str, "BOCO_INTEGRAL" )) defsolver%probe(ip)%type = boco_integral
    if (samestring(str, "RESIDUALS" ))     defsolver%probe(ip)%type = residuals
    
    ! message d'erreur à la lecture de paramètres suivants

    call rpmgetkeyvalstr(pcour, "NAME", str)
    defsolver%probe(ip)%name = str

    ! -- Lecture des paramètres selon le type --

    select case(defsolver%probe(ip)%type)

    case(probe)
      call erreur("Développement","PROBE: type PROBE non implémenté")

    case(boco_field, boco_integral)
      call rpmgetkeyvalstr(pcour, "BOCO", str)
      defsolver%probe(ip)%boco_name  = str
      defsolver%probe(ip)%boco_index = indexboco(defsolver, str)
      if (defsolver%probe(ip)%boco_index == inull) then
        call erreur("définition de capteurs",trim(str)//" nom de condition limite inexistant")
      endif

    case(residuals)
      call erreur("Développement","PROBE: type RESIDUALS non implémenté")

    case default
      call erreur("lecture de menu (PROBE)","type de capteur inconnu")
    endselect

    ! -- Détermination de la quantité à lire

    call rpmgetkeyvalstr(pcour, "QUANTITY", str)
    defsolver%probe(ip)%quantity = quantity(str)

    if (defsolver%probe(ip)%quantity == inull) then
      call erreur("lecture de menu (PROBE)","quantité inconnue")
    endif  

    ! DEV: vérification du type en fonction du solveur  

  enddo

endif


endsubroutine def_capteurs

!------------------------------------------------------------------------------!
! Historique des modifications
!
! nov  2003 : création de la routine
!------------------------------------------------------------------------------!


