!------------------------------------------------------------------------------!
! Procedure : def_capteurs                Auteur : J. Gressier
!                                         Date   : Novembre 2003
! Fonction                                Modif  : (cf historique)
!   Traitement des parametres du fichier menu principal
!   Parametres de definition des capteurs
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

! -- Declaration des entrees --
type(rpmblock), target :: block
integer                :: isolver

! -- Declaration des sorties --
type(mnu_solver)                             :: defsolver

! -- Declaration des variables internes --
type(rpmblock), pointer  :: pblock, pcour  ! pointeur de bloc RPM
integer                  :: nprobe         ! nombre   de capteurs
integer                  :: ip, nkey
character(len=dimrpmlig) :: str            ! chaine RPM intermediaire

! -- Debut de la procedure --

call print_info(5,"- Definition des capteurs")

! -- Recherche du BLOCK:BOCO

pblock => block
call seekrpmblock(pblock, "PROBE", 0, pcour, nprobe)
defsolver%nprobe = nprobe

if (nprobe < 1) then

  call print_info(5,"  pas de capteur defini")

else

  allocate(defsolver%probe(nprobe))

  do ip = 1, nprobe

    call seekrpmblock(pblock, "PROBE", ip, pcour, nkey)

    ! -- Determination du type et du nom

    call rpmgetkeyvalstr(pcour, "TYPE", str)

    defsolver%probe(ip)%type = cnull

    if (samestring(str, "PROBE" ))         defsolver%probe(ip)%type = probe
    if (samestring(str, "BOCO_FIELD" ))    defsolver%probe(ip)%type = boco_field
    if (samestring(str, "BOCO_INTEGRAL" )) defsolver%probe(ip)%type = boco_integral
    if (samestring(str, "RESIDUALS" ))     defsolver%probe(ip)%type = residuals
    
    ! message d'erreur a la lecture de parametres suivants

    call rpmgetkeyvalstr(pcour, "NAME", str)
    defsolver%probe(ip)%name = str

    ! -- Lecture des parametres selon le type --

    select case(defsolver%probe(ip)%type)

    case(probe)
      call erreur("Developpement","PROBE: type PROBE non implemente")

    case(boco_field, boco_integral)
      call rpmgetkeyvalstr(pcour, "BOCO", str)
      defsolver%probe(ip)%boco_name  = str
      defsolver%probe(ip)%boco_index = indexboco(defsolver, str)
      if (defsolver%probe(ip)%boco_index == inull) then
        call erreur("definition de capteurs",trim(str)//" nom de condition limite inexistant")
      endif

    case(residuals)
      call erreur("Developpement","PROBE: type RESIDUALS non implemente")

    case default
      call erreur("lecture de menu (PROBE)","type de capteur inconnu")
    endselect

    ! -- Determination de la quantite a lire

    call rpmgetkeyvalstr(pcour, "QUANTITY", str)
    defsolver%probe(ip)%quantity = quantity(str)

    if (defsolver%probe(ip)%quantity == inull) then
      call erreur("lecture de menu (PROBE)","quantite inconnue")
    endif  

    ! DEV : verification du type en fonction du solveur  

    ! DEV : procedure init_capteurs ?

    ! DEV : PARAMETRE DE STOCKAGE
  enddo

endif


endsubroutine def_capteurs

!------------------------------------------------------------------------------!
! Historique des modifications
!
! nov  2003 : creation de la routine
!------------------------------------------------------------------------------!


