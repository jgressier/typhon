!------------------------------------------------------------------------------!
! Procedure : def_spat                    Auteur : J. Gressier
!                                         Date   : Novembre 2002
! Fonction                                Modif  : (cf historique)
!   Traitement des parametres du fichier menu principal
!   Parametres principaux du projet
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine def_spat(block, isolver, defspat)

use RPM
use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_NUM

implicit none

! -- Declaration des entrees --
type(rpmblock), target :: block
integer                :: isolver

! -- Declaration des sorties --
type(mnu_spat) :: defspat

! -- Declaration des variables internes --
type(rpmblock), pointer  :: pblock, pcour  ! pointeur de bloc RPM
integer                  :: nkey           ! nombre de clefs
integer                  :: i
character(len=dimrpmlig) :: str            ! chaine RPM intermediaire

! -- Debut de la procedure --

call print_info(5,"- Definition des parametres de discretisation spatiale")

! -- Initialisation --

defspat%calc_grad = .false.

! -- Recherche du BLOCK:SPAT_PARAM --

pblock => block
call seekrpmblock(pblock, "SPAT_PARAM", 0, pcour, nkey)

! DEV : est-ce que la presence du bloc est obligatoire ?
if (nkey /= 1) call erreur("lecture de menu", &
                           "bloc SPAT_PARAM inexistant ou surnumeraire")


select case(isolver)

case(solNS)

  call rpmgetkeyvalstr(pcour, "SCHEME", str, "HLLC")
  defspat%sch_hyp = inull

  if (samestring(str,"ROE"))             defspat%sch_hyp = sch_roe
  if (samestring(str,"OSHER-NO"))        defspat%sch_hyp = sch_osher_no
  if (samestring(str,"OSHER-IO"))        defspat%sch_hyp = sch_osher_io
  if (samestring(str,"OSHER"))           defspat%sch_hyp = sch_osher_no
  if (samestring(str,"HUS"))             defspat%sch_hyp = sch_efmo
  if (samestring(str,"EFMO"))            defspat%sch_hyp = sch_efmo
  if (samestring(str,"HLL"))             defspat%sch_hyp = sch_hlle
  if (samestring(str,"HLLE"))            defspat%sch_hyp = sch_hlle
  if (samestring(str,"HLLK"))            defspat%sch_hyp = sch_hllk
  if (samestring(str,"HLLC"))            defspat%sch_hyp = sch_hllc
  if (samestring(str,"HLLCK"))           defspat%sch_hyp = sch_hllck
  if (samestring(str,"STEGER-WARMING"))  defspat%sch_hyp = sch_stegwarm
  if (samestring(str,"VANLEER"))         defspat%sch_hyp = sch_vanleer
  if (samestring(str,"EFM"))             defspat%sch_hyp = sch_efm
  if (samestring(str,"KFVS"))            defspat%sch_hyp = sch_efm
  if (samestring(str,"AUSMM"))           defspat%sch_hyp = sch_ausmm

  if (defspat%sch_hyp == inull) &
    call erreur("lecture de menu","schema numerique inconnu")

  defspat%sch_dis   = inull
  defspat%calc_grad = .false.

case(solKDIF)

  ! -- Methode de calcul des flux dissipatifs --

  call rpmgetkeyvalstr(pcour, "DISSIPATIVE_FLUX", str, "FULL")
  defspat%sch_dis = inull

  if (samestring(str,"COMPACT")) defspat%sch_dis = dis_dif2
  if (samestring(str,"AVERAGE")) defspat%sch_dis = dis_avg2
  if (samestring(str,"FULL"))    defspat%sch_dis = dis_full

  if (defspat%sch_dis == inull) &
    call erreur("lecture de menu",&
                "methode de calcul DISSIPATIVE_FLUX inconnue")

  select case(defspat%sch_dis)
  case(dis_dif2)
  
  case(dis_avg2)
    defspat%calc_grad = .true.
  case(dis_full)
    defspat%calc_grad = .true.
  endselect

case(solVORTEX)

endselect


endsubroutine def_spat

!------------------------------------------------------------------------------!
! Historique des modifications
!
! nov  2002 : creation, lecture de bloc vide
! oct  2003 : choix de la methode de calcul des flux dissipatifs
! mars 2004 : traitement dans le cas solVORTEX
! july 2004 : NS solver parameters
!------------------------------------------------------------------------------!
