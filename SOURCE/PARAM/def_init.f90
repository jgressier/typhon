!------------------------------------------------------------------------------!
! Procedure : def_init                    Auteur : J. Gressier
!                                         Date   : Mars 2003
! Fonction                                Modif  : 
!   Traitement des parametres du fichier menu principal
!   Parametres principaux du projet
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine def_init(block, isolver, defsolver)

use RPM
use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_SOLVER

implicit none

! -- Declaration des entrees --
type(rpmblock), target :: block
integer                :: isolver

! -- Declaration des sorties --
type(mnu_solver) :: defsolver

! -- Declaration des variables internes --
type(rpmblock), pointer  :: pblock, pcour  ! pointeur de bloc RPM
integer                  :: n_init         ! nombre de definition d'initialisation
integer                  :: i, nkey
character(len=dimrpmlig) :: str            ! chaine RPM intermediaire

! -- Debut de la procedure --

call print_info(5,"- Definition de l'initialisation des champs")

! -- Recherche du BLOCK:INIT

pblock => block
call seekrpmblock(pblock, "INIT", 0, pcour, n_init)

if (n_init < 1) call erreur("lecture de menu", &
                            "Pas de definition de l'initialisation (INIT)")

defsolver%ninit = n_init
allocate(defsolver%init(n_init))

do i = 1, n_init

  call seekrpmblock(pblock, "INIT", i, pcour, nkey)

  ! -- Determination des caracteristiques communes

  call rpmgetkeyvalstr(pcour, "UNIFORMITY", str, "YES")
  if (samestring(str, "YES"))  defsolver%init(i)%unif = init_unif
  if (samestring(str, "NO"))   defsolver%init(i)%unif = init_nonunif

  ! -- Determination du type de repere

  !call rpmgetkeyvalstr(pcour, "TYPE", str)
  !defsolver%boco(ib)%typ_boco = bocotype(str)

  !if (defsolver%boco(ib)%typ_boco /= inull) then
  !  call print_info(8,"    famille "//defsolver%boco(ib)%family//": condition "//trim(str))
  !else
  !  call erreur("lecture de menu (def_init)","condition aux limites inconnue")
  !endif

  ! Traitement selon solveurs

  call print_info(10,"  Directive d'initialisation par champs uniformes")

  select case(isolver)
  case(solKDIF)
    call def_init_kdif(pcour, defsolver%init(i)%kdif, defsolver%init(i)%unif, &
                       defsolver%init(i)%profil)
  case(solVORTEX)
    call def_init_vortex(pcour, defsolver%init(i)%vortex)
  case(solNS)
    call def_init_ns(pcour, defsolver%init(i)%ns)
  case default
    call erreur("incoherence interne (def_init)","solveur inconnu")
  endselect

enddo


endsubroutine def_init

!------------------------------------------------------------------------------!
! Historique des modifications
!
! mars 2003 : creation de la routine
! juil 2004 : cas EQNS
!------------------------------------------------------------------------------!


