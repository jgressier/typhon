!------------------------------------------------------------------------------!
! Procedure : def_init                                Authors : J. Gressier
!                                                     Created : March 2003
! Fonction 
!   Traitement des parametres du fichier menu principal
!   Parametres principaux du projet
!
!------------------------------------------------------------------------------!
subroutine def_init(block, isolver, defsolver)

use RPM
use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_SOLVER

implicit none

! -- INPUTS --
type(rpmblock), target :: block
integer                :: isolver

! -- OUTPUTS --
type(mnu_solver) :: defsolver

! -- Internal variables --
type(rpmblock), pointer  :: pblock, pcour  ! pointeur de bloc RPM
integer                  :: n_init         ! nombre de definition d'initialisation
integer                  :: i, nkey
character(len=dimrpmlig) :: str            ! chaine RPM intermediaire

! -- BODY --

call print_info(5,"- Definition of Initial Conditions")

! -- Recherche du BLOCK:INIT

pblock => block
call seekrpmblock(pblock, "INIT", 0, pcour, n_init)

if (n_init < 1) call erreur("Parameter parsing", &
                            "no block definition found (BLOCK:INIT)")

defsolver%ninit = n_init
allocate(defsolver%init(n_init))

do i = 1, n_init

  call seekrpmblock(pblock, "INIT", i, pcour, nkey)

  ! -- Determination des caracteristiques communes

  call rpmgetkeyvalstr(pcour, "UNIFORMITY", str, "YES")
  if (samestring(str, "YES"))  defsolver%init(i)%unif = init_unif
  if (samestring(str, "NO"))   defsolver%init(i)%unif = init_nonunif

  call rpmgetkeyvalstr(pcour, "TYPE", str, "DEFINITION")
  if (samestring(str, "DEFINITION"))  defsolver%init(i)%type = init_def
  if (samestring(str, "CGNS"))        defsolver%init(i)%type = init_cgns
  if (samestring(str, "FILE"))        defsolver%init(i)%type = init_file
  if (samestring(str, "UDF"))         defsolver%init(i)%type = init_udf

  ! -- Determination du type de repere

  !call rpmgetkeyvalstr(pcour, "TYPE", str)
  !defsolver%boco(ib)%typ_boco = bocotype(str)

  !if (defsolver%boco(ib)%typ_boco /= inull) then
  !  call print_info(8,"    famille "//defsolver%boco(ib)%family//": condition "//trim(str))
  !else
  !  call erreur("lecture de menu (def_init)","condition aux limites inconnue")
  !endif

  select case(defsolver%init(i)%type)

  case(init_def)    ! USUAL DEFINITION : parsing parameters according to solver
    call print_info(10,"  Parsing parameters for uniform initial conditions")
    select case(isolver)
    case(solKDIF)
      call def_init_kdif(pcour, defsolver%init(i)%kdif, defsolver%init(i)%unif)
    case(solVORTEX)
      call def_init_vortex(pcour, defsolver%init(i)%vortex)
    case(solNS)
      call def_init_ns(pcour, defsolver%init(i)%ns)
    case default
      call erreur("internal error (def_init)","unknown solver")
    endselect

    ! PROVISOIRE
    defsolver%init(i)%file = "bidon"

  case(init_file)
    call rpmgetkeyvalstr(pcour, "INIT_FILE", str) 
    defsolver%init(i)%file = trim(str)

  case(init_udf, init_cgns)
    ! nothing to do

  case default
    call erreur("parameter reading (def_init)","unknown parameter (TYPE)")
  endselect

enddo


endsubroutine def_init

!------------------------------------------------------------------------------!
! Changes history
!
! mars 2003 : creation de la routine
! juil 2004 : cas EQNS
! oct  2005 : udf initialization
!------------------------------------------------------------------------------!


