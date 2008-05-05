!------------------------------------------------------------------------------!
! Procedure : def_inverse
!
! Fonction
!   INVERSE SOLVER parameters (in BLOCK:INVERSE)
!------------------------------------------------------------------------------!
subroutine def_inverse(prj, block, definverse)

use VARCOM
use RPM
use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_GEN
use MENU_INVERSE

implicit none

! -- INPUTS --
type(mnu_project)      :: prj
type(rpmblock), target :: block
integer                :: solver

! -- OUTPUTS --
type(mnu_inv) :: definverse

! -- Internal variables --
type(rpmblock), pointer  :: pblock, pcour  ! pointeur de bloc RPM
integer                  :: nkey           ! nombre de clefs
integer                  :: i
character(len=dimrpmlig) :: str            ! chaine RPM intermediaire

! -- BODY --

call print_info(5,"- Definition of Inverse Solver parameters")

! -- Recherche du BLOCK:INVERSE

pblock => block
call seekrpmblock(pblock, "INVERSE", 0, pcour, nkey)

if (nkey /= 1) call erreur("parameters parsing", &
                           "bloc INVERSE inexistant ou surnumeraire")

call rpmgetkeyvalint(pcour,  "NCYCLE_PROSPECT", definverse%ncyc_futur)
call rpmgetkeyvalint(pcour,  "NCYCLE_SENSI",    definverse%ncyc_sensi, huge(definverse%ncyc_sensi))
call rpmgetkeyvalreal(pcour, "REF_FLUX",        definverse%ref_flux,  1._krp)

call rpmgetkeyvalstr(pcour, "BOCO_UNKNOWN", definverse%bc_unknown)
call rpmgetkeyvalstr(pcour, "BOCO_MEASURE", definverse%bc_tmes)

call rpmgetkeyvalstr(pcour, "FILE_MODES", definverse%mode_file, "modes.def")
call rpmgetkeyvalstr(pcour, "FILE_TMES",  definverse%tmes_file, "tmes.dat")

endsubroutine def_inverse
!------------------------------------------------------------------------------!
! Changes History
!
! Apr  2008: Created
!------------------------------------------------------------------------------!
