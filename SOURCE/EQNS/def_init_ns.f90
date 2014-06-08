!------------------------------------------------------------------------------!
! Procedure : def_init_ns                 Auteur : J. Gressier
!                                         Date   : Juillet 2004
! Fonction                                Modif  : cf historique
!   Traitement des parametres du fichier menu principal
!   Parametres d'initialisation des champs
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine def_init_ns(block, defsolver, initns)

use RPM
use VARCOM
use OUTPUT
use MENU_SOLVER
use MENU_NS
use FCT_PARSER

implicit none

! -- INPUTS --
type(rpmblock), target :: block    ! bloc RPM contenant les definitions
type(mnu_solver)       :: defsolver
integer                :: type     ! type de condition aux limites

! -- OUTPUTS --
type(st_init_ns) :: initns

! -- internal variables --
type(rpmblock), pointer  :: pblock, pcour  ! pointeur de bloc RPM
integer                  :: ib, nkey, info
character(len=dimrpmlig) :: str            ! chaine RPM intermediaire
type(v3d)                :: direction

! -- BODY --

initns%xyz_depend = .false.
pblock => block

if (rpm_existkey(pblock, "PI")) then
  initns%is_pstat = .false.
  call parsefct("PI", initns%ptot)
else
  initns%is_pstat = .true.
  call parsefct("P", initns%pstat)
endif

if (rpm_existkey(pblock, "DENSITY")) then
  initns%is_density = .true.
  call parsefct("DENSITY", initns%density)
  if (rpm_existkey(pblock, "TI")) call error_stop("NS initialization: over-defined state (TI)")
  if (rpm_existkey(pblock, "T"))  call error_stop("NS initialization: over-defined state (T)")
  initns%is_tstat = .false.
elseif (rpm_existkey(pblock, "TI")) then
  initns%is_density = .false.
  initns%is_tstat   = .false.
  call parsefct("TI", initns%ttot)
else
  initns%is_density = .false.
  initns%is_tstat   = .true.
  call parsefct("T", initns%tstat)
endif

if (rpm_existkey(pblock, "VX")) then
  initns%is_vcomponent = .true.
  initns%is_velocity   = .true.  ! same kind of definition
  call parsefct("VX", initns%vx)
  call parsefct("VY", initns%vy)
  call parsefct("VZ", initns%vz, "0")   ! default VZ
else
  initns%is_vcomponent = .false.
  if (rpm_existkey(pblock, "MACH")) then
    initns%is_velocity = .false.
    call parsefct("MACH", initns%mach)
  else
    initns%is_velocity = .true.
    call parsefct("VELOCITY", initns%velocity)
  endif

  if (rpm_existkey(pblock, "DIRECTION")) then
    call rpmgetkeyvalstr (pblock, "DIRECTION", str)
    direction = v3d_of(str, info)
    if (info /= 0) &
         call error_stop("menu definition: problem when parsing DIRECTION vector (NS initialization)") 
    call convert_to_funct(direction%x, initns%dir_x, info)
    call convert_to_funct(direction%y, initns%dir_y, info)
    call convert_to_funct(direction%z, initns%dir_z, info)
  else
    call parsefct("DIR_X", initns%dir_x)
    call parsefct("DIR_Y", initns%dir_y)
    call parsefct("DIR_Z", initns%dir_z, "0")
  endif
endif

initns%xyz_depend = initns%xyz_depend .or. &
             fctset_needed_dependency(defsolver%fctenv, "x").or. &
             fctset_needed_dependency(defsolver%fctenv, "y").or. &
             fctset_needed_dependency(defsolver%fctenv, "z")

contains
!------------------------------------------------------------------------------!
subroutine parsefct(key, fct, defkey)
implicit none
type(st_fct_node) :: fct
character(len=*)            :: key       ! key identifier
character(len=*), optional  :: defkey    ! default key
character(len=dimrpmlig)    :: strfct

  if (present(defkey)) then
    call rpmgetkeyvalstr(pblock, key, strfct, defkey)
  else
    call rpmgetkeyvalstr(pblock, key, strfct)
  endif
  call convert_to_funct(strfct, fct, info)  
  if (info /= 0) call error_stop("menu definition: problem when parsing "//key//" (NS init): "//trim(strfct)) 
  call print_info(10, "    . parsing "//key//"="//strfct)
  call fctset_checkdependency(defsolver%fctenv, fct)
  initns%xyz_depend = initns%xyz_depend.or.fct_xyzdependency(fct)
  print*,'init',initns%xyz_depend 
endsubroutine

endsubroutine def_init_ns
!------------------------------------------------------------------------------!
! Changes history
!
! juil 2004 : creation de la routine
! sept 2006 : FCT functions for Ptot, Ttot ans Mach
! Nov  2007 : FCT function for direction
!------------------------------------------------------------------------------!
