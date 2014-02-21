!------------------------------------------------------------------------------!
! Procedure : def_boco_ns                 Author : J. Gressier
!                                         Date   : Novembre 2003
! Function                                Modif  : (cf historique)
!   Processing of main menu file parameters
!   Main project parameters
!
! Faults/Limitations/Varia :
!
!------------------------------------------------------------------------------!
subroutine def_boco_ns(block, type, defsolver, boco, unif)

use RPM
use TYPHMAKE
use VARCOM
use OUTPUT
use MENU_SOLVER
use MENU_BOCO
use FCT_PARSER

implicit none

! -- Inputs --
type(rpmblock), target :: block    ! RPM block with all definitions
integer(kip)           :: unif     ! uniformity of boundary condition
type(mnu_solver)       :: defsolver

! -- Outputs --
type(st_boco_ns) :: boco

! -- Inputs / Outputs --
integer(kip)           :: type     ! kind of boundary condition

! -- Internal variables --
type(rpmblock), pointer  :: pblock, pcour  ! RPM block pointer
integer(kip)             :: ib, nkey
character(len=dimrpmlig) :: str            ! intermediate RPM string
integer(kip)             :: info
integer(kip)             :: typ
type(v3d)                :: temp_direction

! -- BODY --

pblock => block

call fctset_initdependency(defsolver%fctenv)
boco%xyz_depend = .false.

select case(type)

case(bc_wall_adiab)
  call rpmgetkeyvalstr (pblock, "WALL_VELOCITY", str, "(0., 0., 0.)")
  boco%wall_velocity = v3d_of(str, info)
  if (info /= 0) call error_stop("parsing parameters: unable to read WALL_VELOCITY data")
  typ = bc_wall_flux
  boco%flux = 0._krp

case(bc_wall_isoth)
  call rpmgetkeyvalstr (pblock, "WALL_VELOCITY", str, "(0., 0., 0.)")
  boco%wall_velocity = v3d_of(str, info)
  if (info /= 0) call error_stop("parsing parameters: unable to read WALL_VELOCITY data")
  typ = bc_wall_isoth
  select case(unif)
  case(uniform)
    call rpmgetkeyvalreal(pblock, "WALL_TEMP", boco%temp_wall)
  case(nonuniform)
    boco%alloctemp = .true.
    call rpmgetkeyvalstr(pblock, "TEMP_FILE", str)
    boco%tempfile = str
  endselect

case(bc_wall_flux)
  call rpmgetkeyvalstr (pblock, "WALL_VELOCITY", str, "(0., 0., 0.)")
  boco%wall_velocity = v3d_of(str, info)
  if (info /= 0) call error_stop("parsing parameters: unable to read WALL_VELOCITY data")
  typ = bc_wall_flux
  select case(unif)
  case(uniform)
    call rpmgetkeyvalreal(pblock, "WALL_FLUX", boco%flux)
    boco%flux = - boco%flux ! convention : flux out in the algorithm
                            ! BOCO convention : flux in for user
  case(nonuniform)
    boco%allocflux = .true.
    call rpmgetkeyvalstr(pblock, "FLUX_FILE", str)
    boco%fluxfile = str
  endselect

case(bc_inlet_sub)
  typ = bc_inlet_sub
  call parsefct("PI", boco%ptot)
  if (rpm_existkey(pblock, "TI")) then
    boco%is_ttot = .true.
    call parsefct("TI", boco%ttot)
  elseif (rpm_existkey(pblock, "S")) then
    boco%is_ttot = .false.
    call parsefct("S", boco%entropy)
  else
    call error_stop("menu definition: missing either TI or S (NS boundary condition)") 
  endif
  if (rpm_existkey(pblock, "DIRECTION")) then
    call rpmgetkeyvalstr (pblock, "DIRECTION", str)
    temp_direction = v3d_of(str, info)
    if (info /= 0) call error_stop("menu definition: problem when parsing DIRECTION vector (NS initialization)") 
    call convert_to_funct(temp_direction%x, boco%dir_x, info)
    call convert_to_funct(temp_direction%y, boco%dir_y, info)
    call convert_to_funct(temp_direction%z, boco%dir_z, info)
  else
    call parsefct("DIR_X", boco%dir_x)
    call parsefct("DIR_Y", boco%dir_y)
    call parsefct("DIR_Z", boco%dir_z)
  endif

case(bc_inlet_sup)
  typ = bc_inlet_sup

  call parsefct("PI",   boco%ptot)
  call parsefct("TI",   boco%ttot)
  call parsefct("MACH", boco%mach)

  if (rpm_existkey(pblock, "DIRECTION")) then
    call rpmgetkeyvalstr (pblock, "DIRECTION", str)
    temp_direction = v3d_of(str, info)
    if (info /= 0) call error_stop("menu definition: problem when parsing DIRECTION vector (NS initialization)") 
    call convert_to_funct(temp_direction%x, boco%dir_x, info)
    call convert_to_funct(temp_direction%y, boco%dir_y, info)
    call convert_to_funct(temp_direction%z, boco%dir_z, info)
  else
    call parsefct("DIR_X", boco%dir_x)
    call parsefct("DIR_Y", boco%dir_y)
    call parsefct("DIR_Z", boco%dir_z)
  endif

case(bc_outlet_sub)
  typ = bc_outlet_sub
  call parsefct("P", boco%pstat)

case(bc_outlet_sup)
  typ = bc_outlet_sup
  ! No parameter to read

case default
  call error_stop("reading menu: unknown boundary condition type for Navier-Stokes solver")
endselect

type = typ

boco%xyz_depend = boco%xyz_depend .or. &
             fctset_needed_dependency(defsolver%fctenv, "x").or. &
             fctset_needed_dependency(defsolver%fctenv, "y").or. &
             fctset_needed_dependency(defsolver%fctenv, "z")

contains

subroutine parsefct(key, fct)
implicit none
type(st_fct_node) :: fct
character(len=*)  :: key
character(len=dimrpmlig)   :: strfct
  call rpmgetkeyvalstr(pblock, key, strfct)
  call convert_to_funct(strfct, fct, info)  
  if (info /= 0) call error_stop("menu definition: problem when parsing "//key//" (NS BC): "//trim(strfct)) 
  call print_info(10, "    . parsing "//key//"="//strfct)
  call fctset_checkdependency(defsolver%fctenv, fct)
  boco%xyz_depend = boco%xyz_depend.or.fct_xyzdependency(fct)
endsubroutine

endsubroutine def_boco_ns
!------------------------------------------------------------------------------!
! Changes history
!
! nov  2003 : creation
! june 2004 : definition and reading of boundary conditions(inlet/outlet)
! june 2005 : wall conditions
! nov  2006 : NS wall conditions with WALL_VELOCITY
! fev  2007 : English translation
! feb  2011 : symbolic funcions support added for inlet DIRECTION (A. Gardi)
!------------------------------------------------------------------------------!
