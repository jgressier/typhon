!------------------------------------------------------------------------------!
! Procedure : def_boco_kdif               Author : J. Gressier
!                                         Date   : Mars 2003
! Function                                Modif  : 
!   Processing of main menu file parameters 
!   Main project parameters
!
! Faults/Limitations/Varia :
!
!------------------------------------------------------------------------------!
subroutine def_boco_kdif(block, type, boco, unif)

use RPM
use TYPHMAKE
use VARCOM
use OUTPUT
use MENU_KDIF
use MENU_BOCO
use FCT_PARSER

implicit none

! -- Inputs --
type(rpmblock), target :: block    ! RPM block with all definitions
integer(kip)           :: unif     ! uniformity of boundary condition

! -- Outputs --
type(st_boco_kdif) :: boco

! -- Inputs / Outputs --
integer(kip)           :: type     ! kind of boundary condition

! -- Internal variables --
type(rpmblock), pointer  :: pblock, pcour  ! RPM block pointer
integer(kip)             :: ib, nkey,i, info
character(len=dimrpmlig) :: str            ! intermediate RPM string 
integer(kip)             :: typ

! -- BODY --

pblock => block

select case(type)

case(bc_wall_adiab)
  typ = bc_wall_flux
  str = "0."
  call string_to_funct(str, boco%wall_flux, info)

case(bc_wall_isoth)
  typ = bc_wall_isoth
  select case(unif)
  
  case(uniform)
    call rpmgetkeyvalstr(pblock, "WALL_TEMP", str)
    print*," parsing WALL_TEMP = "//trim(str)
    call string_to_funct(str, boco%wall_temp, info)

  case(nonuniform)
    boco%alloctemp = .true.
    call rpmgetkeyvalstr(pblock, "TEMP_FILE", str)
    boco%tempfile = str

  endselect

case(bc_wall_flux)
  typ = bc_wall_flux  
  select case(unif)
  
  case(uniform)
    call rpmgetkeyvalstr(pblock, "WALL_FLUX", str)
    print*," parsing WALL_FLUX = "//trim(str)
    call string_to_funct(str, boco%wall_flux, info)

  case(nonuniform)
    boco%allocflux = .true.
    call rpmgetkeyvalstr(pblock, "FLUX_FILE", str)
    boco%fluxfile = str

  endselect

case(bc_wall_hconv)
  typ = bc_wall_hconv  
  select case(unif)
  
  case(uniform)
    call rpmgetkeyvalstr(pblock, "H", str)
    print*," parsing         H = "//trim(str)
    call string_to_funct(str, boco%h_conv, info)
    !boco%h_conv = - boco%h_conv ! convention : flux out in the algorithm
                                 ! BOCO : convention flux in for user
    call rpmgetkeyvalreal(pblock, "T_CONV", boco%temp_conv)

  case(nonuniform)
    boco%allochconv = .true.
    call rpmgetkeyvalstr(pblock, "H_FILE", str)
    boco%hfile = str
    call rpmgetkeyvalstr(pblock, "TCONV_FILE", str)
    boco%tconvfile = str

  endselect

case(bc_wall_hgen)
  typ = bc_wall_hgen 
  select case(unif)
  
  case(uniform)
    call rpmgetkeyvalstr(pblock, "H", str)
    print*," parsing         H = "//trim(str)
    call string_to_funct(str, boco%h_conv, info)
    call rpmgetkeyvalreal(pblock, "T_CONV", boco%temp_conv)
    call rpmgetkeyvalstr(pblock, "WALL_FLUX", str)
    print*," parsing WALL_FLUX = "//trim(str)
    call string_to_funct(str, boco%wall_flux, info)


  case(nonuniform)
    boco%allochconv = .true.
    call rpmgetkeyvalstr(pblock, "H_FILE", str)
    boco%hfile = str
    call rpmgetkeyvalstr(pblock, "TCONV_FILE", str)
    boco%tconvfile = str
    boco%allocflux = .true.
    call rpmgetkeyvalstr(pblock, "FLUX_FILE", str)
    boco%fluxfile = str

  endselect

case default
  call erreur("Lecture de menu","type de conditions aux limites non reconnu&
              & pour le solveur de conduction")
endselect

type = typ

! -- read radiating parameters --

boco%radiating = -1_kpp
call rpmgetkeyvalstr(pblock, "RADIATING", str, "NONE")
if (samestring(str, "NONE"))    boco%radiating = rad_none
if (samestring(str, "SIMPLE"))  boco%radiating = rad_direct
if (samestring(str, "DIRECT"))  boco%radiating = rad_direct
if (samestring(str, "COUPLED")) boco%radiating = rad_coupled
if (boco%radiating == -1_kpp) then
  call erreur("parameter reading", "unknown option for RADIATING keyword")
endif

call rpmgetkeyvalreal(pblock, "EMMISSIVITY", boco%emmissivity, 1._krp)
call rpmgetkeyvalreal(pblock, "RAD_TINF",    boco%rad_Tinf,    0._krp)


endsubroutine def_boco_kdif


!------------------------------------------------------------------------------!
! Changes history
!
! mars 2003 : creation
! june 2004 : Flux, and Convection (mixed) boundary conditions
! apr  2005 : radiating boundary conditions
!------------------------------------------------------------------------------!


