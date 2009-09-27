!------------------------------------------------------------------------------!
! Procedure : def_probe 
! Fonction
!   Definition of probes 
!
!------------------------------------------------------------------------------!
subroutine def_probe(block, isolver, defsolver)

use RPM
use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_SOLVER
use FCT_PARSER

implicit none

! -- INPUTS --
type(rpmblock), target :: block
integer                :: isolver

! -- OUTPUTS --
type(mnu_solver)                             :: defsolver

! -- Private Data --
type(rpmblock), pointer  :: pblock, pcour  ! pointeur de bloc RPM
integer                  :: nprobe         ! nombre   de probe
integer                  :: ip, nkey, info
character(len=dimrpmlig) :: str            ! chaine RPM intermediaire

! ---------------- BODY ----------------

call print_master(1, "- Probes definition")

! -- seek BLOCK:PROBE --

pblock => block
call seekrpmblock(pblock, "PROBE", 0, pcour, nprobe)
defsolver%nprobe = nprobe

if (nprobe < 1) then
  call print_master(2, "  no used-defined probe")
else

  allocate(defsolver%probe(nprobe))

  do ip = 1, nprobe

    call seekrpmblock(pblock, "PROBE", ip, pcour, nkey)

    ! -- Determination du type et du nom

    call rpmgetkeyvalstr(pcour, "TYPE", str)

    defsolver%probe(ip)%type = inull

    if (samestring(str, "PROBE" ))         defsolver%probe(ip)%type = probe_cell
    if (samestring(str, "BOCO_FIELD" ))    defsolver%probe(ip)%type = boco_field
    if (samestring(str, "BOCO_INTEGRAL" )) defsolver%probe(ip)%type = boco_integral
    if (samestring(str, "VOL_AVERAGE" ))   defsolver%probe(ip)%type = vol_average
    if (samestring(str, "VOL_MIN" ))       defsolver%probe(ip)%type = vol_min
    if (samestring(str, "VOL_MAX" ))       defsolver%probe(ip)%type = vol_max
    if (samestring(str, "RESIDUALS" ))     defsolver%probe(ip)%type = residuals
    
    ! possible error message further

    call rpmgetkeyvalstr(pcour, "NAME", str)
    defsolver%probe(ip)%name = str

    select case(defsolver%probe(ip)%type)

    case(probe_cell)
      call error_stop("Development - PROBE: type PROBE non implemente")

    case(boco_field, boco_integral)
      call rpmgetkeyvalstr(pcour, "BOCO", str)
      defsolver%probe(ip)%boco_name  = str
      defsolver%probe(ip)%boco_index = indexboco(defsolver, str)
      if (defsolver%probe(ip)%boco_index == inull) then
        call error_stop("Probe definition - unknown boundary condition name "//trim(str))
      endif

    case(vol_min, vol_max, vol_average)
      ! nothing to do

    case(residuals)
      call error_stop("Development - PROBE: type RESIDUALS non implemente")

    case default
      call error_stop("Parameter parsing (PROBE): unknown type of probe")
    endselect

    ! -- what to compute ?

    call rpmgetkeyvalstr(pcour, "QUANTITY", str)
    call convert_to_funct(str, defsolver%probe(ip)%quantity, info)  
    if (info /= 0) &
      call error_stop("Parameter parsing (PROBE) - unable to process symbolic function "//trim(str))

    defsolver%probe(ip)%write = .true.
  enddo

endif


endsubroutine def_probe

!------------------------------------------------------------------------------!
! Changes history
!
! nov  2003: created
! June 2009: add volumic probes (min, max, average)
!------------------------------------------------------------------------------!


