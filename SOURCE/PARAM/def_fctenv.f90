!------------------------------------------------------------------------------!
! Procedure : def_fctenv  
! 
! Fonction 
!   Definition of functions for zone environment
!
!------------------------------------------------------------------------------!
subroutine def_fctenv(block, defsolver)

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
type(mnu_solver) :: defsolver

! -- Internal variables --
type(rpmblock), pointer  :: pblock, pcour  ! pointeur de bloc RPM
integer                  :: i, nkey, info, nfct
character(len=dimrpmlig) :: str            ! chaine RPM intermediaire

! -- BODY --

call print_info(5,"- Definition of FCT environment")


! -- look for BLOCK:FCTENV

pblock => block
call seekrpmblock(pblock, "FCTENV", 0, pcour, nkey)

select case(nkey)
case(0)
  defsolver%fctenv%nfct = 0
  ! nothing to do - optional block
case(1)
  nfct = pcour%nblig
  defsolver%fctenv%nfct = nfct
  allocate(defsolver%fctenv%fct(nfct))
  do i = 1, nfct
    call rpmgetkey(pcour%txt(i), str)
    defsolver%fctenv%fct(i)%name = trim(str)    
    call rpmgetvalstr(pcour%txt(i), str)
    call print_info(5,"  . define "//defsolver%fctenv%fct(i)%name//"= "//trim(str))
    call string_to_funct(str, defsolver%fctenv%fct(i)%func, info)
  enddo
case default
  call error_stop("parameter parsing: too many BLOCK:FCTENV")
endselect


endsubroutine def_fctenv
!------------------------------------------------------------------------------!
! Changes history
!
! Mar  2013 : creation
!------------------------------------------------------------------------------!


