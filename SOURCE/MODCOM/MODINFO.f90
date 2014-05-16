!------------------------------------------------------------------------------!
! MODULE : MODINFO
! 
! Fonction
!   Definition des structures de donnees generales pour l'integration (gestion)
!
!------------------------------------------------------------------------------!

module MODINFO

use TYPHMAKE     ! Definition de la precision
use STRING

implicit none

! -- Variables globales du module -------------------------------------------

integer,           parameter :: prjdef_ver  = 1
character(len=11), parameter :: prjdef_name = "restart.prj"

! -- DECLARATIONS -----------------------------------------------------------

!------------------------------------------------------------------------------!
! Definition de la structure ST_INFO : informations generales sur la gestion du calcul
! niveau WORLD
!------------------------------------------------------------------------------!
type st_info
  logical   :: stop_integration     ! stop of integration
  integer   :: icycle               ! cycle courant
  real(krp) :: curtps               ! temps physique courant
  real(krp) :: residu_ref, cur_res  ! residu de reference et courant
  integer, pointer &
            :: headproc(:)          ! id of heading proc for each zone
endtype st_info


!------------------------------------------------------------------------------!
! Definition de la structure ST_INFOZONE : informations sur la zone
!------------------------------------------------------------------------------!
type st_infozone
  logical   :: stop_integration     ! stop of integration
  character :: time_model           ! (S)tationnaire, (I)nstationnaire, (P)eriodique
  logical   :: end_cycle            ! end of cycle
  integer   :: iter_tot             ! nombre d'iteration total
  integer   :: iter_loc             ! nombre d'iteration local dans le cycle
  integer   :: maxit                ! max number of iteration
  real(krp) :: cycle_start          ! starting time of current cycle
  real(krp) :: cycle_dt             ! duration      of cycle
  real(krp) :: cycle_time           ! local time    in the cycle
  real(krp) :: cflmax               ! maximum cfl
  real(krp) :: dtmin                ! minimum timestep
  real(krp) :: residumax            ! residu maximal admissible pour le cycle
  real(krp) :: residu_ref, cur_res  ! residu de reference (world) et courant (cycle)
  real(krp) :: residu_reforigine    ! residu de reference du premier cycle
  real(krp) :: totvolume            ! total volume on all grids
  integer   :: totndof              ! total number DOF (or cell)
  integer   :: nbproc               ! total number of communicating processors in the zone
  integer   :: headproc
  integer, pointer &
            :: proc(:)              ! list of proc. which are computing this zone
  ! -- outputs --
  integer   :: itfreq_screen = 10          
  integer   :: itfreq_file   = 1
  logical   :: mon_it, mon_time, mon_res, mon_cflmax, mon_dtmin         
endtype st_infozone

! -- INTERFACES -------------------------------------------------------------

! -- Fonctions et Operateurs ------------------------------------------------

! -- IMPLEMENTATION ---------------------------------------------------------
contains

!------------------------------------------------------------------------------!
! Procedure : write project file
!------------------------------------------------------------------------------!
subroutine writedef_prjinfo(iunit, info)
implicit none
integer       :: iunit
type(st_info) :: info

write(iunit,*) prjdef_ver,      "# version"
write(iunit,*) info%icycle,     "# cycle number"
write(iunit,*) info%curtps,     "# current time"
write(iunit,*) info%residu_ref, "# reference residual"
write(iunit,*) info%cur_res,    "# current residual"

endsubroutine

!------------------------------------------------------------------------------!
! Procedure : read project file
!------------------------------------------------------------------------------!
subroutine readdef_prjinfo(iunit, info, ver)
implicit none
integer       :: iunit, ver
type(st_info) :: info

read(iunit,*) ver
select case(ver)
case(1)
  read(iunit,*) info%icycle
  read(iunit,*) info%curtps
  read(iunit,*) info%residu_ref
  read(iunit,*) info%cur_res
case default
  print*,'unknown version'//prjdef_name
end select

endsubroutine

!------------------------------------------------------------------------------!
! Procedure : write zone info
!------------------------------------------------------------------------------!
subroutine writedef_zoneinfo(iunit, infozone)
implicit none
integer           :: iunit
type(st_infozone) :: infozone

write(iunit, *) "# ZONE information"
write(iunit, *) infozone%time_model,        "# steady, unsteady, etc"
write(iunit, *) infozone%iter_tot,          "# total number of iterations"
write(iunit, *) infozone%residu_ref,        "# reference residual" 
write(iunit, *) infozone%residu_reforigine, "# very first reference residual"

endsubroutine

!------------------------------------------------------------------------------!
! Procedure : read zone info
!------------------------------------------------------------------------------!
subroutine readdef_zoneinfo(iunit, infozone, ver)
implicit none
integer           :: iunit, ver
type(st_infozone) :: infozone

select case(ver)
case(1)
  read(iunit, *) 
  read(iunit, *) infozone%time_model
  read(iunit, *) infozone%iter_tot
  read(iunit, *) infozone%residu_ref
  read(iunit, *) infozone%residu_reforigine
case default
  print*,'unknown version of '//prjdef_name
end select

endsubroutine

!------------------------------------------------------------------------------!
! Procedure : moninfo_header
!------------------------------------------------------------------------------!
subroutine moninfo_header(str, zinfo)
implicit none
character(len=*)  :: str
type(st_infozone) :: zinfo
integer           :: pos

str = ''
pos = 1
if (zinfo%mon_it)     call addstr(str, '     it',      pos)
if (zinfo%mon_time)   call addstr(str, '     time   ', pos)
if (zinfo%mon_res)    call addstr(str, '  residuals ', pos)
if (zinfo%mon_cflmax) call addstr(str, ' cfl_max',     pos)
if (zinfo%mon_dtmin)  call addstr(str, '    dt_min  ', pos)

endsubroutine

!------------------------------------------------------------------------------!
! Procedure : moninfo_monitor
!------------------------------------------------------------------------------!
subroutine moninfo_monitor(str, zinfo)
implicit none
character(len=*)  :: str
type(st_infozone) :: zinfo
integer           :: pos

str = ''
pos = 1
if (zinfo%mon_it)     call addstr(str, strofr(zinfo%iter_loc, 7),       pos)
if (zinfo%mon_time)   call addstr(str, strofe(zinfo%cycle_time, 12, 4),     pos)
if (zinfo%mon_res)    call addstr(str, strofe(log10(zinfo%cur_res), 12, 4), pos)
if (zinfo%mon_cflmax) call addstr(str, stroff(zinfo%cflmax, 8, 2), pos)
if (zinfo%mon_dtmin)  call addstr(str, strofe(zinfo%dtmin, 12, 4), pos)

endsubroutine



endmodule MODINFO

!------------------------------------------------------------------------------!
! Change history
!
! mars 2003 : creation du module
! sept 2003 : informations specifiques pour l'integration d'un cycle
! oct 2003  : ajout de residu_ref_origine
!------------------------------------------------------------------------------!
