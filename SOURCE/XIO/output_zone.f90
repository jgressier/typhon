!------------------------------------------------------------------------------!
! Procedure : output_zone
!
! Function
!   Write the field of each zone
!
!------------------------------------------------------------------------------!
subroutine output_zone(defio, zone, winfo)

use TYPHMAKE
use OUTPUT
use VARCOM
use DEFZONE
use MENU_GEN
USE MODINFO

implicit none

! -- INPUTS --
type(mnu_output)             :: defio     ! output parameter
type(st_zone)                :: zone      ! zone
type(st_info)                :: winfo     ! world info

! -- OUPUTS --

! -- Internal variables --
type(st_grid), pointer  :: pgrid
integer  	            :: isim, nsim
character(len=longname) :: suffix
character(len=shortname):: int_str
! -- BODY --

nsim = zone%defsolver%nsim
!------------------------------------------------------------------
! prepare DATA to save
!------------------------------------------------------------------

! -- recompute PRIMITIVE data (for all OUTPUTS) --

pgrid => zone%gridlist%first
do while (associated(pgrid))
  call calc_varprim(zone%defsolver, pgrid%info%field_loc)     ! calcul des var. primitives
  pgrid => pgrid%next
enddo

! -- recompute BOCO data (needed for BOCO outputs AND Tecplot output) --

pgrid => zone%gridlist%first
do while (associated(pgrid))
  call calcboco_ust(winfo%curtps, zone%defsolver, zone%defsolver%defspat, pgrid)
  pgrid => pgrid%next
enddo

select case(defio%dataset)
case(dataset_node)
case(dataset_cell)
case(dataset_bococell)
  if (defio%format .ne. fmt_TECPLOT) then
    call error_stop("TYPE=BOCO/BOCOCELL output implemented only for FORMAT=TECPLOT")
  endif
!case(dataset_boconode)
case default
  call error_stop("internal error(output_zone): unknown dataset parameter")
endselect

! --- set filename suffix ---

do isim = 1, nsim ! ----- loop on all simulations -----

  if (nsim > 1) then
    suffix =  ".sim" // strof_full_int(isim,3)
  else
    suffix = ""
  endif

  if (mpi_run) suffix = ".p"//strof_full_int(myprocid,3)

  ! cycle suffix 
  if (defio%index == 0) then
    ! nothing to do
  else
    suffix = trim(suffix)//"."//strof_full_int(defio%index, 4)
  endif

  defio%filename = trim(defio%basename)//trim(suffix)
  
select case(defio%format)
case(fmt_TYPHON)
  call print_info(2,"* write TYPHON file: " // trim(defio%filename))
case(fmt_VTK)
  call print_info(2,"* write VTK file: " // trim(defio%filename))
case(fmt_VTKBIN)
  call print_info(2,"* write VTK Binary file: " // trim(defio%filename))
case(fmt_TECPLOT)

  call print_info(2,"* write TECPLOT file: " // trim(defio%filename))
  call output_tecplot(trim(defio%filename), defio, zone)

case(fmt_CGNS, fmt_CGNS_linked)
#ifdef CGNS
  call print_info(2,"* write CGNS file: " // trim(defio%filename))
  ! write sol in GENERAL OUTPUT
#else /*CGNS*/
  call error_stop("Internal error (output_zone): CGNS format was not activated at configure time")
#endif/*CGNS*/

case default
  call error_stop("Internal error (output_zone): unknown output format parameter")
endselect

!------------------------------------------------------------------
! General output
!------------------------------------------------------------------

select case(defio%format)
case(fmt_TECPLOT)
  ! already saved
case(fmt_CGNS, fmt_CGNS_linked, fmt_VTK, fmt_VTKBIN, fmt_TYPHON)
  call outputzone_open   (defio, zone)
  call outputzone_ustmesh(defio, zone)
  call outputzone_sol    (defio, zone, isim, nsim)
  call outputzone_close  (defio, zone)
case default
  call error_stop("Internal error (output_zone): unknown output format parameter")
endselect

enddo

endsubroutine output_zone
!------------------------------------------------------------------------------!
! Changes history
!
! May  2008: creation from output_result
! JuLy 2009: restructuration, general output routines
! Apr  2011: TYPHON format output
!------------------------------------------------------------------------------!
