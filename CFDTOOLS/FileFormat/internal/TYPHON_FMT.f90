!------------------------------------------------------------------------------!
! MODULE : TYPHON_FMT
!
!------------------------------------------------------------------------------!

module TYPHON_FMT

use XBIN_DATA
use MESHPREC         ! Precision configuration
use USTMESH

implicit none

! -- Global Variables -------------------------------------------

integer(xbinkip),  parameter :: xty_defaultver  = 1
integer(xbinkip),  parameter :: xty_maxver      = 1

character(len=3),  parameter :: xtyext_mesh    = "tym"
character(len=3),  parameter :: xtyext_sol     = "tys"
!character(len=3),  parameter :: xtyext_mon     = "tym"

! -- DECLARATIONS -----------------------------------------------------------

! -- XBIN DATA section TYPE --

integer(xbinkpp), parameter :: xbinty_filedef = 1   ! DATA section for FILE definition
integer(xbinkpp), parameter :: xbinty_meshdef = 10  ! DATA section for FILE definition
! -- MESH definition --
integer(xbinkpp), parameter :: xbinty_nodes    = 11  ! DATA section for FILE definition
integer(xbinkpp), parameter :: xbinty_cells    = 12  ! DATA section for FILE definition
integer(xbinkpp), parameter :: xbinty_faces    = 15  ! DATA section for FILE definition
integer(xbinkpp), parameter :: xbinty_marks    = 20  ! DATA section for FILE definition
integer(xbinkpp), parameter :: xbinty_solution = 30  ! DATA section for FILE definition
integer(xbinkpp), parameter :: xbinty_scalar   = 31  ! DATA section for FILE definition
integer(xbinkpp), parameter :: xbinty_vector   = 32  ! DATA section for FILE definition

! -- Parameters --

integer(xbinkip), parameter :: xty_file_mesh    = 10
integer(xbinkip), parameter :: xty_file_sol     = 20
integer(xbinkip), parameter :: xty_file_monitor = 50

integer(xbinkip), parameter :: xty_mesh_umesh     = 10    ! mono-grid   unstructured mesh
integer(xbinkip), parameter :: xty_mesh_partumesh = 15    ! partitioned unstructured mesh

integer(xbinkip), parameter :: xty_grid_umesh     = 10    ! mono-grid   unstructured mesh
integer(xbinkip), parameter :: xty_grid_partumesh = 15    ! partitioned unstructured mesh


!------------------------------------------------------------------------------!
! ST_DEFTYPHON
!------------------------------------------------------------------------------!
type st_deftyphon
  type(st_defxbin) :: defxbin
  integer(xbinkip) :: xty_version
  integer(xbinkip) :: xty_filetype
  integer(xbinkip) :: nb_mesh      ! number of mesh parts
  integer(xbinkip) :: nb_sol       ! number of solution per mesh part
endtype st_deftyphon

contains 
!------------------------------------------------------------------------------!

!------------------------------------------------------------------------------!
! write TYPHON header definition
!------------------------------------------------------------------------------!
subroutine typhonwrite_filedef(deftyphon)
implicit none
! -- INPUTS --
type(st_deftyphon)       :: deftyphon
! -- OUTPUTS --
! -- private data --
type(st_xbindatasection) :: xbindata
! -- BODY --

!! DEV : check consistency ?

call xbin_defdatasection(xbindata, xbinty_filedef, "FILE_HEADER", &
     (/ xty_defaultver,          &    ! TYPHON internal format version
        deftyphon%xty_filetype,  &    ! type of TYPHON file (mesh, solution, ...)
        deftyphon%nb_mesh,       &    ! number of grids/meshes
        deftyphon%nb_sol         &    ! number of solution per mesh
      /) )

call xbin_writedata_nodata(deftyphon%defxbin, xbindata)

endsubroutine typhonwrite_filedef

!------------------------------------------------------------------------------!
! read TYPHON header definition
!------------------------------------------------------------------------------!
subroutine typhonread_filedef(deftyphon)
implicit none
! -- INPUTS/OUTPUTS --
type(st_deftyphon)       :: deftyphon
! -- private data --
integer                       :: info
type(st_xbindatasection)      :: xbindata
! -- BODY --

!! DEV : check consistency ?

call xbin_readdatahead(deftyphon%defxbin, xbindata)

if (xbindata%nparam >= 4) then
  deftyphon%xty_version  = xbindata%param(1)
  deftyphon%xty_filetype = xbindata%param(2)
  deftyphon%nb_mesh      = xbindata%param(3)
  deftyphon%nb_sol       = xbindata%param(4)
else
  call cfd_error("XBIN/TYPHON error: expecting parameters in TYPHON header")
endif

call xbin_skipdata(deftyphon%defxbin, xbindata)

endsubroutine typhonread_filedef

!------------------------------------------------------------------------------!
! open XBIN TYPHON
!------------------------------------------------------------------------------!
subroutine typhon_openread(iunit, filename, deftyphon)
implicit none
! -- INPUTS --
integer            :: iunit
character(len=*)   :: filename
! -- OUTPUTS --
type(st_deftyphon) :: deftyphon
! -- private data --
! -- BODY --

  call xbin_openread(iunit, filename, deftyphon%defxbin)
  call typhonread_filedef(deftyphon)
  if (deftyphon%xty_version > xty_maxver) &
    call cfd_error("XBIN/TYPHON: unable to handle this TYPHON format version number")

endsubroutine typhon_openread

!------------------------------------------------------------------------------!
! open XBIN TYPHON
!------------------------------------------------------------------------------!
subroutine typhon_openwrite(iunit, filename, deftyphon, nbmesh, nbsol)
implicit none
! -- INPUTS --
integer            :: iunit
character(len=*)   :: filename
integer            :: nbmesh
integer, optional  :: nbsol
! -- OUTPUTS --
type(st_deftyphon) :: deftyphon
! -- private data --
! -- BODY --

  if (present(nbsol)) then
    deftyphon%nb_sol = nbsol
  else
    deftyphon%nb_sol = 0
  endif
  if (deftyphon%nb_sol == 0) then
    deftyphon%xty_filetype = xty_file_mesh
  else
    deftyphon%xty_filetype = xty_file_sol
  endif
  deftyphon%nb_mesh = nbmesh   
  
  call xbin_openwrite(iunit, filename, deftyphon%defxbin)
  call typhonwrite_filedef(deftyphon)

endsubroutine typhon_openwrite



endmodule TYPHON_FMT
!------------------------------------------------------------------------------!
! Changes
!
! June 2010: created, basic TYPHON sections writing
! Apr  2011: basic TYPHON sections reading
!------------------------------------------------------------------------------!
