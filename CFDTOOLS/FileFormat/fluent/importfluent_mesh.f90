!------------------------------------------------------------------------------!
! Procedure : importfluent_mesh
!
! Fonction: FLUENT mesh reading
!
!------------------------------------------------------------------------------!
subroutine importfluent_mesh(defmesh, umesh)

! use IO_UNIT
use FLUENT
use MESHPARAMS
use USTMESH

implicit none

! -- INPUTS --
type(mnu_mesh)   , intent(in)  :: defmesh

! -- OUTPUTS --
type(st_ustmesh) , intent(out) :: umesh

! -- Internal variables --
! integer             :: iunit
type(st_deffluent)  :: deffluent

! -- BODY --

!------------------------------------------------------------------------
! read FLUENT mesh
!------------------------------------------------------------------------

call cfd_print("* READING FLUENT MESH: "//trim(defmesh%filename))

! --- open FLUENT file ---
! iunit = getnew_io_unit() ! (defined in deffluent)
call fluent_openread(trim(defmesh%filename), deffluent)

! --- process FLUENT file ---
call fluentread_ustmesh(deffluent, umesh)

! --- close FLUENT file ---
call fluent_close(deffluent)

endsubroutine importfluent_mesh
!------------------------------------------------------------------------------!
! Change history
!
! Aug  2014: creation (read FLUENT mesh file)
!------------------------------------------------------------------------------!
