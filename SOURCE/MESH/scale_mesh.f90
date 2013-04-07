!------------------------------------------------------------------------------!
! Procedure : scale_mesh                  Authors : J. Gressier
!                                         Created : September 2005
! Fonction
!
!------------------------------------------------------------------------------!
subroutine scale_mesh(defmesh, fctenv, mesh)

use TYPHMAKE
use PACKET
use OUTPUT
use MESHBASE
use MESHPARAMS
use FCT_EVAL
use FCT_FUNC

implicit none

! -- Inputs --
type(mnu_mesh)      :: defmesh
type(st_fctfuncset) :: fctenv

! -- Inputs/Ouputs --
type(st_mesh) :: mesh     

! -- private data --
integer, pointer      :: ista(:), iend(:) ! starting and ending index
integer               :: ib, buf, nblock  ! buffer size 
integer               :: iv
type(st_fct_env)      :: env

! -- BODY --

if (defmesh%scaling) then
  call v3d_eq_mult_t(mesh%vertex(1:mesh%nvtex,1,1), defmesh%scale)
endif

if (defmesh%morphing) then
  call morph_vertex(fctenv, mesh, defmesh%morph_x, defmesh%morph_y, defmesh%morph_z)
endif

endsubroutine scale_mesh
!------------------------------------------------------------------------------!
! Change history
!
! Sept 2005: creation, scale factor
! Apr  2011: mesh morphing
!------------------------------------------------------------------------------!
