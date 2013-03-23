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

  call fctset_initdependency(fctenv)
  call fctset_checkdependency(fctenv, defmesh%morph_x)
  call fctset_checkdependency(fctenv, defmesh%morph_y)
  call fctset_checkdependency(fctenv, defmesh%morph_z)

  call new_buf_index(mesh%nvtex, cell_buffer, nblock, ista, iend)

  !$OMP PARALLEL private(iv, buf, env) shared(ista, iend, nblock)
  call new_fct_env(env)      ! temporary environment from FCT_EVAL

  !$OMP DO 
  do ib = 1, nblock
    buf = iend(ib)-ista(ib)+1
    do iv = ista(ib), iend(ib)
      call fct_env_set_real(env, "X", mesh%vertex(iv,1,1)%x)
      call fct_env_set_real(env, "Y", mesh%vertex(iv,1,1)%y)
      call fct_env_set_real(env, "Z", mesh%vertex(iv,1,1)%z) 
      call fctset_compute_neededenv(fctenv, env)
      call fct_eval_real(env, defmesh%morph_x, mesh%vertex(iv,1,1)%x)
      call fct_eval_real(env, defmesh%morph_y, mesh%vertex(iv,1,1)%y)
      call fct_eval_real(env, defmesh%morph_z, mesh%vertex(iv,1,1)%z)
    enddo
  enddo
  !$OMP END DO
  call delete_fct_env(env)      ! temporary environment from FCT_EVAL
  !$OMP END PARALLEL 
  deallocate(ista, iend)
endif

endsubroutine scale_mesh
!------------------------------------------------------------------------------!
! Change history
!
! Sept 2005: creation, scale factor
! Apr  2011: mesh morphing
!------------------------------------------------------------------------------!
