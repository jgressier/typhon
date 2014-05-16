!------------------------------------------------------------------------------!
!> @brief geometrical data of a mesh
!! - centers and volumes of cells
!! - normal vectors, centers and surface of faces
!! - position of vertices
!------------------------------------------------------------------------------!
module MESHBASE

use TYPHMAKE
use MPICOMM
use VEC3D      ! 3D vectors
use MESHPARAMS
use PACKET
use FCT_EVAL
use FCT_FUNC

implicit none

! -- Variables globales du module -------------------------------------------

! -- DECLARATIONS -----------------------------------------------------------

!------------------------------------------------------------------------------!
! Definition de la structure INFO_MESH
!------------------------------------------------------------------------------!
type info_mesh
  type(v3d) :: min, max            ! coordonnees min et max des vertex
  type(v3d) :: center              ! mesh gravity center
  real(krp) :: minscale, maxscale  ! echelle de longueur (min et max)
  real(krp) :: minvol,   maxvol    ! cell volumes        (min et max)
  real(krp) :: totvol              ! total volume
endtype

!------------------------------------------------------------------------------!
! Definition de la structure ST_FACE : face de cellule
!------------------------------------------------------------------------------!
type st_face
  type(v3d)   :: normale        ! normale a la face, orientee indice croissant
  type(v3d)   :: centre         ! centre de face
  real(krp)   :: surface        ! valeur de la surface de la face
endtype st_face

!------------------------------------------------------------------------------!
! Definition de la structure ST_MESH : liste de vertex, faces, centres, volumes
!------------------------------------------------------------------------------!
type st_mesh
  type(info_mesh) :: info
  integer         :: nvtex                 ! nombre de sommets
  integer         :: nface
  integer         :: ncell                 ! nombre de faces et cellules totales
  type(v3d), dimension(:,:,:), pointer &  ! coordonnees des sommets et centres
                  :: vertex, centre        ! de cellules (i,j,k)
  type(v3d), dimension(:,:,:), allocatable :: vertex_orig ! MRF addition: table of original vertex positions
  type(st_face), dimension(:,:,:), pointer &
                  :: iface !, jface, kface   ! tableaux de faces
  real(krp), dimension(:,:,:), pointer &
                  :: volume                ! volume des cellules
  real(krp), dimension(:,:,:), pointer &
                  :: metricsvm                ! transformation of a triangular cell (SV) to a standard triangle
contains
  procedure, pass :: send   => mesh_send
  procedure, pass :: recv   => mesh_recv
endtype st_mesh

! -- INTERFACES -------------------------------------------------------------

interface new
  module procedure new_mesh
end interface

interface delete
  module procedure delete_mesh
end interface


! -- Fonctions et Operateurs ------------------------------------------------

private :: mesh_send, mesh_recv

! -- IMPLEMENTATION ---------------------------------------------------------
contains


!------------------------------------------------------------------------------!
! Procedure : initialization and allocation of MESH structure
!------------------------------------------------------------------------------!
subroutine new_mesh(mesh, ncell, nface, nvtex)
implicit none
type(st_mesh) :: mesh
integer       :: ncell, nface, nvtex

  call init_mesh(mesh)
  call alloc_mesh(mesh, ncell, nface, nvtex)
  
end subroutine new_mesh


!------------------------------------------------------------------------------!
! Procedure : (partial) allocation of MESH structure
!------------------------------------------------------------------------------!
subroutine alloc_mesh(mesh, ncell, nface, nvtex)
implicit none
type(st_mesh) :: mesh
integer       :: ncell, nface, nvtex

  if (ncell /= 0) then
    mesh%ncell = ncell
    allocate(mesh%centre(1:ncell, 1,1))
    allocate(mesh%volume(1:ncell, 1,1))
  endif
  if (nface /= 0) then
    mesh%nface = nface
    allocate(mesh% iface(1:nface, 1,1))
  endif
  if (nvtex /= 0) then
    mesh%nvtex = nvtex
    allocate(mesh%vertex(1:nvtex, 1,1))
  endif

endsubroutine alloc_mesh

!------------------------------------------------------------------------------!
! Procedure : (partial) allocation of metricsvm MESH structure
! transformation of a triangular cell (SV) to a standard triangle
!------------------------------------------------------------------------------!
subroutine alloc_mesh_metricsvm(mesh, ncoeff)
implicit none
type(st_mesh) :: mesh
integer       :: ncoeff

  if (ncoeff /= 0) then
    allocate(mesh%metricsvm(1:ncoeff, 1,1))
  endif

endsubroutine alloc_mesh_metricsvm
!------------------------------------------------------------------------------!
! Procedure : initialization of MESH structure
!------------------------------------------------------------------------------!
subroutine init_mesh(mesh)
implicit none
type(st_mesh) :: mesh
integer       :: ncell, nface, nvtex

  mesh%nface = 0
  mesh%nvtex = 0
  mesh%ncell = 0
  nullify(mesh%centre)
  nullify(mesh%volume)
  nullify(mesh%iface)
  nullify(mesh%vertex)
end subroutine init_mesh


!------------------------------------------------------------------------------!
! Procedure : desallocation d'une structure MESH
!------------------------------------------------------------------------------!
subroutine delete_mesh(mesh)
implicit none
type(st_mesh) :: mesh

  if (associated(mesh%centre)) deallocate(mesh%centre)
  if (associated(mesh%volume)) deallocate(mesh%volume)
  if (associated(mesh%iface))  deallocate(mesh%iface) 
  if (associated(mesh%vertex)) deallocate(mesh%vertex)
end subroutine delete_mesh

!---------------------------------------------------------------------------------------!
!> @brief send grid information 
!---------------------------------------------------------------------------------------!
subroutine mesh_send(this, iproc, tag)
implicit none
class(st_mesh) :: this
integer(kip)       :: iproc, array(3)
integer(kmpi)      :: tag
  ! -- mesh info --
  array(1:3) = (/ this%nvtex, this%nface, this%ncell /) 
  call mpi_isend_int(array, 4, iproc, tag, wait=.true.)
  ! -- nodes --
  call mpi_isend_int(array, 4, iproc, tag, wait=.true.)

  ! -- centers --
  ! -- volumes --
  ! -- faces --
end subroutine

!---------------------------------------------------------------------------------------!
!> @brief receive grid information
!---------------------------------------------------------------------------------------!
subroutine mesh_recv(this, iproc, tag)
implicit none
class(st_mesh) :: this
integer(kip)       :: iproc, array(3)
integer(kmpi)      :: tag
  call mpi_isend_int(array, 3, iproc, tag, wait=.true.)
  this%nvtex = array(1)
  this%nface = array(2)
  this%ncell = array(3)
end subroutine

!------------------------------------------------------------------------------!
! Procedure : desallocation d'une structure MESH
!------------------------------------------------------------------------------!
subroutine calc_mesh_info(mesh)
implicit none
type(st_mesh) :: mesh
integer(kip)  :: i

  ! -- min & max cell volume --

  mesh%info%minvol = minval(mesh%volume(1:mesh%ncell,1,1))
  mesh%info%maxvol = maxval(mesh%volume(1:mesh%ncell,1,1))

  ! -- total volume & volume gravity center --

  mesh%info%totvol = 0._krp
  mesh%info%center = v3d_zero
  do i = 1, mesh%ncell
    mesh%info%center = mesh%info%center + mesh%volume(i,1,1)*mesh%centre(i,1,1)
    mesh%info%totvol = mesh%info%totvol + mesh%volume(i,1,1)
  enddo
  mesh%info%center = mesh%info%center / mesh%info%totvol

endsubroutine calc_mesh_info

!------------------------------------------------------------------------------!
! Procedure : morph_vertex 
!------------------------------------------------------------------------------!
subroutine morph_vertex(fctenv, mesh, morphx, morphy, morphz)
implicit none

! -- Inputs --
type(st_fctfuncset) :: fctenv
type(st_fct_node)   :: morphx, morphy, morphz
! -- Inputs/Ouputs --
type(st_mesh) :: mesh     
! -- private data --
integer, pointer                 :: ista(:), iend(:) ! starting and ending index
integer                          :: ib, buf, nblock  ! buffer size 
real(krp), dimension(fct_buffer) :: x, y, z
integer                          :: iv
type(st_fct_env)                 :: env

! -- BODY --

  call fctset_initdependency(fctenv)
  call fctset_checkdependency(fctenv, morphx)
  call fctset_checkdependency(fctenv, morphy)
  call fctset_checkdependency(fctenv, morphz)

  call new_buf_index(mesh%nvtex, fct_buffer, nblock, ista, iend)

  !$OMP PARALLEL & 
  !$OMP private(iv, env, x, y, z, buf) &
  !$OMP shared(ista, iend, nblock)
  
  call new_fct_env(env)      ! temporary environment from FCT_EVAL

  !$OMP DO
  block: do ib = 1, nblock

    buf = iend(ib)-ista(ib)+1

    do iv = ista(ib), iend(ib)
      x(iv-ista(ib)+1) = mesh%vertex(iv,1,1)%x
      y(iv-ista(ib)+1) = mesh%vertex(iv,1,1)%y
      z(iv-ista(ib)+1) = mesh%vertex(iv,1,1)%z
    enddo
    call fct_env_set_realarray(env, "x", x(1:buf))
    call fct_env_set_realarray(env, "y", y(1:buf))
    call fct_env_set_realarray(env, "z", z(1:buf))
    call fctset_compute_neededenv(fctenv, env)

    call fct_eval_realarray(env, morphx, x)
    call fct_eval_realarray(env, morphy, y)
    call fct_eval_realarray(env, morphz, z)

    do iv = ista(ib), iend(ib)
      mesh%vertex(iv,1,1)%x = x(iv-ista(ib)+1)
      mesh%vertex(iv,1,1)%y = y(iv-ista(ib)+1)
      mesh%vertex(iv,1,1)%z = z(iv-ista(ib)+1)
    enddo

  enddo block
  
  !$OMP END DO
  call delete_fct_env(env)      ! temporary environment from FCT_EVAL
  !$OMP END PARALLEL 

  deallocate(ista, iend)

endsubroutine morph_vertex

endmodule MESHBASE
!------------------------------------------------------------------------------!
! Changes history
!
! oct  2002 : creation du module
! fev  2004 : suppression de certains elements propres au structure
!             structure information de MESH
!             redefintion de new_mesh (allocation de non structure)
! Oct  2009 : transfered from TYPHON sources
! Apr  2013 : internal scaling mesh routine
!------------------------------------------------------------------------------!
