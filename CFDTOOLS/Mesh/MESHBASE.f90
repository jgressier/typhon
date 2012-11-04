!------------------------------------------------------------------------------!
! MODULE : MESHBASE 
!
!------------------------------------------------------------------------------!
module MESHBASE

use MESHPREC   ! configuration
use VEC3D      ! 3D vectors
use MESHPARAMS

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
  integer         :: idim, jdim, kdim      ! indices max des cellules 
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
endtype st_mesh

! -- INTERFACES -------------------------------------------------------------

interface new
  module procedure new_mesh
endinterface

interface delete
  module procedure delete_mesh
endinterface


! -- Fonctions et Operateurs ------------------------------------------------


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
  
endsubroutine new_mesh


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
! Procedure : initialization of MESH structure
!------------------------------------------------------------------------------!
subroutine init_mesh(mesh)
implicit none
type(st_mesh) :: mesh
integer       :: ncell, nface, nvtex

  mesh%idim = 0
  mesh%jdim = 0
  mesh%kdim = 0
  mesh%nface = 0
  mesh%nvtex = 0
  mesh%ncell = 0
  nullify(mesh%centre)
  nullify(mesh%volume)
  nullify(mesh%iface)
  nullify(mesh%vertex)

endsubroutine init_mesh


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
  
endsubroutine delete_mesh

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


endmodule MESHBASE

!------------------------------------------------------------------------------!
! Changes history
!
! oct  2002 : creation du module
! fev  2004 : suppression de certains elements propres au structure
!             structure information de MESH
!             redefintion de new_mesh (allocation de non structure)
! Oct  2009 : transfered from TYPHON sources
!------------------------------------------------------------------------------!
