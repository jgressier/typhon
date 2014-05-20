!------------------------------------------------------------------------------!
<<<<<<< HEAD
!> @brief definition of unstructured mesh
!! connectivity
!! - cellvtex, facecell, facevtex
!! - include mesh geometry from module MESHBASE
!! - boco tags on boundaring faces
=======
! MODULE : USTMESH 
!
!> @brief Grid geometry and connectivity for unstructured mesh
>>>>>>> origin/spectral
!------------------------------------------------------------------------------!
module USTMESH

use MPICOMM
use MESHBASE      ! geometrical basic elements
use MESHPARAMS
use CONNECTIVITY  ! lists & connectivity 
use ELEMVTEX      ! Cell (vtex) definition
use IOCFD
use DEF_USTBOCO

implicit none


! -- DECLARATIONS -----------------------------------------------------------


!------------------------------------------------------------------------------!
!> @struct st_ustmesh
!> @brief derived type for unstructured mesh geometry and connectity
!------------------------------------------------------------------------------!
! Organization of face arrays:
!   SVM internal faces (1..nface_svmint)
! les elements limites.

type st_ustmesh
  integer(kip)          :: id                    !> domain id
  integer(kip)          :: level                 !> multigrid level
  integer(kpp)          :: elemdim               !> mesh dimension
  integer(kip)          :: nvtex, nface, ncell   !> number of vertices, faces and cells
  integer(kip)          :: nface_int, ncell_int  !> number of internal faces and cells
  integer(kip)          :: nface_lim, ncell_lim  !> number of boundering faces and cells
  type(st_mesh)         :: mesh                  !> mesh (geometrical data)
  type(st_connect)      :: facevtex, &           !> connectivite face   -> sommets   par type
                           facecell              !> connectivite face   -> cellules  par type
                                                 !> SUPPOSED TO INDEX LOWER INDEX CELL FIRST
  type(st_genconnect)   :: colors                !> independent set of faces
  type(st_genconnect)   :: vtexface              !> VTEX-FACE connectivity (only temporary)
  type(st_genelemvtex)  :: cellvtex              !> CELL-VTEX connectivity
  
  integer               :: nboco                 !> number of boundary conditions
  type(st_ustboco), dimension(:), pointer &
                        :: boco                  !> liste des conditions aux limites
  type(st_connect)      :: face_Ltag, face_Rtag  !> define Riemann face as a local Gauss pt index
  ! --- specific SVM structure ---
  integer(kip)          :: nface_intsvm          !> number of internal SVM faces
contains
  procedure, pass :: send   => ustmesh_send
  procedure, pass :: recv   => ustmesh_recv
endtype st_ustmesh



! -- INTERFACES -------------------------------------------------------------

interface new
  module procedure new_ustmesh
endinterface

interface delete
  module procedure delete_ustmesh
endinterface

private ustmesh_send, ustmesh_recv

! -- Fonctions et Operateurs ------------------------------------------------



! -- IMPLEMENTATION ---------------------------------------------------------
contains

!------------------------------------------------------------------------------!
! Procedure : allocation d'une structure USTMESH
!------------------------------------------------------------------------------!
subroutine new_ustmesh(umesh, ncell, nface, nvtex)
implicit none
type(st_ustmesh) :: umesh
integer       :: ncell, nface, nvtex
  print*,"!!! pas d'allocation dans new_ustmesh !!!"
  stop
end subroutine new_ustmesh

!------------------------------------------------------------------------------!
! Procedure : Initialization of USTMESH structure
!------------------------------------------------------------------------------!
subroutine init_ustmesh(umesh, id)
implicit none
type(st_ustmesh) :: umesh
integer(kpp)     :: geotyp
integer(kip)     :: id

  umesh%id     = id
  !umesh%geotyp = 0 ! geotyp
  umesh%elemdim = 0
  umesh%level  = 0
  umesh%nvtex  = 0
  umesh%ncell  = 0 ; umesh%ncell_int = 0 ; umesh%ncell_lim = 0
  umesh%nface  = 0 ; umesh%nface_int = 0 ; umesh%nface_lim = 0 ; umesh%nface_intsvm = 0 
  umesh%nboco  = 0
  call new_genelemvtex(umesh%cellvtex, 0)   ! initialization
  nullify(umesh%face_Ltag%fils)
  nullify(umesh%face_Rtag%fils)
  umesh%vtexface%nbnodes = 0

  call init_mesh(umesh%mesh)  ! nullify pointers

endsubroutine init_ustmesh


!------------------------------------------------------------------------------!
! Procedure : some checkings on ustmesh
!------------------------------------------------------------------------------!
subroutine check_ustmesh_elements(umesh)
implicit none
type(st_ustmesh) :: umesh
! -- private date --
integer :: ielem
! -- BODY --
  if (umesh%elemdim == 0) then
    do ielem = 1, umesh%cellvtex%nsection
      umesh%elemdim = max(umesh%elemdim, dim_element(umesh%cellvtex%elem(ielem)))
    enddo
  endif
  !if (umesh%elemdim /= geodim(umesh)) then
  !  call cfd_error("inconsistent mesh and element dimension (USTMESH)")
  !endif
  umesh%ncell_int = number_element(umesh%cellvtex, dim=umesh%elemdim)

endsubroutine check_ustmesh_elements


!------------------------------------------------------------------------------!
! Fonction : dimgeo : dimension de la geometrie du maillage
!------------------------------------------------------------------------------!
integer function geodim(umesh)
  implicit none
  type(st_ustmesh) :: umesh
  
!!$  select case(umesh%geotyp)
!!$  case(geo_1D)
!!$    geodim = 1
!!$  case(geo_2D, geo_2Daxi, geo_2Dcurv)
!!$    geodim = 2
!!$  case(geo_3D)
!!$    geodim = 3
!!$  case default
!!$    call cfd_error("unknown mesh type (USTMESH/geodim)")
!!$  endselect
  geodim = umesh%elemdim
  
endfunction geodim


!------------------------------------------------------------------------------!
! Procedure : delete non "volumic" elements
!------------------------------------------------------------------------------!
subroutine delete_ustmesh_subelements(umesh)
implicit none
type(st_ustmesh) :: umesh
! -- private date --
integer :: ielem
! -- BODY --

do ielem = 1, umesh%cellvtex%nsection
  if (dim_element(umesh%cellvtex%elem(ielem)) < umesh%elemdim) &
    call delete_elemvtex(umesh%cellvtex%elem(ielem))
enddo
call pack_genelemvtex(umesh%cellvtex)

endsubroutine delete_ustmesh_subelements


!------------------------------------------------------------------------------!
! Procedure : desallocation d'une structure USTMESH
!------------------------------------------------------------------------------!
subroutine delete_ustmesh(umesh)
implicit none
type(st_ustmesh) :: umesh
integer          :: i

  call delete(umesh%mesh)
  call delete(umesh%facevtex)
  call delete(umesh%facecell)
  call delete_genconnect(umesh%vtexface)
  call delete_genconnect(umesh%colors)
  call delete_genelemvtex(umesh%cellvtex)
  if ((umesh%nboco > 0).and.associated(umesh%boco)) then
    do i = 1, umesh%nboco 
      call delete(umesh%boco(i))
    enddo
    deallocate(umesh%boco)
  endif
  if (st_allocated(umesh%face_Ltag)) call delete(umesh%face_Ltag)
  if (st_allocated(umesh%face_Rtag)) call delete(umesh%face_Rtag)

  !deallocate(umesh%center, umesh%vertex, umesh%volume)
  !deallocate(umesh%iface, umesh%jface)
  !if (umesh%kdim /= 1) deallocate(umesh%kface)

endsubroutine delete_ustmesh


!------------------------------------------------------------------------------!
! Procedure : creation d'une structure BOCO dans USTMESH
!------------------------------------------------------------------------------!
subroutine createboco(umesh, nboco)
implicit none
type(st_ustmesh) :: umesh
integer          :: nboco

  umesh%nboco = nboco
  allocate(umesh%boco(nboco))

endsubroutine createboco


!------------------------------------------------------------------------------!
! Procedure : recherche d'une condition limite dans USTMESH
!------------------------------------------------------------------------------!
function pboco_ustmesh(umesh, name) result(pboco)
implicit none
type(st_ustboco), pointer    :: pboco
type(st_ustmesh), intent(in) :: umesh
character(len=*), intent(in) :: name
! -- variables internes --
integer :: i, info

  info = 0
  do i = 1, umesh%nboco
    if (samestring(umesh%boco(i)%family, name)) then
      info  =  info + 1
      pboco => umesh%boco(i)
    endif
  enddo
  if (info /= 1) call cfd_error("many BOCO structures have the same tag")
  
endfunction pboco_ustmesh




!------------------------------------------------------------------------------!
! Procedure : get_bocofacecenter
!   Extract array of V3D centers of faces indexed by boco list
!------------------------------------------------------------------------------!
subroutine get_bocofacecenter(boco, umesh, center)
implicit none
! -- INPUTS --
type(st_ustboco) :: boco
type(st_ustmesh) :: umesh

! -- OUTPUTS --
type(v3d), dimension(1:boco%nface) :: center

! -- Private data --
integer :: if, iface

! --- BODY ---

do if = 1, boco%nface
  iface = boco%iface(if)
  center(if) = umesh%mesh%face_center(iface,1)
enddo

endsubroutine get_bocofacecenter

!---------------------------------------------------------------------------------------!
!> @brief send ustmesh information 
!---------------------------------------------------------------------------------------!
subroutine ustmesh_send(this, iproc, tag)
implicit none
class(st_ustmesh) :: this
integer(kip)       :: iproc, array(12)
integer(kmpi)      :: tag

  array(1:12) = (/ this%id, this%level, this%elemdim, &
                  this%nvtex, this%nface, this%ncell, &
                  this%nface_int, this%ncell_int, this%nface_lim, this%ncell_lim, &
                  this%nboco, this%nface_intsvm /) 
  call mpi_isend_int(array, 12, iproc, tag, wait=.true.)
endsubroutine

!---------------------------------------------------------------------------------------!
!> @brief receive ustmesh information
!---------------------------------------------------------------------------------------!
subroutine ustmesh_recv(this, iproc, tag)
implicit none
class(st_ustmesh) :: this
integer(kip)       :: iproc, array(12)
integer(kmpi)      :: tag
  call mpi_isend_int(array, 12, iproc, tag, wait=.true.)
  this%id           = array(1)
  this%level        = array(2)
  this%elemdim      = array(3)
  this%nvtex        = array(4)
  this%nface        = array(5)
  this%ncell        = array(6)
  this%nface_int    = array(7)
  this%ncell_int    = array(8)
  this%nface_lim    = array(9)
  this%ncell_lim    = array(10)
  this%nboco        = array(11)
  this%nface_intsvm = array(12) 
endsubroutine

endmodule USTMESH
!------------------------------------------------------------------------------!
! History
!
! oct  2002 : creation du module
! juil 2003 : suppression des structures USTCONNECT, definition dans CONNECTIVITY
!             creation d'une structure de connectivite CELLVTEX
! sept 2007: new routines from createface_fromcgns (traitface, face_exist, same_face)
! Apr  2008: change CELLVTEX description, cf ELEMVTEX module
! Apr  2008: split USTMESH with BOCO definition in USTBOCO
! Aug  2008: include "extract_centre" as "get_bocofacecenter"
! June 2011: face connectivity routines transfered to MESHCONNECT module
! May  2014: send and recv routines
!------------------------------------------------------------------------------!
