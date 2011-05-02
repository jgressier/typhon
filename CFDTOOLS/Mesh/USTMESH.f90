!------------------------------------------------------------------------------!
! MODULE : USTMESH 
!
! Grid geometry and connectivity for unstructured mesh
!------------------------------------------------------------------------------!

module USTMESH

use MESHPREC      ! configuration of machine accuracy
use MESHBASE      ! geometrical basic elements
use MESHPARAMS
use CONNECTIVITY  ! lists & connectivity 
use ELEMVTEX      ! Cell (vtex) definition
use IOCFD
use DEF_USTBOCO

implicit none


! -- DECLARATIONS -----------------------------------------------------------


!------------------------------------------------------------------------------!
! Definition de la structure ST_USTMESH : Maillage non structure
!------------------------------------------------------------------------------!
! Organization of face arrays:
!   SVM internal faces (1..nface_svmint)
! les elements limites.

type st_ustmesh
  integer(kip)          :: id                    ! domain id
  integer(kip)          :: level                 ! multigrid level
  integer(kpp)          :: geotyp                ! mesh type (cf MESHGEOM)
  integer(kpp)          :: elemdim               ! mesh dimension
  integer(kip)          :: nvtex, nface, ncell   ! number of vertices, faces and cells
  integer(kip)          :: nface_int, ncell_int  ! number of internal faces and cells
  integer(kip)          :: nface_lim, ncell_lim  ! number of boundering faces and cells
  type(st_mesh)         :: mesh                  ! mesh (geometrical data)
  type(st_connect)      :: facevtex, &           ! connectivite face   -> sommets   par type
                           facecell              ! connectivite face   -> cellules  par type
                                                 ! SUPPOSED TO INDEX LOWER INDEX CELL BEFORE
  type(st_genelemvtex)  :: cellvtex              ! CELL-VTEX connectivity
  
  integer               :: nboco                 ! number of boundary conditions
  type(st_ustboco), dimension(:), pointer &
                        :: boco                  ! liste des conditions aux limites
  type(st_connect)      :: face_Ltag, face_Rtag  ! define Riemann face as a local Gauss pt index
  ! --- specific SVM structure ---
  integer(kip)          :: nface_intsvm          ! number of internal SVM faces
endtype st_ustmesh



! -- INTERFACES -------------------------------------------------------------

interface new
  module procedure new_ustmesh
endinterface

interface delete
  module procedure delete_ustmesh
endinterface


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
endsubroutine new_ustmesh

!------------------------------------------------------------------------------!
! Procedure : Initialization of USTMESH structure
!------------------------------------------------------------------------------!
subroutine init_ustmesh(umesh, geotyp, id)
implicit none
type(st_ustmesh) :: umesh
integer(kpp)     :: geotyp
integer(kip)     :: id

  umesh%id     = id
  umesh%geotyp = geotyp
  umesh%elemdim = 0
  umesh%level  = 0
  umesh%nvtex  = 0
  umesh%ncell  = 0 ; umesh%ncell_int = 0 ; umesh%ncell_lim = 0
  umesh%nface  = 0 ; umesh%nface_int = 0 ; umesh%nface_lim = 0 ; umesh%nface_intsvm = 0 
  umesh%nboco  = 0
  call new_genelemvtex(umesh%cellvtex, 0)   ! initialization
  nullify(umesh%face_Ltag%fils)
  nullify(umesh%face_Rtag%fils)

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
  if (umesh%elemdim /= geodim(umesh)) then
    call cfd_error("inconsistent mesh and element dimension (USTMESH)")
  endif
  umesh%ncell_int = number_element(umesh%cellvtex, dim=umesh%elemdim)

endsubroutine check_ustmesh_elements


!------------------------------------------------------------------------------!
! Fonction : dimgeo : dimension de la geometrie du maillage
!------------------------------------------------------------------------------!
integer function geodim(umesh)
  implicit none
  type(st_ustmesh) :: umesh
  
  select case(umesh%geotyp)
  case(geo_1D)
    geodim = 1
  case(geo_2D, geo_2Daxi, geo_2Dcurv)
    geodim = 2
  case(geo_3D)
    geodim = 3
  case default
    call cfd_error("unknown mesh type (USTMESH/geodim)")
  endselect
  
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
  call delete_genelemvtex(umesh%cellvtex)
  do i = 1, umesh%nboco 
    call delete(umesh%boco(i))
  enddo
  deallocate(umesh%boco)
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
! Fonction : face_invtexlist
! Teste la face est incluse (selon ses sommets) dans une liste de sommets
!------------------------------------------------------------------------------!
logical function face_invtexlist(nsf, face, nsl, vtexlist)
implicit none
! -- Entrees --
integer                   :: nsf, nsl         ! nombre de sommets de la face et de la liste
integer, dimension(1:nsf) :: face             ! face a rechercher
integer, dimension(1:nsl) :: vtexlist         ! liste des sommets
! -- Variables internes --
integer :: isf, isl
logical :: same_som

  ! -- Debut de procedure
   
  do isf = 1, nsf   ! boucle sur les sommets de la face
    ! recherche sommet par sommet de FACE dans VTEXLIST

    do isl = 1, nsl
      same_som = (face(isf)==vtexlist(isl)).or.(face(isf)==0)   ! la face peut etre definie avec des 0
      if (same_som) exit    ! le sommet a ete trouve : on passe au suivant (de la face)
    enddo

    if (.not.same_som) exit   ! un sommet non trouve de la face suffit a quitter
  enddo

  face_invtexlist = same_som

endfunction face_invtexlist


!------------------------------------------------------------------------------!
! Procedure : ust_create_face
!   . create face in "face_vtex" connectivity & check if it already exists
!   . update "face_cell" connectivity (allow only 2 neighbours) 
!   . if face_prop /= 0, set Ltag or Rtag
!------------------------------------------------------------------------------!
subroutine ust_create_face(nsom, icell, face, face_prop, face_vtex, face_cell, Ltag, Rtag, vtex_face, nface)
  implicit none

  ! -- INPUTS --
  integer, intent(in)     :: nsom          ! nb of vertices for face(:)
  integer, intent(in)     :: icell         ! neighbouring cell
  integer, intent(in)     :: face(1:nsom)  ! face to create (with vertex connection)
  integer, intent(in)     :: face_prop     ! face property

  ! -- INPUTS/OUTPUTS --
  type(st_connect)        :: face_vtex     ! face->vertices connectivity (add a face)
  type(st_connect)        :: face_cell     ! face->cell     connectivity (add a face and/or a neighbour)
  type(st_connect)        :: Ltag, Rtag    ! face tag (with face property, if /= 0)
  type(st_connect)        :: vtex_face     ! vertex->face   connectivity (useful for the algorithm)
  integer                 :: nface(1:vtex_face%nbnodes)   ! number of assigned faces 

  ! -- internal variables --
  integer :: iface, newf         ! face index if found, face index if new
  integer :: i, is, ns           ! loop index, vertex index, number of vertices

  ! -- BODY --

  ! -- looking for same face --

  iface = scan_face(face, nsom, face_vtex, vtex_face, nface)   ! return 0 if not found

  if (iface == 0) then      ! ----------------- new face (face of left cell) ---------------
    !
    ! -- add the face to face_vtex --
    newf              = face_vtex%nbnodes + 1
    face_vtex%nbnodes = newf
    if (newf > size(face_vtex%fils, 1)) &
      call cfd_error("unexpected face in mesh or bad face array allocation >= "//strof(newf))
    face_vtex%fils(newf, 1:nsom) = face(1:nsom)
    !print*,'newface',face
    !
    ! -- update face_cell connectivity --
    face_cell%nbnodes = newf                               
    face_cell%fils(newf, 1) = icell      ! (*,1) first neighbouring cell of this face
    !
    ! -- update vtex_face connectivity (useful for face_exist algorithm) --
    do i = 1, nsom
      is = face(i)
      nface(is)                     = nface(is) + 1
      vtex_face%fils(is, nface(is)) = newf
    enddo
    !
    ! -- set face property --
    if (face_prop /= 0) Ltag%fils(newf, 1) = face_prop

  else ! ---------- face already exists: no creation / current cell is right cell ---------

    !print*,'oldface',face
    if (face_cell%fils(iface,2) /= 0) then
      call cfd_error("cell->face conversion error, bad connectivity : 3 cells found for only one face")
    endif
    face_cell%fils(iface,2) = icell   ! seconde cellule
    !
    ! -- set face property --
    if (face_prop /= 0) Rtag%fils(iface, 1) = face_prop

  endif

endsubroutine ust_create_face


!------------------------------------------------------------------------------!
! Function : scan_face
!   seeks 'face' in existing 'face_vtex' connectivity
!   (needs working connectivity vtex_face and nface)
!   return face index if it exists, O otherwise
!------------------------------------------------------------------------------!
integer function scan_face(face, nvtex, face_vtex, vtex_face, nface) 
  implicit none 
  ! -- INPUTS --
  integer          :: nvtex              ! number of vertex in the face
  integer          :: face(1:nvtex)      ! face definition
  type(st_connect) :: face_vtex                    ! face->vertices connectivity (existing faces)
  type(st_connect) :: vtex_face                    ! vertex->face   connectivity (useful for the algorithm)
  integer          :: nface(1:vtex_face%nbnodes)   ! number of assigned faces 
  ! -- Internal variables --
  integer :: if, iface, isom  
  logical :: find_face

  ! -- BODY --

  scan_face = 0  ! Initialization (result if face not found)

  ! Only testing 
  isom      = face(1)
  find_face = .false.

  ! -- loop on existing faces containing face(1) vertex --
  do if = 1, nface(isom)   
    iface     = vtex_face%fils(isom,if)                                   ! current face index
    find_face = same_face(nvtex, face, face_vtex%fils(iface, 1:nvtex))    ! face comparison
    if (find_face) exit 
  enddo

  if (find_face) scan_face = iface

  !-------------------------
endfunction scan_face

!------------------------------------------------------------------------------!
! Function : scan_bocoface
!   seeks 'face' in existing 'facevtex' connectivity (ONLY BOUNDARY CONDITION faces)
!   return face index if it exists, O otherwise
!------------------------------------------------------------------------------!
integer function scan_bocoface(face, nvtex, umesh) 
  implicit none 
  ! -- INPUTS --
  integer          :: nvtex              ! number of vertex in the face
  integer          :: face(1:nvtex)      ! face definition
  type(st_ustmesh) :: umesh
  ! -- Internal variables --
  integer :: iface, isom  
  logical :: find_face

  ! -- BODY --

  scan_bocoface = 0  ! Initialization (result if face not found)

  ! Only testing 
  isom      = face(1)
  find_face = .false.

  ! -- loop on existing faces containing face(1) vertex --
  do iface = umesh%nface_int+1, umesh%nface
    find_face = same_face(nvtex, face, umesh%facevtex%fils(iface, 1:nvtex))
    if (find_face) exit
  enddo

  if (find_face) scan_bocoface = iface

  !-------------------------
endfunction scan_bocoface


!------------------------------------------------------------------------------!
! Fonction : same_face  
! Teste si deux faces ont les memes sommets 
!  (Hyp. : elles ont le meme nombre de sommets)
!------------------------------------------------------------------------------!
logical function same_face(nsom, face1, face2) 
  implicit none
  ! -- Entrees --
  integer, dimension(:)   :: face1, face2     ! faces a comparer
  integer                 :: nsom             ! nombre de sommets des faces
  ! -- Variables internes --
  integer :: isom1, isom2
  logical :: same_som

  ! -- Debut de procedure

  do isom1 = 1, nsom   ! boucle sur les sommets de la face1
    ! recherche sommet par sommet de FACE1 dans FACE2

    do isom2 = 1, nsom
      same_som = ( face1(isom1) == face2(isom2) )
      if (same_som) exit    ! le sommet a ete trouve : on passe au suivant
    enddo

    if (.not.same_som) exit   ! un sommet non trouve suffit a quitter
  enddo

  same_face = same_som

  !-------------------------
endfunction same_face


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
  center(if) = umesh%mesh%iface(iface,1,1)%centre
enddo

endsubroutine get_bocofacecenter



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
!------------------------------------------------------------------------------!
