!------------------------------------------------------------------------------!
! MODULE : USTMESH                                Authors : J. Gressier
!                                                 Created : October 2002
! Fonction
!   Bibliotheque de procedures et fonctions pour la gestion de maillages
!   non structures
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

module USTMESH

use TYPHMAKE      ! machine accuracy
use GEO3D 
use MESHBASE      ! geometrical basic elements
use CONNECTIVITY  ! lists & connectivity 
use ELEMVTEX      ! Cell (vtex) definition
use DEFFIELD
use GRID_CONNECT  ! definition of connectivity between grids

implicit none

! -- Variables globales du module -------------------------------------------

integer, parameter :: defboco_connect = -1       

! -- DECLARATIONS -----------------------------------------------------------


!------------------------------------------------------------------------------!
! structure ST_CELLVTEX : Definition de connectivite CELL -> VERTEX
!   une connectivite speciale est definie pour une meilleure gestions des
!   actions selon le type des elements.
!------------------------------------------------------------------------------!
!!$type st_cellvtex
!!$  integer          :: dim                      ! dimension spatiale des elements (2D/3D)
!!$  integer          :: nbar, ntri, nquad, &     ! nombre d'elements par famille
!!$                      ntetra, npyra, npenta, nhexa  
!!$  type(st_connect) :: bar, tri, quad,    &     ! definition des elements
!!$                      tetra, pyra, penta, hexa
!!$  integer, dimension(:), pointer &
!!$                   :: ibar, itri, iquad, &     ! redirection d'index vers "icell" de ustmesh
!!$                      itetra, ipyra, ipenta, ihexa 
!!$endtype st_cellvtex


!------------------------------------------------------------------------------!
! Definition de la structure ST_USTBOCO : Definition des conditions aux limites
!------------------------------------------------------------------------------!
type st_ustboco
  character(len=strlen)          :: family     ! nom de famille
  integer                        :: idefboco   ! index pointer to defsolver boco definition
                                               !   if <= 0 then other definition (cf defboco_* constants)
  integer                        :: nface      ! nombre de faces concernees
  integer, dimension(:), pointer :: iface      ! liste des faces concernees par
                                               ! les conditions aux limites
  type(st_genericfield), pointer :: bocofield  ! pointer to chained list of boco fields (in MGRID)
  type(st_gridconnect)           :: gridcon    ! connectivity to grid
endtype st_ustboco


!------------------------------------------------------------------------------!
! Definition de la structure ST_USTMESH : Maillage non structure
!------------------------------------------------------------------------------!
! Organization of face arrays:
!   SVM internal faces (1..nface_svmint)
! les elements limites.

type st_ustmesh
  integer(kip)          :: id                    ! domain id
  integer(kip)          :: level                 ! multigrid level
  !integer               :: nbdim                ! nombre de dimension du maillage
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
  module procedure new_ustmesh, new_ustboco
endinterface

interface delete
  module procedure delete_ustmesh, delete_ustboco
endinterface


! -- Fonctions et Operateurs ------------------------------------------------



! -- IMPLEMENTATION ---------------------------------------------------------
contains


!------------------------------------------------------------------------------!
! Procedure : allocation d'une structure USTMESH
!------------------------------------------------------------------------------!
subroutine new_ustmesh(mesh, ncell, nface, nvtex)
implicit none
type(st_ustmesh) :: mesh
integer       :: ncell, nface, nvtex

  print*,"!!! pas d'allocation dans new_ustmesh !!!"
  stop

endsubroutine new_ustmesh


!------------------------------------------------------------------------------!
! Procedure : Initialization of USTMESH structure
!------------------------------------------------------------------------------!
subroutine init_ustmesh(umesh)
implicit none
type(st_ustmesh) :: umesh

  umesh%id    = -1
  umesh%level = 0
  umesh%nvtex = 0
  umesh%ncell = 0 ; umesh%ncell_int = 0 ; umesh%ncell_lim = 0
  umesh%nface = 0 ; umesh%nface_int = 0 ; umesh%nface_lim = 0 ; umesh%nface_intsvm = 0 
  umesh%nboco = 0
  call new_genelemvtex(umesh%cellvtex, 0)   ! initialization
  nullify(umesh%face_Ltag%fils)
  nullify(umesh%face_Rtag%fils)

  call init_mesh(umesh%mesh)  ! nullify pointers

endsubroutine init_ustmesh


!------------------------------------------------------------------------------!
! Procedure : desallocation d'une structure USTMESH
!------------------------------------------------------------------------------!
subroutine delete_ustmesh(mesh)
implicit none
type(st_ustmesh) :: mesh
integer          :: i

  call delete(mesh%mesh)
  call delete(mesh%facevtex)
  call delete(mesh%facecell)
  call delete_genelemvtex(mesh%cellvtex)
  do i = 1, mesh%nboco 
    call delete(mesh%boco(i))
  enddo
  deallocate(mesh%boco)
  if (st_allocated(mesh%face_Ltag)) call delete(mesh%face_Ltag)
  if (st_allocated(mesh%face_Rtag)) call delete(mesh%face_Rtag)

  !deallocate(mesh%center, mesh%vertex, mesh%volume)
  !deallocate(mesh%iface, mesh%jface)
  !if (mesh%kdim /= 1) deallocate(mesh%kface)

endsubroutine delete_ustmesh


!------------------------------------------------------------------------------!
! Procedure : creation d'une structure BOCO dans USTMESH
!------------------------------------------------------------------------------!
subroutine createboco(mesh, nboco)
implicit none
type(st_ustmesh) :: mesh
integer          :: nboco

  mesh%nboco = nboco
  allocate(mesh%boco(nboco))

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
  if (info /= 1) call erreur("structure","plusieurs noms de conditions limites identiques")
  
endfunction pboco_ustmesh


!------------------------------------------------------------------------------!
! Procedure : allocation d'une structure USTBOCO
!------------------------------------------------------------------------------!
subroutine new_ustboco(bc, nom, n)
implicit none
type(st_ustboco), intent(out) :: bc
character(len=*), intent(in)  :: nom
integer,          intent(in)  :: n

  bc%family = nom
  bc%nface  = n
  allocate(bc%iface(n))
  nullify(bc%bocofield)  
 
  call init_gridconnect(bc%gridcon)

endsubroutine new_ustboco


!------------------------------------------------------------------------------!
! Procedure : desallocation d'une structure USTBOCO
!------------------------------------------------------------------------------!
subroutine delete_ustboco(bc)
implicit none
type(st_ustboco) :: bc
integer          :: i

  deallocate(bc%iface)
  if (bc%idefboco <= 0) call delete(bc%gridcon)
  call delete_chainedgfield(bc%bocofield)

endsubroutine delete_ustboco


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
! Fonction : typgeo : type de geometrie du maillage
!------------------------------------------------------------------------------!
character function typgeo(umesh)
implicit none
type(st_ustmesh) :: umesh

  typgeo = umesh%mesh%info%geom

endfunction typgeo


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
      call erreur("Fatal error", "unexpected face in mesh or bad face array allocation >= "//strof(newf))
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
      call erreur("Conversion CGNS->TYPHON",&
           "bad connectivity : 3 cells found for only one face")
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




endmodule USTMESH

!------------------------------------------------------------------------------!
! History
!
! oct  2002 : creation du module
! juil 2003 : suppression des structures USTCONNECT, definition dans CONNECTIVITY
!             creation d'une structure de connectivite CELLVTEX
! sept 2007: new routines from createface_fromcgns (traitface, face_exist, same_face)
! Apr  2008: change CELLVTEX description, cf ELEMVTEX module
!------------------------------------------------------------------------------!



