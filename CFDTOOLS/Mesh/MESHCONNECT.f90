!------------------------------------------------------------------------------!
! MODULE : MESHCONNECT
! 
! Fonction
!   Computation of mesh connectivity
!------------------------------------------------------------------------------!
module MESHCONNECT

use IOCFD   
use USTMESH  

implicit none

! -- Variables globales du module -------------------------------------------

! -- DECLARATIONS -----------------------------------------------------------

! -- INTERFACES -------------------------------------------------------------

! -- Fonctions et Operateurs ------------------------------------------------

! -- IMPLEMENTATION ---------------------------------------------------------
contains

!------------------------------------------------------------------------------!
! Procedure : create_face_connect
!
! Fonction
!   creation of TYPHON USTMESH connectivities
!   . face->cells
!   . face->vtex
!   . boundary condition links (tags->faces)
!------------------------------------------------------------------------------!
subroutine create_face_connect(facetag, umesh, verbose) 

use IOCFD   
use USTMESH  

implicit none 

! -- INPUTS --
logical            :: facetag
integer, optional  :: verbose

! -- OUTPUTS --
type(st_ustmesh)   :: umesh             ! unstructured mesh

! -- Internal variables --
type(st_ustmesh)   :: umeshcon              ! temporary unstructured mesh connectivity
                                             !   (face->vtex, face->cell, left & right face tag)
integer            :: ntotcell              ! calcul du nombre total de cellules
integer            :: maxvtex, maxface      ! nombre de sommets/face, face/cellule
integer            :: nface, nfacesec       ! estimated face number
integer            :: ielem, nelem , nvtex    
integer            :: ista, iend, itype, igeodim
integer            :: iv, if, iverb     

! -- BODY --

if (present(verbose)) then
  iverb = verbose
else
  iverb = 0
endif

!--------------------------------------------------------------------------
! initialization
!--------------------------------------------------------------------------

! initialize face array

nullify(umesh%mesh%iface)
umesh%mesh%nface = 0

if (iverb > 0) call cfd_print("  creating faces and associated connectivity")

! -- estimate of number of faces and number of vtex per face--

nface   = 0
maxvtex = 0
do ielem = 1, umesh%cellvtex%nsection
  nfacesec = umesh%cellvtex%elem(ielem)%nelem * ( nface_element(umesh%cellvtex%elem(ielem)%elemtype) -1)
  if (nfacesec < 0) then
    call cfd_error("inconsistent value, corrupted data in estimated face number")
  endif
  nface   = nface + nfacesec
  maxvtex = max(maxvtex, nvtexperface_element(umesh%cellvtex%elem(ielem)%elemtype))
enddo

! -- connectivite intermediaire face->sommets --

call new(umeshcon%facevtex, nface, maxvtex)         ! nface is only is estimated size
umeshcon%facevtex%nbnodes   = 0                     ! set face counter to zero (not yet created)
umeshcon%facevtex%fils(:,:) = 0                     ! initialization

! -- connectivite intermediaire face->cellules --
call new(umeshcon%facecell, nface, 2)               ! nface is only is estimated size
umeshcon%facecell%nbnodes   = 0                     ! reinitialisation : nombre de faces crees
umeshcon%facecell%fils(:,:) = 0                     ! initialisation de la connectivite

if (facetag) then
  call new_connect(umeshcon%face_Ltag, nface, 1)
  call new_connect(umeshcon%face_Rtag, nface, 1)
  umeshcon%face_Ltag%fils(:,:) = 0
  umeshcon%face_Rtag%fils(:,:) = 0
endif  

!-------------------------------------------------------------------
! creation of faces (face->vtex) and connectivities (face->cell)
!-------------------------------------------------------------------

call create_facevtex(facetag, umesh%nvtex, umesh%cellvtex, umeshcon)

! Recopie des connectivites dans la structure TYPHON
! avec le nombre exact de faces reconstruites

nface           = umeshcon%facevtex%nbnodes     ! meme valeur que umeshcon%facecell%nbnodes aussi
umesh%nface     = nface

call new(umesh%facevtex, nface, maxvtex)
do if = 1, nface   ! loop because of stack size problem
  umesh%facevtex%fils(if,1:maxvtex) = umeshcon%facevtex%fils(if,1:maxvtex)
enddo

call new(umesh%facecell, nface, 2)
do if = 1, nface   ! loop because of stack size problem
  umesh%facecell%fils(if,1:2) = umeshcon%facecell%fils(if,1:2)
enddo


if (facetag) then
  call new_connect(umesh%face_Ltag, nface, 1)
  umesh%face_Ltag%fils(1:nface,1)       = umeshcon%face_Ltag%fils(1:nface,1)
  call new_connect(umesh%face_Rtag, nface, 1)
  umesh%face_Rtag%fils(1:nface,1)       = umeshcon%face_Rtag%fils(1:nface,1)
endif

if (iverb > 0) then
  call cfd_print("  >"//strof(nface,9)//" created faces")
  call cfd_print("  >"//strof(count(umesh%facecell%fils(1:nface,2)==0),9)//" external faces")
endif

! -- Renumerotation des faces --

call reorder_ustconnect(0, umesh, umeshcon%vtexface)    ! action sur les connectivites uniquement

! -- Converting boundary conditions --

call create_ustboco(umesh, umeshcon%vtexface, verbose=iverb)

umesh%ncell_lim = umesh%nface_lim
umesh%ncell     = umesh%ncell_int + umesh%ncell_lim

! -- desallocation --
call delete(umeshcon)

! -- Deleting unused element/vtex connectivities --
call delete_ustmesh_subelements(umesh)

!-------------------------
endsubroutine create_face_connect
!------------------------------------------------------------------------------!
! Changes history
!
! nov  2002 : created
! fev  2004 : renseignements dans structure INFO_MESH
! juin 2004 : memorisation de la connectivite cell->vtex
!             construction de faces avec toutes les familles
! oct  2007 : SVM oriented mesh creation (subcells)
! dec  2010 : typhon (cgns2typhon_ustmesh) -> cfdtools (create_face_connect)
!------------------------------------------------------------------------------!

!------------------------------------------------------------------------------!
! Procedure : create_ustboco
!  
! Fonction
!   Creation des conditions aux limites par conversion CGNS->TYPHON
!   Test de coherence entre les faces CGNS et les faces creees dans TYPHON
!
! Defauts/Limitations/Divers :
!   ATTENTION : les faces sont supposees ordonnees (faces limites en fin de tab)
!
!------------------------------------------------------------------------------!
subroutine create_ustboco(umesh, vtexface, verbose) 

use USTMESH       ! Definition des structures maillage non structure
use IOCFD  

implicit none 

! -- INPUTS --
type(st_genconnect) :: vtexface
integer, optional   :: verbose

! -- INPUTS/OUTPUTS --
type(st_ustmesh)    :: umesh           ! unstructured mesh

! -- Internal variables --
integer             :: nface_int       ! nombre de faces internes
integer             :: nface_lim       ! nombre de faces limites
integer             :: ib, if, nf, iib
integer             :: iverb

! -- BODY --

if (present(verbose)) then
  iverb = verbose
else
  iverb = 0
endif

! -- Creation des conditions aux limites --

do ib = 1, umesh%nboco

  umesh%boco(ib)%family = uppercase(umesh%boco(ib)%family)
  if (iverb > 0) &
    call cfd_print("  . linking boundary condition marks"//strof(ib,3)//"/"//trim(strof(umesh%nboco))// &
                    ": "//trim(umesh%boco(ib)%family))

  select case(umesh%boco(ib)%ilocation)
  case(iloc_vtex)
    if (iverb > 0) call cfd_print('      vertex tagging method')
    call seek_bcface_vtex(umesh%boco(ib), umesh)
  case(iloc_elemcell)
    if (iverb > 0) call cfd_print('      cell tagging method')
    call cfd_error('tagging method not yet implemented')
    !call seek_bcface_vtex(ib, umesh%boco(ib), umesh, listface)
  case(iloc_elemface)
    if (iverb > 0) call cfd_print('      face element tagging method')
    call seek_bcface_face(umesh%boco(ib), umesh, vtexface)
  case(iloc_face)
    if (iverb > 0) call cfd_print('      genuine face tagging method (nothing to do)')
  case default
    call cfd_error("boundary condition links: unknown tagging method ("//trim(strof(umesh%boco(ib)%ilocation))//")")
  endselect

  if (iverb > 0) then
    if (umesh%boco(ib)%nface /= 0) then
      call cfd_print('      > '//trim(strof(umesh%boco(ib)%nface))//' mesh faces tagged')
    else
      call cfd_print("      > no marked face has been found, skipping boundary condition mark section...")
    endif
  endif

enddo

! --- Check empty boco marks ---

nf  = 0
iib = 0

do ib = 1, umesh%nboco
  if (umesh%boco(ib)%nface /= 0) then
    iib = iib + 1
    nf  = nf + umesh%boco(ib)%nface
    if (iib < ib) umesh%boco(iib) = umesh%boco(ib)   ! if staggered, then transfer
  else
    call delete_ustboco(umesh%boco(ib))
  endif
enddo

umesh%nboco = iib

nf = 0
do ib = 1, umesh%nboco
  nf = nf + umesh%boco(ib)%nface
enddo

! --- check bounding faces == tagged faces ---

if (nf /= umesh%nface_lim) &
  call cfd_error("number of tagged faces does not correspond to number of boundaring faces")

!-------------------------
endsubroutine create_ustboco
!------------------------------------------------------------------------------!
! Changes history
!
! fev  2003: creation
! june 2004: construction des connectivites BOCO-> faces, generalisation
!            procedure intrinseque transferee dans USTMESH
! Nov  2007: check empty boco marks
! Dec  2010: TYPHON(cgns2typhon_ustboco)->CFDTOOLS(create_ustboco)
!------------------------------------------------------------------------------!


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
subroutine ust_create_face(nsom, icell, face, face_prop, umeshcon)
  implicit none

  ! -- INPUTS --
  integer, intent(in)     :: nsom          ! nb of vertices for face(:)
  integer, intent(in)     :: icell         ! neighbouring cell
  integer, intent(in)     :: face(1:nsom)  ! face to create (with vertex connection)
  integer, intent(in)     :: face_prop     ! face property

  ! -- INPUTS/OUTPUTS --
  type(st_ustmesh)        :: umeshcon      ! connectivities

  ! -- internal variables --
  integer, parameter :: resize = 10
  integer            :: iface, newf         ! face index if found, face index if new
  integer            :: i, is, ns           ! loop index, vertex index, number of vertices

  ! -- BODY --

  ! -- looking for same face --

  iface = scanface_vtexface(face, nsom, umeshcon%facevtex, umeshcon%vtexface)   ! return 0 if not found

  if (iface == 0) then      ! ----------------- new face (face of left cell) ---------------
    !
    ! -- add the face to umeshcon%facevtex --
    newf              = umeshcon%facevtex%nbnodes + 1
    umeshcon%facevtex%nbnodes = newf
    if (newf > size(umeshcon%facevtex%fils, 1)) &
      call cfd_error("unexpected face in mesh or bad face array allocation >= "//strof(newf))
    umeshcon%facevtex%fils(newf, 1:nsom) = face(1:nsom)
    !
    ! -- update umeshcon%facecell connectivity --
    umeshcon%facecell%nbnodes = newf                               
    umeshcon%facecell%fils(newf, 1) = icell      ! (*,1) first neighbouring cell of this face
    !
    ! -- update vtex_face connectivity (useful for face_exist algorithm) --
    do i = 1, nsom
      is = face(i)
      call add_element(umeshcon%vtexface, is, newf, resize=resize)
    enddo
    !
    ! -- set face property --
    if (face_prop /= 0) umeshcon%face_Ltag%fils(newf, 1) = face_prop

  else ! ---------- face already exists: no creation / current cell is right cell ---------

    !print*,'oldface',face
    if (umeshcon%facecell%fils(iface,2) /= 0) then
      call cfd_error("cell->face conversion error, bad connectivity : 3 cells found for only one face")
    endif
    umeshcon%facecell%fils(iface,2) = icell   ! seconde cellule
    !
    ! -- set face property --
    if (face_prop /= 0) umeshcon%face_Rtag%fils(iface, 1) = face_prop

  endif

endsubroutine ust_create_face


!------------------------------------------------------------------------------!
! Function : scan_face
!   seeks 'face' in existing 'face_vtex' connectivity
!   (needs working connectivity vtex_face and nface)
!   return face index if it exists, O otherwise
!------------------------------------------------------------------------------!
integer function scanface_vtexface(face, nvtex, face_vtex, vtex_face) 
  implicit none 
  ! -- INPUTS --
  integer          :: nvtex              ! number of vertex in the face
  integer          :: face(1:nvtex)      ! face definition
  type(st_connect)    :: face_vtex                    ! face->vertices connectivity (existing faces)
  type(st_genconnect) :: vtex_face                    ! vertex->face   connectivity (useful for the algorithm)
  ! -- Internal variables --
  integer :: if, iface, isom  
  logical :: find_face

  ! -- BODY --

  scanface_vtexface = 0  ! Initialization (result if face not found)

  ! Only testing 
  isom      = face(1)
  find_face = .false.

  ! -- loop on existing faces containing face(1) vertex --
  do if = 1, vtex_face%node(isom)%nelem
    iface     = vtex_face%node(isom)%elem(if)                             ! current face index
    find_face = same_face(nvtex, face, face_vtex%fils(iface, 1:nvtex))    ! face comparison
    if (find_face) exit 
  enddo

  if (find_face) scanface_vtexface = iface

  !-------------------------
endfunction scanface_vtexface

!------------------------------------------------------------------------------!
! Function : scan_bocoface
!   seeks 'face' in existing 'facevtex' connectivity (ONLY BOUNDARY CONDITION faces)
!   return face index if it exists, O otherwise
!------------------------------------------------------------------------------!
integer function scanface_allface(face, nvtex, umesh, firstface) 
  implicit none 
  ! -- INPUTS --
  integer           :: nvtex              ! number of vertex in the face
  integer           :: face(1:nvtex)      ! face definition
  type(st_ustmesh)  :: umesh
  integer, optional :: firstface
  ! -- Internal variables --
  integer :: iface, isom, ifirst
  logical :: find_face

  ! -- BODY --

  scanface_allface = 0  ! Initialization (result if face not found)

  if (present(firstface)) then
    ifirst = firstface
  else
    ifirst = 1
  endif

  ! Only testing 
  isom      = face(1)
  find_face = .false.

  ! -- loop on existing faces containing face(1) vertex --
  do iface = ifirst, umesh%nface
    find_face = same_face(nvtex, face, umesh%facevtex%fils(iface, 1:nvtex))
    if (find_face) exit
  enddo

  if (find_face) scanface_allface = iface

  !-------------------------
endfunction scanface_allface


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

  ! -- BODY --

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
! Procedure : reorder_ustconnect.f90      Auteur : J. Gressier
!                                         Date   : Fevrier 2003
! Fonction                                Modif  :
!   Renumerotation des faces (faces internes puis faces limites)
!     et des connectivites associees
!
! Defauts/Limitations/Divers :
!   choix des actions a effectuer par (iaction)
!     O: action sur les connectivites, pas le maillage
!     1: action sur connectivites et maillage
!------------------------------------------------------------------------------!
subroutine reorder_ustconnect(iaction, umesh, vtexface) 
implicit none 
! -- INPUTS --
integer             :: iaction
! -- INPUTS/OUTPUTS --
type(st_ustmesh)              :: umesh           ! unstructured mesh
type(st_genconnect), optional :: vtexface     
! -- Variables internes --
type(st_connect)    :: conn            ! temporary connectivity
integer, dimension(:), allocatable &
                    :: newfaceindex     ! nouvelle renumerotation
integer             :: nface_int       ! nombre de faces internes
integer             :: nface_lim       ! nombre de faces limites
integer             :: ntotface        ! nombre total de faces
integer             :: if, oldf, newf, i, nelem

! -- BODY --

if (iaction /= 0) then
  call cfd_error("unexpexted action in reorder_ustconnect")
endif

ntotface = umesh%facecell%nbnodes
allocate(newfaceindex(ntotface))

! -- renumerotation des faces (faces limites en dernier) --

nface_int = 0
nface_lim = 0

! calcul des nombres de faces et de la transposition d'index
do if = 1, ntotface
  if (umesh%facecell%fils(if,2) == 0) then     ! si face(if) est face limite
    nface_lim = nface_lim + 1
    newfaceindex(if) = ntotface+1-nface_lim
  else
    nface_int = nface_int + 1
    newfaceindex(if) = nface_int
  endif
enddo

! --- face reordering ---

conn = copy(umesh%facevtex)
do if = 1, ntotface
  umesh%facevtex%fils(newfaceindex(if),:) = conn%fils(if,:)
enddo
call delete(conn)

conn = copy(umesh%facecell)
do if = 1, ntotface
  umesh%facecell%fils(newfaceindex(if),:) = conn%fils(if,:)
enddo
call delete(conn)

!print*,st_allocated(umesh%face_Ltag)
if (st_allocated(umesh%face_Ltag)) then
  conn = copy(umesh%face_Ltag)
  do if = 1, ntotface
    umesh%face_Ltag%fils(newfaceindex(if),:) = conn%fils(if,:)
  enddo
  call delete(conn)
endif

!print*,st_allocated(umesh%face_Rtag)
if (st_allocated(umesh%face_Rtag)) then
  conn = copy(umesh%face_Rtag)
  do if = 1, ntotface
    umesh%face_Rtag%fils(newfaceindex(if),:) = conn%fils(if,:)
  enddo
  call delete(conn)
endif

if (present(vtexface)) then
  do i = 1, vtexface%nbnodes
    nelem = vtexface%node(i)%nelem
    if (nelem > 0) then
      vtexface%node(i)%elem(1:nelem) = newfaceindex(vtexface%node(i)%elem(1:nelem))
    endif
  enddo
endif

!!$do if = 1, ntotface
!!$  oldf = newfaceindex(if)
!!$  newf = if
!!$  umesh%facecell%fils(newf,:) = f_cell%fils(oldf,:)
!!$  umesh%facevtex%fils(newf,:) = f_vtex%fils(oldf,:)
!!$enddo

umesh%nface_int = nface_int
umesh%nface_lim = nface_lim

! desallocation

deallocate(newfaceindex)

!-------------------------
endsubroutine reorder_ustconnect
!------------------------------------------------------------------------------!
! Change history
! oct 2007: compact form, added face_L/Rtag reordering
!------------------------------------------------------------------------------!

endmodule MESHCONNECT
!------------------------------------------------------------------------------!
! Changes history
!
! June 2011: created from create_face_connect
! June 2011: use vtexface connectivity to seek marked faces
!            face connectivity routines transfered from USTMESH module
!------------------------------------------------------------------------------!



