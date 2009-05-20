!------------------------------------------------------------------------------!
! Procedure : convert_to_svm                Authors : J. Gressier
!                                           Date    : Oct 2007
! Fonction 
!   Split cells into n Spectral Volume subcells
!
!------------------------------------------------------------------------------!
subroutine convert_to_svm(defmesh, defspat, umesh, newmesh)

use OUTPUT
use USTMESH
use MESHBASE
use MENU_MESH
use MENU_NUM

implicit none

! -- INPUTS  --
type(mnu_mesh)        :: defmesh      ! mesh parameters
type(mnu_spat)        :: defspat      ! spatial numerical method parameters
type(st_ustmesh)      :: umesh        ! ustmesh to convert

! -- OUTPUTS --
type(st_ustmesh)      :: newmesh      ! new ustmesh (split and converted)

! -- Internal variables --
type(v3d)              :: node
integer                :: ic, ic0, icn, if, ifn, iv  ! cell, face, vtex index
integer                :: iv0, ic1, ic2, fnv, cnv, nRface, icv
integer                :: i, iif, ifR, ifl, iv1, iv2, ib, ifb, ibdef, ifsv
integer                :: cellv(8)         ! cell/vtex definition
integer                :: facev(8)         ! CV face vtex definition
integer                :: face (2), CVface(2), SVface(2)         ! face definition
integer                :: intv(8)          ! internal vtex definition
integer                :: nfgauss          ! number of integration points per face (defspat%svm)
integer                :: ielem, ielemtri, ielemquad, nquad
type(st_connect)       :: cell_fvtex       ! cell to face.midpoint connectivity
type(st_connect)       :: face_cell, &     ! temporary connectivity
                          face_vtex, &     ! temporary connectivity
                          vtex_face, &     ! temporary connectivity
                          Ltag, Rtag       ! temporary connectivity
integer, allocatable   :: nfaceofvtex(:)   ! number of faces which share "this" vertex
integer, allocatable   :: faceboco(:)      ! face to boco type connectivity
logical                :: rightface

! -- BODY --

call print_info(10, "  . converting to SVM mesh...")

fnv     = 2     ! nb of vertices per face
cnv     = 3     ! nb of vertices per SV cell
nfgauss = defspat%svm%nb_facepoints

!--------------------------------------------------------------------
! initialize and allocate new USTMESH

call init_ustmesh(newmesh)

newmesh%id           = umesh%id 
newmesh%nvtex        = umesh%nvtex + &                                  ! existing vertices
                       umesh%ncell_int * defspat%svm%intnode + &        ! internal SV nodes
                       umesh%nface     * (defspat%svm%svface_split-1)   ! new face nodes 
newmesh%nface_intsvm = umesh%ncell_int * defspat%svm%internal_faces     ! internal SV faces
newmesh%nface_int    = newmesh%nface_intsvm + &
                       umesh%nface_int * defspat%svm%svface_split
newmesh%nface_lim    = umesh%nface_lim * defspat%svm%svface_split
newmesh%ncell_int    = umesh%ncell_int * defspat%svm%cv_split
newmesh%ncell_lim    = umesh%ncell_lim * defspat%svm%cv_split

newmesh%nface = newmesh%nface_int + newmesh%nface_lim
newmesh%ncell = newmesh%ncell_int + newmesh%ncell_lim

newmesh%mesh%info = umesh%mesh%info

! -- allocate only vertices array --
!
call new_mesh(newmesh%mesh, 0, 0, newmesh%nvtex) 

!--------------------------------------------------------------------
! Create new mesh NODES

! Nodes are created with the following order
!   * already existing nodes
!   * new internal nodes
!   * new face nodes

! -- copy existing vertices --
!
newmesh%mesh%vertex(1:umesh%nvtex, 1, 1) = umesh%mesh%vertex(1:umesh%nvtex, 1, 1) 

if (defmesh%splitmesh /= split_svm2quad) then
  call erreur("Development", "SVM_2QUAD is the only implemented parameter")
endif

! -- check there are only tri --
!
call getindex_genelemvtex(umesh%cellvtex, elem_tri3, ielem)
if (umesh%ncell_int /= umesh%cellvtex%elem(ielem)%nelem) then
  call erreur("Development", "SVM_2QUAD can only be used with original TRI cells")
endif

! -- create internal nodes of SV cells --
!
if (defspat%svm%intnode /= 1) &
  call erreur("Error", "SVM_2QUAD : internal node should be defined to 1")

do ic = 1, umesh%cellvtex%elem(ielem)%nelem
  iv   = umesh%cellvtex%elem(ielem)%elemvtex(ic,1)          ! first node of cell
  node = umesh%mesh%vertex(iv, 1, 1)                        ! initialize sum of nodes
  do i = 2, umesh%cellvtex%elem(ielem)%nvtex                ! loop on nodes left (and sum)
    iv   = umesh%cellvtex%elem(ielem)%elemvtex(ic,i)
    node = node + umesh%mesh%vertex(iv, 1, 1)
  enddo
  newmesh%mesh%vertex(umesh%nvtex+ic, 1, 1) = node /  real(umesh%cellvtex%elem(ielem)%nvtex, krp)
enddo

! -- create new SV face nodes (splitting dependant) --
!
iv0 = umesh%nvtex + umesh%ncell_int * defspat%svm%intnode         ! index offset

call new_connect(cell_fvtex, umesh%ncell_int, 3*(defspat%svm%svface_split-1))
cell_fvtex%fils(1:cell_fvtex%nbnodes, 1:cell_fvtex%nbfils) = 0

do if = 1, umesh%nface

  facev(1:fnv) = umesh%facevtex%fils(if,1:fnv)
  node = umesh%mesh%vertex(facev(1), 1, 1) + umesh%mesh%vertex(facev(2), 1, 1)
  newmesh%mesh%vertex(iv0+if, 1, 1) = 0.5_krp*node

  ic1 = umesh%facecell%fils(if,1)
  ic2 = umesh%facecell%fils(if,2)
  ! 
  cell_fvtex%fils(ic1, umesh%face_Ltag%fils(if,1)) = iv0+if
  if (ic2 /= 0) cell_fvtex%fils(ic2, umesh%face_Rtag%fils(if,1)) = iv0+if
enddo


!--------------------------------------------------------------------
! Create CONTROL VOLUMES (CV) as SV subcells

call addelem_genelemvtex(newmesh%cellvtex)                          ! add a ELEMVTEX section
ielemquad = newmesh%cellvtex%ntype

nquad = defspat%svm%cv_split*umesh%cellvtex%elem(ielem)%nelem               ! define number of QUAD (TRI => 3 QUAD)
call new_elemvtex(newmesh%cellvtex%elem(ielemquad), nquad, elem_quad4)      ! allocation
newmesh%cellvtex%elem(ielemquad)%ielem(1:nquad) = (/ (ic, ic=1, nquad) /)   ! numbering

call print_info(20, "    . creating"//strof(nquad,7)//" CV cells")

do ic = 1, umesh%ncell_int
  icn = (ic-1)*defspat%svm%cv_split
  cellv(1:3) = umesh%cellvtex%elem(ielem)%elemvtex(ic, 1:3)                 ! original vertices
  intv(1)    = umesh%nvtex + ic                                             ! internal vertices
  facev(1:cell_fvtex%nbfils) = cell_fvtex%fils(ic, 1:cell_fvtex%nbfils)     ! CV face  vertices
  ! CV 1 : connected to original vertex 1
  newmesh%cellvtex%elem(ielemquad)%elemvtex(icn+1, 1:4) = (/ cellv(1), facev(3), intv(1), facev(2) /)
  ! CV 2 : connected to original vertex 2
  newmesh%cellvtex%elem(ielemquad)%elemvtex(icn+2, 1:4) = (/ cellv(2), facev(1), intv(1), facev(3) /)
  ! CV 3 : connected to original vertex 3
  newmesh%cellvtex%elem(ielemquad)%elemvtex(icn+3, 1:4) = (/ cellv(3), facev(2), intv(1), facev(1) /)
enddo

!--------------------------------------------------------------------
! Create new mesh FACES : define face (facevtex) and connectivity (facecell)
!   tags are (SV local) Gauss points index

call new_connect(newmesh%facevtex,  newmesh%nface, 2)
call new_connect(newmesh%facecell,  newmesh%nface, 2)
call new_connect(newmesh%face_Ltag, newmesh%nface, nfgauss) ; newmesh%face_Ltag%fils(:,:) = 0
call new_connect(newmesh%face_Rtag, newmesh%nface, nfgauss) ; newmesh%face_Rtag%fils(:,:) = 0

! --- internal faces ---

call print_info(20, "    . creating"//strof(newmesh%nface_intsvm,7)//" internal CV faces")

do ic = 1, umesh%ncell_int
  ifn     = (ic-1)*defspat%svm%internal_faces
  intv(1) = umesh%nvtex  + ic                                            ! internal vertices
  facev(1:cell_fvtex%nbfils) = cell_fvtex%fils(ic, 1:cell_fvtex%nbfils)  ! CV face  vertices
  ! internal face 1 (separate CV 2 & 3)
  newmesh%facevtex%fils(ifn+1, 1:2) = (/ intv(1), facev(1) /)
  newmesh%facecell%fils(ifn+1, 1:2) = (/ ifn+2, ifn+3 /)         ! neighbours are local CV 2 & 3
  newmesh%face_Ltag%fils(ifn+1, 1)  = 1
  newmesh%face_Rtag%fils(ifn+1, 1)  = 1
  ! internal face 2 (separate CV 1 & 3)
  newmesh%facevtex%fils(ifn+2, 1:2) = (/ intv(1), facev(2) /)
  newmesh%facecell%fils(ifn+2, 1:2) = (/ ifn+1, ifn+3 /)         ! neighbours are local CV 1 & 3
  newmesh%face_Ltag%fils(ifn+2, 1)  = 2
  newmesh%face_Rtag%fils(ifn+2, 1)  = 2
  ! internal face 3 (separate CV 1 & 2)
  newmesh%facevtex%fils(ifn+3, 1:2) = (/ intv(1), facev(3) /)
  newmesh%facecell%fils(ifn+3, 1:2) = (/ ifn+1, ifn+2 /)         ! neighbours are local CV 1 & 2
  newmesh%face_Ltag%fils(ifn+3, 1)  = 3
  newmesh%face_Rtag%fils(ifn+3, 1)  = 3
enddo

! --- Riemann faces from original mesh faces (temporary connectivities) ---

nRface = newmesh%nface-newmesh%nface_intsvm

call new_connect(face_cell, nRface, 2)       ; face_cell%nbnodes = 0 ; face_cell%fils = 0
call new_connect(face_vtex, nRface, fnv)     ; face_vtex%nbnodes = 0
call new_connect(Ltag,      nRface, nfgauss) ; Ltag%fils(:,:) = 0
call new_connect(Rtag,      nRface, nfgauss) ; Rtag%fils(:,:) = 0

call new_connect(vtex_face, newmesh%nvtex, 20)    ! 20 face per vertex max
vtex_face%fils(:,:) = 0                           ! initialization

allocate(nfaceofvtex(newmesh%nvtex))
nfaceofvtex(1:newmesh%nvtex) = 0

call print_info(20, "    . creating"//strof(nRface,7)//" Riemann  CV faces")

do ic = 1, umesh%ncell_int

  ic0     = (ic-1)*defspat%svm%internal_faces       ! CV index offset
  facev(1:cell_fvtex%nbfils) = cell_fvtex%fils(ic, 1:cell_fvtex%nbfils)  ! CV face  vertices
  cellv(1:cnv)               = umesh%cellvtex%elem(ielem)%elemvtex(ic, 1:cnv)
  !
  ! -- 'CV 1' Riemann faces --
  icv  = ic0 + 1
  face = (/ facev(2), cellv(1) /)
  call ust_create_face(fnv, icv, face, 4, face_vtex, face_cell, Ltag, Rtag, vtex_face, nfaceofvtex)
  face = (/ cellv(1), facev(3) /)
  call ust_create_face(fnv, icv, face, 5, face_vtex, face_cell, Ltag, Rtag, vtex_face, nfaceofvtex)
  !
  ! -- 'CV 2' Riemann faces --
  icv  = ic0 + 2
  face = (/ facev(3), cellv(2) /)
  call ust_create_face(fnv, icv, face, 6, face_vtex, face_cell, Ltag, Rtag, vtex_face, nfaceofvtex)
  face = (/ cellv(2), facev(1) /)
  call ust_create_face(fnv, icv, face, 7, face_vtex, face_cell, Ltag, Rtag, vtex_face, nfaceofvtex)
  !
  ! -- 'CV 3' Riemann faces --
  icv  = ic0 + 3
  face = (/ facev(1), cellv(3) /)
  call ust_create_face(fnv, icv, face, 8, face_vtex, face_cell, Ltag, Rtag, vtex_face, nfaceofvtex)
  face = (/ cellv(3), facev(2) /)
  call ust_create_face(fnv, icv, face, 9, face_vtex, face_cell, Ltag, Rtag, vtex_face, nfaceofvtex)

enddo

! --- check created faces ---

if (count(face_cell%fils(:,2) == 0) /= newmesh%nface_lim) &
  call erreur("Spectral Volume Mesh creation", "bad number of boundering faces")

! --- Riemann faces : Transfer from temporary connectivities ---

ifR = newmesh%nface_intsvm
ifl = newmesh%nface_int

do if = 1, nRface
  if (face_cell%fils(if, 2) == 0) then  ! --- this face is a boundering face
    ifl = ifl +1
    iif = ifl
  else                                  ! --- this face is an internal Riemann face
    ifR = ifR +1
    iif = ifR
  endif
  newmesh%facecell %fils(iif, 1:2)       = face_cell%fils(if, 1:2)
  newmesh%facevtex %fils(iif, 1:fnv)     = face_vtex%fils(if, 1:fnv)
  !print*,'reorder',iif, face_vtex%fils(if, 1:fnv)
  newmesh%face_Ltag%fils(iif, 1:nfgauss) = Ltag     %fils(if, 1:nfgauss)
  newmesh%face_Rtag%fils(iif, 1:nfgauss) = Rtag     %fils(if, 1:nfgauss)
enddo

if (ifR /= newmesh%nface_int) &
  call erreur("Spectral Volume Mesh creation", "bad number of internal faces")

!--------------------------------------------------------------------
! delete
call delete(cell_fvtex)

!--------------------------------------------------------------------
! reordering faces
!call reorder_ustconnect(O, umesh)

!!$!--------------------------------------------------------------------
!!$! BOCO transfer !!!!!!!!!!!!!!!!!!! a given cell can be neighbour of many BOCOs !!!!!!!!!!!!!!!!!!!!!!!!
!!$
!!$! --- compute original cell->boco connectivity ---
!!$
!!$
!!$do ib = 1, umesh%nboco
!!$  do if = 1, umesh%boco(ib)%nface
!!$    ic = umesh%facecell%fils(umesh%boco(ib)%iface(if), 1)    ! left (internal) cell of 'if' boundaring face
!!$    print*,'cellboco',ic,ib
!!$    cellboco(ic) = ib                                        ! UST BOCO definition
!!$  enddo
!!$enddo

! --- allocate SVM boco ---

call createboco(newmesh, umesh%nboco)

do ib = 1, umesh%nboco
  call new_ustboco(newmesh%boco(ib), umesh%boco(ib)%family, umesh%boco(ib)%nface*defspat%svm%svface_split)
  newmesh%boco(ib)%idefboco = umesh%boco(ib)%idefboco   ! save BOCO index in defsolver
  newmesh%boco(ib)%nface    = 0                         ! reinit face counter
enddo

!!$! --- define SVM boco ---
!!$
!!$do if = newmesh%nface_int+1, newmesh%nface
!!$  ic  = newmesh%facecell%fils(if,1)         ! internal      CV cell in newmesh
!!$  ic0 = ((ic-1)/defspat%svm%cv_split)+1     ! corresponding SV cell in umesh
!!$  ib  = cellboco(ic0)
!!$  !
!!$  print*,if,ic,ic0,ib
!!$  newmesh%boco(ib)%nface                         = newmesh%boco(ib)%nface + 1
!!$  newmesh%boco(ib)%iface(newmesh%boco(ib)%nface) = if
!!$enddo
!!$
!!$! --- check boco numbers ---
!!$
!!$do ib = 1, newmesh%nboco
!!$  if (size(newmesh%boco(ib)%iface) == newmesh%boco(ib)%nface) &
!!$    call erreur("Spectral volume creation", "unexpected number of boundering face for "//trim(newmesh%boco(ib)%family))
!!$enddo
!!$
!!$deallocate(cellboco)

allocate(faceboco(newmesh%nface_int+1:newmesh%nface))
faceboco(newmesh%nface_int+1:newmesh%nface) = 0

call print_info(20, "    . creating BOCO tags")

do if = newmesh%nface_int+1, newmesh%nface

  CVface(1:fnv) = newmesh%facevtex%fils(if, 1:fnv)       ! look for CVface associated BOCO
  ibdef  = 0

  bocoloop: do ib = 1, umesh%nboco                           ! loop on BOCO

    if (umesh%boco(ib)%nface == 0) cycle      

    do ifb = 1, umesh%boco(ib)%nface
      ifsv = umesh%boco(ib)%iface(ifb)
      SVface(1:fnv) = umesh%facevtex%fils(ifsv, 1:fnv)
      rightface = .true.
      do iv = 1, fnv        ! test all vertices pf CV face
        rightface = ((CVface(iv) == iv0+ifsv).or.(any(CVface(iv)==SVface(1:fnv))))
        if (.not.rightface) exit
      enddo
      if (rightface) exit
    enddo
    if (rightface) then
      ibdef = ib
      exit
    endif

  enddo bocoloop

  if (ibdef == 0) call erreur("SVM mesh creation", "BOCO not found for CV face")
  faceboco(if) = ibdef
enddo

! --- redistribute boco defs ---

do if = newmesh%nface_int+1, newmesh%nface
  ib = faceboco(if)
  newmesh%boco(ib)%nface = newmesh%boco(ib)%nface + 1
  newmesh%boco(ib)%iface(newmesh%boco(ib)%nface) = if
enddo

endsubroutine convert_to_svm

!------------------------------------------------------------------------------!
! Change history
!
! Oct  2007: created
!------------------------------------------------------------------------------!
