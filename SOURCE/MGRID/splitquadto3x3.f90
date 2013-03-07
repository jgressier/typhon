!------------------------------------------------------------------------------!
! Procedure : splitquadto3x3
!
! Fonction 
!   Split quad cells into 3x3 quads
!
!------------------------------------------------------------------------------!
subroutine splitquadto3x3(defmesh, umesh, newmesh, delta)

use OUTPUT
use USTMESH
use MESHBASE
use MESHPARAMS
use MESHCONNECT

implicit none

! -- INPUTS  --
type(mnu_mesh)        :: defmesh      ! mesh parameters
type(st_ustmesh)      :: umesh        ! ustmesh to convert
real(krp)             :: delta

! -- OUTPUTS --
type(st_ustmesh)      :: newmesh      ! new ustmesh (split and converted)

! -- Internal variables --
type(v3d)              :: node
integer                :: ic, ic0, icn, if, if0, ifn, iv  ! cell, face, vtex index
integer                :: iv0, ivf, ic1, ic2, fnv, nv_elem, nf_elem, nRface, icv
integer                :: i, iif, ifR, ifl, iv1, iv2, ib, ifb, ibdef, ifsv
integer                :: cellv(4)         ! cell/vtex definition
integer                :: facev(8)         ! CV face vtex definition
integer                :: face(2), CVface(2), SVface(2)         ! face definition
integer                :: intv(4)          ! internal vtex definition
integer                :: nfgauss          ! number of integration points per face (defspat%svm)
integer                :: ielem, ielemref, nquad
integer                :: intnode, subface, subcell
type(st_connect)       :: cell_fvtex       ! cell to face.midpoint connectivity
type(st_ustmesh)       :: umeshcon
integer, allocatable   :: faceboco(:)      ! face to boco type connectivity
logical                :: rightface

! -- BODY --

call print_info(10, "  . Isotropic 3x3 (quad) mesh refinement...")

! -- quad splitting --
nv_elem = 4     ! nb of vertices per SV cell
nf_elem = 4     ! nb of faces    per SV cell
fnv     = 2     ! nb of vertices per face
nfgauss = 1

! -- check there are only quads --
!
ielemref = getindex_genelemvtex(umesh%cellvtex, elem_quad4)
if (ielemref /= 0) then
  if (umesh%ncell_int /= umesh%cellvtex%elem(ielemref)%nelem) &
    call error_stop("This REFINEMENT can only be used with original QUAD cells (there are not only quads)")
else
  call error_stop("This REFINEMENT can only be used with original QUAD cells (there is no quad)")
endif

!--------------------------------------------------------------------
! initialize and allocate new USTMESH

subcell  = defmesh%defsplit%subcell
intnode  = defmesh%defsplit%intnode
subface  = defmesh%defsplit%svface_split

call init_ustmesh(newmesh, umesh%id)

newmesh%nvtex        = umesh%nvtex + &                                   ! existing vertices
                       umesh%ncell_int*intnode + &                       ! internal SV nodes
                       umesh%nface*(subface-1)                           ! new face nodes 
newmesh%nface_intsvm = umesh%ncell_int * defmesh%defsplit%internal_faces ! internal SV faces
newmesh%nface_int    = newmesh%nface_intsvm + &
                       umesh%nface_int * subface
newmesh%nface_lim    = umesh%nface_lim * subface
newmesh%ncell_int    = umesh%ncell_int * defmesh%defsplit%subcell
newmesh%ncell_lim    = newmesh%nface_lim

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
iv0 = umesh%nvtex

! -- create internal nodes of SV cells --

if (umesh%cellvtex%elem(ielemref)%nvtex /= nv_elem) &
  call error_stop('internal error: unexpected number of nodes')

do ic = 1, umesh%cellvtex%elem(ielemref)%nelem
  cellv(1:4) = umesh%cellvtex%elem(ielemref)%elemvtex(ic,1:4)
  node = umesh%mesh%vertex(cellv(1), 1, 1)                        ! initialize sum of nodes
  do i = 2, nv_elem        ! loop on nodes left (and sum)
    node = node + umesh%mesh%vertex(cellv(i), 1, 1)
  enddo
  node = node / real(nv_elem, krp)
  do iv = 1, intnode
    newmesh%mesh%vertex(iv0+(ic-1)*intnode+iv, 1, 1) =  (1._krp-2._krp*delta)*umesh%mesh%vertex(cellv(iv), 1, 1) + (2._krp*delta)*node
  enddo
enddo

! -- create new SV face nodes (splitting dependant) --
!
iv0 = iv0 + intnode*umesh%ncell_int          ! index offset (4 interior nodes)

call new_connect(cell_fvtex, umesh%ncell_int, nf_elem*(subface-1))  ! 4 SV faces * 2 points per face
cell_fvtex%fils(1:cell_fvtex%nbnodes, 1:cell_fvtex%nbfils) = 0

do if = 1, umesh%nface

  ivf = iv0+(if-1)*(subface-1)

  facev(1:fnv) = umesh%facevtex%fils(if,1:fnv)
  newmesh%mesh%vertex(ivf+1, 1, 1) = (1._krp - delta ) * umesh%mesh%vertex(facev(1), 1, 1) & !1st point
                                               + delta * umesh%mesh%vertex(facev(2), 1, 1)

  newmesh%mesh%vertex(ivf+2, 1, 1) = (1._krp - delta ) * umesh%mesh%vertex(facev(2), 1, 1) & !2nd point
                                               + delta * umesh%mesh%vertex(facev(1), 1, 1)

  ! 
  ! for each SV cell, locate global index vertices
  !   original face L/R tag contains local face index (1-4)
  !ic1 = umesh%facecell%fils(if,1)
  !ic2 = umesh%facecell%fils(if,2)
  !do i = 1, subface-1
  !  cell_fvtex%fils(ic1, (subface-1)*(umesh%face_Ltag%fils(if,1)-1)+i) = iv0+(if-1)*(subface-1)+i
  !  if (ic2 /= 0) &
  !    cell_fvtex%fils(ic2,  (subface-1)*(umesh%face_Rtag%fils(if,1)-1)+i) = iv0+(if-1)*(subface-1)+i
  !enddo
  
  do i = 1,2
    ic = umesh%facecell%fils(if,i)
    if (ic /= 0) then
      cellv(1:4) = umesh%cellvtex%elem(ielemref)%elemvtex(ic, 1:4)
      if (facev(1).eq.cellv(1)) then
        if (facev(2).eq.cellv(2)) then
          cell_fvtex%fils(ic,1) = ivf+1
          cell_fvtex%fils(ic,2) = ivf+2
        else
          cell_fvtex%fils(ic,8) = ivf+1
          cell_fvtex%fils(ic,7) = ivf+2
        endif
      elseif(facev(1).eq.cellv(2)) then
        if (facev(2).eq.cellv(3)) then
          cell_fvtex%fils(ic,3) = ivf+1
          cell_fvtex%fils(ic,4) = ivf+2
        else
          cell_fvtex%fils(ic,2) = ivf+1
          cell_fvtex%fils(ic,1) = ivf+2
        endif
      elseif(facev(1).eq.cellv(3)) then
        if (facev(2).eq.cellv(4)) then
          cell_fvtex%fils(ic,5) = ivf+1
          cell_fvtex%fils(ic,6) = ivf+2
        else
          cell_fvtex%fils(ic,4) = ivf+1
          cell_fvtex%fils(ic,3) = ivf+2
        endif
      elseif(facev(1).eq.cellv(4)) then
        if (facev(2).eq.cellv(1)) then
          cell_fvtex%fils(ic,7) = ivf+1
          cell_fvtex%fils(ic,8) = ivf+2
        else
          cell_fvtex%fils(ic,6) = ivf+1
          cell_fvtex%fils(ic,5) = ivf+2
        endif
      endif
    endif
  enddo
  
  
  
enddo

!--------------------------------------------------------------------
! Create CONTROL VOLUMES (CV) as SV subcells

call addelem_genelemvtex(newmesh%cellvtex)                          ! add a ELEMVTEX section

ielem = 1
nquad = subcell*umesh%cellvtex%elem(ielemref)%nelem               ! define number of QUAD (QUAD => 9 QUAD)

call new_elemvtex(newmesh%cellvtex%elem(ielem), nquad, elem_quad4)      ! allocation

newmesh%cellvtex%elem(ielem)%ielem(1:nquad) = (/ (ic, ic=1, nquad) /)   ! numbering

call print_info(20, "    . creating"//strof(nquad,7)//" CV cells")

do ic = 1, umesh%ncell_int
  icn = (ic-1)*subcell
  cellv(1:nv_elem) = umesh%cellvtex%elem(ielemref)%elemvtex(ic, 1:4)                 ! original vertices
  intv(1:intnode)  = (/ (umesh%nvtex+intnode*(ic-1)+i, i=1, intnode) /)           ! internal vertices
  facev(1:cell_fvtex%nbfils) = cell_fvtex%fils(ic, 1:cell_fvtex%nbfils)     ! SV/CV face  vertices
  ! CV 1-9 
  newmesh%cellvtex%elem(ielem)%elemvtex(icn+1, 1:nv_elem) = (/ cellv(1), facev(1),  intv(1), facev(8) /)
  newmesh%cellvtex%elem(ielem)%elemvtex(icn+2, 1:nv_elem) = (/ facev(1), facev(2),  intv(2),  intv(1) /)
  newmesh%cellvtex%elem(ielem)%elemvtex(icn+3, 1:nv_elem) = (/ facev(4), cellv(2), facev(3),  intv(2) /)
  newmesh%cellvtex%elem(ielem)%elemvtex(icn+4, 1:nv_elem) = (/ facev(8), cellv(1), cellv(4), facev(7) /)
  newmesh%cellvtex%elem(ielem)%elemvtex(icn+5, 1:nv_elem) = (/ cellv(1), cellv(2), cellv(3), cellv(4) /)
  newmesh%cellvtex%elem(ielem)%elemvtex(icn+6, 1:nv_elem) = (/  intv(2), facev(3), facev(4),  intv(3) /)
  newmesh%cellvtex%elem(ielem)%elemvtex(icn+7, 1:nv_elem) = (/ facev(7),  intv(4), facev(6), cellv(4) /)
  newmesh%cellvtex%elem(ielem)%elemvtex(icn+8, 1:nv_elem) = (/  intv(4),  intv(3), facev(5), facev(6) /)
  newmesh%cellvtex%elem(ielem)%elemvtex(icn+9, 1:nv_elem) = (/  intv(3), facev(4), cellv(3), facev(5) /)
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
  ic0             = (ic-1)*subcell
  if0             = (ic-1)*defmesh%defsplit%internal_faces   ! only internal faces first (12)
  intv(1:intnode) =  (/ (umesh%nvtex+intnode*(ic-1)+i, i=1, intnode) /)         ! internal vertices
  facev(1:cell_fvtex%nbfils) = cell_fvtex%fils(ic, 1:cell_fvtex%nbfils)     ! SV/CV face  vertices
  ! internal face tag 2 (separate CV 1 & 2)
  ifn = if0+1
  newmesh%facevtex%fils(ifn, 1:2) = (/ intv(1), facev(1) /)
  newmesh%facecell%fils(ifn, 1:2) = (/ ic0+1, ic0+2 /)         ! neighbours are local CV 1 & 2
  newmesh%face_Ltag%fils(ifn, 1:nfgauss)  = 2   ! local SV tag
  newmesh%face_Rtag%fils(ifn, 1:nfgauss)  = 2
  ! internal face tag 3 
  ifn = if0+2
  newmesh%facevtex%fils(ifn, 1:2) = (/ intv(2), facev(2) /)
  newmesh%facecell%fils(ifn, 1:2) = (/ ic0+2, ic0+3 /)         ! neighbours are local CV 2 & 3
  newmesh%face_Ltag%fils(ifn, 1:nfgauss)  = 3
  newmesh%face_Rtag%fils(ifn, 1:nfgauss)  = 3
  ! internal face tag 6
  ifn = if0+3
  newmesh%facevtex%fils(ifn, 1:2) = (/ intv(4), intv(1) /)
  newmesh%facecell%fils(ifn, 1:2) = (/ ic0+4, ic0+5 /)         ! neighbours are local CV 4 & 5
  newmesh%face_Ltag%fils(ifn, 1:nfgauss)  = 6
  newmesh%face_Rtag%fils(ifn, 1:nfgauss)  = 6
  ! internal face tag 7
  ifn = if0+4
  newmesh%facevtex%fils(ifn, 1:2) = (/ intv(3), intv(2) /)
  newmesh%facecell%fils(ifn, 1:2) = (/ ic0+5, ic0+6 /)         ! neighbours are local CV 5 & 6
  newmesh%face_Ltag%fils(ifn, 1:nfgauss)  = 7
  newmesh%face_Rtag%fils(ifn, 1:nfgauss)  = 7
  ! internal face tag 10
  ifn = if0+5
  newmesh%facevtex%fils(ifn, 1:2) = (/ facev(6), intv(4) /)
  newmesh%facecell%fils(ifn, 1:2) = (/ ic0+7, ic0+8 /)         ! neighbours are local CV 7 & 8
  newmesh%face_Ltag%fils(ifn, 1:nfgauss)  = 10
  newmesh%face_Rtag%fils(ifn, 1:nfgauss)  = 10
  ! internal face tag 11
  ifn = if0+6
  newmesh%facevtex%fils(ifn, 1:2) = (/ facev(5), intv(3) /)
  newmesh%facecell%fils(ifn, 1:2) = (/ ic0+8, ic0+9 /)         ! neighbours are local CV 8 & 9
  newmesh%face_Ltag%fils(ifn, 1:nfgauss)  = 11
  newmesh%face_Rtag%fils(ifn, 1:nfgauss)  = 11
  ! internal face tag 14
  ifn = if0+7
  newmesh%facevtex%fils(ifn, 1:2) = (/ facev(8), intv(1) /)
  newmesh%facecell%fils(ifn, 1:2) = (/ ic0+1, ic0+4 /)         ! neighbours are local CV 1 & 4
  newmesh%face_Ltag%fils(ifn, 1:nfgauss)  = 14
  newmesh%face_Rtag%fils(ifn, 1:nfgauss)  = 14
  ! internal face tag 15
  ifn = if0+8
  newmesh%facevtex%fils(ifn, 1:2) = (/ facev(7), intv(4) /)
  newmesh%facecell%fils(ifn, 1:2) = (/ ic0+4, ic0+7 /)         ! neighbours are local CV 4 & 7
  newmesh%face_Ltag%fils(ifn, 1:nfgauss)  = 15
  newmesh%face_Rtag%fils(ifn, 1:nfgauss)  = 15
  ! internal face tag 18
  ifn = if0+9
  newmesh%facevtex%fils(ifn, 1:2) = (/ intv(1), intv(2) /)
  newmesh%facecell%fils(ifn, 1:2) = (/ ic0+2, ic0+5 /)         ! neighbours are local CV 2 & 5
  newmesh%face_Ltag%fils(ifn, 1:nfgauss)  = 18
  newmesh%face_Rtag%fils(ifn, 1:nfgauss)  = 18
  ! internal face tag 19
  ifn = if0+10
  newmesh%facevtex%fils(ifn, 1:2) = (/ intv(4), intv(3) /)
  newmesh%facecell%fils(ifn, 1:2) = (/ ic0+5, ic0+8 /)         ! neighbours are local CV 5 & 8
  newmesh%face_Ltag%fils(ifn, 1:nfgauss)  = 19
  newmesh%face_Rtag%fils(ifn, 1:nfgauss)  = 19
  ! internal face tag 22
  ifn = if0+11
  newmesh%facevtex%fils(ifn, 1:2) = (/ intv(2), facev(3) /)
  newmesh%facecell%fils(ifn, 1:2) = (/ ic0+3, ic0+6 /)         ! neighbours are local CV 3 & 6
  newmesh%face_Ltag%fils(ifn, 1:nfgauss)  = 22
  newmesh%face_Rtag%fils(ifn, 1:nfgauss)  = 22
  ! internal face tag 23
  ifn = if0+12
  newmesh%facevtex%fils(ifn, 1:2) = (/ intv(3), facev(4) /)
  newmesh%facecell%fils(ifn, 1:2) = (/ ic0+6, ic0+9 /)         ! neighbours are local CV 6 & 9
  newmesh%face_Ltag%fils(ifn, 1:nfgauss)  = 23
  newmesh%face_Rtag%fils(ifn, 1:nfgauss)  = 23
enddo

! --- Riemann faces from original mesh faces (temporary connectivities) ---

call init_ustmesh(umeshcon, 0)

nRface = newmesh%nface-newmesh%nface_intsvm

call new_connect(umeshcon%facecell,  nRface, 2)       ; umeshcon%facecell%nbnodes = 0 ; umeshcon%facecell%fils = 0
call new_connect(umeshcon%facevtex,  nRface, fnv)     ; umeshcon%facevtex%nbnodes = 0
call new_connect(umeshcon%face_Ltag, nRface, nfgauss) ; umeshcon%face_Ltag%fils(:,:) = 0
call new_connect(umeshcon%face_Rtag, nRface, nfgauss) ; umeshcon%face_Rtag%fils(:,:) = 0

call new_genconnect(umeshcon%vtexface, newmesh%nvtex, 10, initdim=0)    ! 10 face per vertex as initial guess
!vtex_face%fils(:,:) = 0                           ! initialization

call print_info(20, "    . creating"//strof(nRface,7)//" Riemann  CV faces")

do ic = 1, umesh%ncell_int

  ic0     = (ic-1)*subcell      ! CV index offset
  facev(1:cell_fvtex%nbfils) = cell_fvtex%fils(ic, 1:cell_fvtex%nbfils)     ! CV vertices on SV faces
  cellv(1:nv_elem)           = umesh%cellvtex%elem(ielemref)%elemvtex(ic, 1:nv_elem)

  !
  ! -- 'CV 1' Riemann faces --
  icv  = ic0 + 1
  face = (/ facev(8), cellv(1) /)
  call ust_create_face(fnv, icv, face, 1, umeshcon)
  face = (/ cellv(1), facev(1) /)
  call ust_create_face(fnv, icv, face, 13, umeshcon)
  !
  ! -- 'CV 2' Riemann faces --
  icv  = ic0 + 2
  face = (/ facev(1), facev(2) /)
  call ust_create_face(fnv, icv, face, 17, umeshcon)
  !
  ! -- 'CV 3' Riemann faces --
  icv  = ic0 + 3
  face = (/ facev(3), cellv(2) /)
  call ust_create_face(fnv, icv, face, 4, umeshcon)
  face = (/ cellv(2), facev(2) /)
  call ust_create_face(fnv, icv, face, 21, umeshcon)
  !
  ! -- 'CV 4' Riemann faces --
  icv  = ic0 + 4
  face = (/ facev(7), facev(8) /)
  call ust_create_face(fnv, icv, face, 5, umeshcon)
  !
  ! -- 'CV 5' Riemann faces : no Riemann face
  ! -- 'CV 6' Riemann faces --
  icv  = ic0 + 6
  face = (/ facev(4), facev(3) /)
  call ust_create_face(fnv, icv, face, 8, umeshcon)
  !
  ! -- 'CV 7' Riemann faces --
  icv  = ic0 + 7
  face = (/ cellv(4), facev(7) /)
  call ust_create_face(fnv, icv, face, 9, umeshcon)
  face = (/ cellv(4), facev(6) /)
  call ust_create_face(fnv, icv, face, 16, umeshcon)
  !
  ! -- 'CV 8' Riemann faces --
  icv  = ic0 + 8
  face = (/ facev(6), facev(5) /)
  call ust_create_face(fnv, icv, face, 20, umeshcon)
  !
  ! -- 'CV 9' Riemann faces --
  icv  = ic0 + 9
  face = (/ facev(4), cellv(3) /)
  call ust_create_face(fnv, icv, face, 12, umeshcon)
  face = (/ cellv(3), facev(5) /)
  call ust_create_face(fnv, icv, face, 24, umeshcon)

enddo

! --- check created faces ---

if (count(umeshcon%facecell%fils(:,2) == 0) /= newmesh%nface_lim) &
  call error_stop("Spectral Volume Mesh creation: bad number of boundering faces")

! --- Riemann faces : Transfer from temporary connectivities ---

ifR = newmesh%nface_intsvm
ifl = newmesh%nface_int

do if = 1, nRface
  if (umeshcon%facecell%fils(if, 2) == 0) then  ! --- this face is a boundering face
    ifl = ifl +1
    iif = ifl
  else                                  ! --- this face is an internal Riemann face
    ifR = ifR +1
    iif = ifR
  endif
  newmesh%facecell%fils(iif, 1:2)        = umeshcon%facecell%fils(if, 1:2)
  newmesh%facevtex%fils(iif, 1:fnv)      = umeshcon%facevtex%fils(if, 1:fnv)
  newmesh%face_Ltag%fils(iif, 1:nfgauss) = umeshcon%face_Ltag%fils(if, 1:nfgauss)
  newmesh%face_Rtag%fils(iif, 1:nfgauss) = umeshcon%face_Rtag%fils(if, 1:nfgauss)
enddo

if (ifR /= newmesh%nface_int) &
  call error_stop("Spectral Volume Mesh creation: bad number of internal faces")

!--------------------------------------------------------------------
! delete
call delete(cell_fvtex)
call delete(umeshcon)

!!$!--------------------------------------------------------------------
!!$! BOCO transfer !!!!!!!!!!!!!!!!!!! a given cell can be neighbour of many BOCOs !!!!!!!!!!!!!!!!!!!!!!!!
!!$

! --- allocate SVM boco ---

call createboco(newmesh, umesh%nboco)

do ib = 1, umesh%nboco
  call new_ustboco(newmesh%boco(ib), umesh%boco(ib)%family, umesh%boco(ib)%nface*subface)
  newmesh%boco(ib)%idefboco = umesh%boco(ib)%idefboco   ! save BOCO index in defsolver
  newmesh%boco(ib)%nface    = 0                         ! reinit face counter
enddo

allocate(faceboco(newmesh%nface_int+1:newmesh%nface))
faceboco(newmesh%nface_int+1:newmesh%nface) = 0

call print_info(20, "    . creating BOCO tags")

iv0 = umesh%nvtex + umesh%ncell_int * intnode   

do if = newmesh%nface_int+1, newmesh%nface

  CVface(1:fnv) = newmesh%facevtex%fils(if, 1:fnv)       ! look for CVface associated BOCO
  ibdef  = 0

  bocoloop: do ib = 1, umesh%nboco                           ! loop on BOCO

    if (umesh%boco(ib)%nface == 0) cycle      

    do ifb = 1, umesh%boco(ib)%nface
      ifsv = umesh%boco(ib)%iface(ifb)
      SVface(1:fnv) = umesh%facevtex%fils(ifsv, 1:fnv)
      rightface = .true.
      do iv = 1, fnv        ! test all vertices of CV face
        ! test if CV vertex is either a SV (same face) vertex or sub-vertex of same face
        rightface = ( (any(CVface(iv) == SVface(1:fnv))) &
                  .or.(any(CVface(iv) == (/ (iv0+(ifsv-1)*(subface-1)+i, i=1,subface-1) /))) )
        if (.not.rightface) exit
      enddo
      if (rightface) exit
    enddo
    if (rightface) then
      ibdef = ib
      exit
    endif

  enddo bocoloop

  if (ibdef == 0) call error_stop("SVM mesh creation: BOCO not found for CV face")

  faceboco(if) = ibdef

enddo

! --- redistribute boco defs ---

do if = newmesh%nface_int+1, newmesh%nface
  ib = faceboco(if)
  newmesh%boco(ib)%nface = newmesh%boco(ib)%nface + 1
  newmesh%boco(ib)%iface(newmesh%boco(ib)%nface) = if
enddo

call check_ustmesh_elements(newmesh)

endsubroutine splitquadto3x3

!------------------------------------------------------------------------------!
! Change history
!
! Feb  2013: renumbered
!------------------------------------------------------------------------------!
