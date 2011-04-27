!------------------------------------------------------------------------------!
! Procedure : convert_to_svm_cub                
!                                           Date    :  Avr 2008
! Fonction 
!   Split cells into 3pent and 3quad Spectral Volume subcells
!
!------------------------------------------------------------------------------!
subroutine convert_to_svm_cub(defmesh, defspat, umesh, newmesh,alpha,beta)

use OUTPUT
use USTMESH
use MESHBASE
use MESHPARAMS
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
integer                :: ic, ic0, icn, if, ifn, iv ,icnp,icnq ! cell, face, vtex index
integer                :: iv0, ic1, ic2, fnv, cnv, nRface, icv
integer                :: i, iif, ifR, ifl, iv1, iv2, ib, ifb, ibdef, ifsv
integer                :: cellv(8)         ! cell/vtex definition
integer                :: facev(8)         ! CV face vtex definition
integer                :: face (2), CVface(2), SVface(2)         ! face definition
integer                :: intv(8)          ! internal vtex definition
integer                :: nfgauss          ! number of integration points per face (defspat%svm)
integer                :: ielem, ielemtri, ielemquad, nquad, ielempent, npent
type(st_connect)       :: cell_fvtex       ! cell to face.midpoint connectivity
type(st_connect)       :: face_cell, &     ! temporary connectivity
                          face_vtex, &     ! temporary connectivity
                          vtex_face, &     ! temporary connectivity
                          Ltag, Rtag       ! temporary connectivity
integer, allocatable   :: nfaceofvtex(:)   ! number of faces which share "this" vertex
integer, allocatable   :: faceboco(:)      ! face to boco type connectivity
logical                :: rightface
real(krp)              :: alpha, beta      ! geometric coefficient for the split
! -- BODY --

call print_info(10, "  . converting to SVM mesh...")

fnv     = 2     ! nb of vertices per face
cnv     = 3     ! nb of vertices per SV cell
nfgauss = defspat%svm%nb_facepoints


!--------------------------------------------------------------------
! initialize and allocate new USTMESH

call init_ustmesh(newmesh, umesh%geotyp, umesh%id)

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


! -- check there are only tri --
!
ielem = getindex_genelemvtex(umesh%cellvtex, elem_tri3)
if ((ielem /= 0).and.(umesh%ncell_int /= umesh%cellvtex%elem(ielem)%nelem)) then
  call error_stop("SVM can only be used with original TRI cells")
endif

! -- create internal nodes of SV cells --
!

do ic = 1, umesh%cellvtex%elem(ielem)%nelem
  iv   = umesh%cellvtex%elem(ielem)%elemvtex(ic,1)          ! first node of cell
  node = umesh%mesh%vertex(iv, 1, 1)                        ! initialize sum of nodes
  do i = 2, umesh%cellvtex%elem(ielem)%nvtex                ! loop on nodes left (and sum)
    iv   = umesh%cellvtex%elem(ielem)%elemvtex(ic,i)
    node = node + umesh%mesh%vertex(iv, 1, 1)
  enddo
  newmesh%mesh%vertex(umesh%nvtex+ic, 1, 1) = node /  real(umesh%cellvtex%elem(ielem)%nvtex, krp)
enddo


iv0 = umesh%nvtex + umesh%ncell_int         ! index offset

do ic = 1, umesh%cellvtex%elem(ielem)%nelem
  do i = 1, umesh%cellvtex%elem(ielem)%nvtex                ! loop on nodes left 
  iv   = umesh%cellvtex%elem(ielem)%elemvtex(ic,i)
  newmesh%mesh%vertex(iv0+(ic-1)*3+i, 1, 1) = (1._krp - 3._krp / 2._krp * beta) * umesh%mesh%vertex(iv, 1, 1) &                                   
       + 3._krp / 2._krp * beta * newmesh%mesh%vertex(umesh%nvtex+ic, 1, 1)
  enddo
enddo



! -- create new SV face nodes --
!
iv0 = umesh%nvtex + umesh%ncell_int * defspat%svm%intnode         ! index offset

call new_connect(cell_fvtex, umesh%ncell_int, 3*(defspat%svm%svface_split-1))
cell_fvtex%fils(1:cell_fvtex%nbnodes, 1:cell_fvtex%nbfils) = 0


do if = 1, umesh%nface
  facev(1:fnv) = umesh%facevtex%fils(if,1:fnv)
  newmesh%mesh%vertex(iv0+(if-1)*2+1, 1, 1) = (1._krp - alpha ) * umesh%mesh%vertex(facev(1), 1, 1) & !1st point
                                               + alpha * umesh%mesh%vertex(facev(2), 1, 1)

  newmesh%mesh%vertex(iv0+(if-1)*2+2, 1, 1) = (1._krp - alpha ) * umesh%mesh%vertex(facev(2), 1, 1) & !2nd point
                                               + alpha * umesh%mesh%vertex(facev(1), 1, 1)
  ! 

  ic1 = umesh%facecell%fils(if,1)
  ic2 = umesh%facecell%fils(if,2)

  cellv(1:3)= umesh%cellvtex%elem(ielem)%elemvtex(ic1, 1:3)
  if (facev(1).eq.cellv(1)) then
    if (facev(2).eq.cellv(2)) then
  cell_fvtex%fils(ic1,1) = iv0+(if-1)*2+1
  cell_fvtex%fils(ic1,2) = iv0+(if-1)*2+2
    else 
  cell_fvtex%fils(ic1,6) = iv0+(if-1)*2+1
  cell_fvtex%fils(ic1,5) = iv0+(if-1)*2+2
    endif  
  elseif(facev(1).eq.cellv(2)) then
    if (facev(2).eq.cellv(3)) then
  cell_fvtex%fils(ic1,3) = iv0+(if-1)*2+1
  cell_fvtex%fils(ic1,4) = iv0+(if-1)*2+2
    else 
  cell_fvtex%fils(ic1,2) = iv0+(if-1)*2+1
  cell_fvtex%fils(ic1,1) = iv0+(if-1)*2+2
    endif  
  elseif(facev(1).eq.cellv(3)) then
    if (facev(2).eq.cellv(1)) then
  cell_fvtex%fils(ic1,5) = iv0+(if-1)*2+1
  cell_fvtex%fils(ic1,6) = iv0+(if-1)*2+2
    else 
  cell_fvtex%fils(ic1,4) = iv0+(if-1)*2+1
  cell_fvtex%fils(ic1,3) = iv0+(if-1)*2+2
    endif  
   endif

  if (ic2 /= 0) then 
  cellv(1:3)= umesh%cellvtex%elem(ielem)%elemvtex(ic2, 1:3)
  if (facev(1).eq.cellv(1)) then
    if (facev(2).eq.cellv(2)) then
  cell_fvtex%fils(ic2,1) = iv0+(if-1)*2+1
  cell_fvtex%fils(ic2,2) = iv0+(if-1)*2+2
    else 
  cell_fvtex%fils(ic2,6) = iv0+(if-1)*2+1
  cell_fvtex%fils(ic2,5) = iv0+(if-1)*2+2
    endif  
   elseif(facev(1).eq.cellv(2)) then
    if (facev(2).eq.cellv(3)) then
  cell_fvtex%fils(ic2,3) = iv0+(if-1)*2+1
  cell_fvtex%fils(ic2,4) = iv0+(if-1)*2+2
    else 
  cell_fvtex%fils(ic2,2) = iv0+(if-1)*2+1
  cell_fvtex%fils(ic2,1) = iv0+(if-1)*2+2
    endif  
   elseif(facev(1).eq.cellv(3)) then
    if (facev(2).eq.cellv(1)) then
  cell_fvtex%fils(ic2,5) = iv0+(if-1)*2+1
  cell_fvtex%fils(ic2,6) = iv0+(if-1)*2+2
    else 
  cell_fvtex%fils(ic2,4) = iv0+(if-1)*2+1
  cell_fvtex%fils(ic2,3) = iv0+(if-1)*2+2
    endif  
   endif
  endif

enddo


!--------------------------------------------------------------------
! Create CONTROL VOLUMES (CV) as SV subcells


call addelem_genelemvtex(newmesh%cellvtex)                          ! add a ELEMVTEX section
call addelem_genelemvtex(newmesh%cellvtex)                          ! add a ELEMVTEX section

ielemquad = 1
ielempent = 2
nquad = 3*umesh%cellvtex%elem(ielem)%nelem               ! define number of QUAD and PENT
npent = 3*umesh%cellvtex%elem(ielem)%nelem

call new_elemvtex(newmesh%cellvtex%elem(ielemquad), nquad, elem_quad4)      ! allocation
call new_elemvtex(newmesh%cellvtex%elem(ielempent), npent, elem_ngon, 5) 


do ic=1,umesh%ncell_int 
  icnq = (ic-1)*3
  icnp = (ic-1)*3
newmesh%cellvtex%elem(ielemquad)%ielem(icnq+1) = (ic-1)*6+1
newmesh%cellvtex%elem(ielemquad)%ielem(icnq+2) = (ic-1)*6+2
newmesh%cellvtex%elem(ielemquad)%ielem(icnq+3) = (ic-1)*6+3
newmesh%cellvtex%elem(ielempent)%ielem(icnp+1) = (ic-1)*6+4
newmesh%cellvtex%elem(ielempent)%ielem(icnp+2) = (ic-1)*6+5
newmesh%cellvtex%elem(ielempent)%ielem(icnp+3) = (ic-1)*6+6
enddo

call print_info(20, "    . creating"//strof(nquad + npent,7)//" CV cells")

do ic = 1, umesh%ncell_int
  icnq = (ic-1)*3
  icnp = (ic-1)*3
  cellv(1:3) = umesh%cellvtex%elem(ielem)%elemvtex(ic, 1:3)           ! original vertices 
  intv(1)    = umesh%nvtex + ic                               
  intv(2)    = umesh%nvtex + umesh%ncell_int + (ic-1) * (defspat%svm%intnode - 1) + 1                         ! other internal vertices
  intv(3)    = umesh%nvtex + umesh%ncell_int + (ic-1) * (defspat%svm%intnode - 1) + 2
  intv(4)    = umesh%nvtex + umesh%ncell_int + (ic-1) * (defspat%svm%intnode - 1) + 3
  facev(1:cell_fvtex%nbfils) = cell_fvtex%fils(ic, 1:cell_fvtex%nbfils)     ! CV face  vertices

  ! CV 1 : connected to original vertex 1
  newmesh%cellvtex%elem(ielemquad)%elemvtex(icnq+1, 1:4) = (/ cellv(1), facev(1), intv(2), facev(6) /)
  ! CV 2 : connected to original vertex 2
  newmesh%cellvtex%elem(ielemquad)%elemvtex(icnq+2, 1:4) = (/ cellv(2), facev(3), intv(3), facev(2) /)
  ! CV 3 : connected to original vertex 3
  newmesh%cellvtex%elem(ielemquad)%elemvtex(icnq+3, 1:4) = (/ cellv(3), facev(5), intv(4), facev(4) /)
  ! CV 4 (between CV1 and CV2)
  newmesh%cellvtex%elem(ielempent)%elemvtex(icnp+1, 1:5) = (/ intv(2), facev(1), facev(2), intv(3), intv(1) /)
  ! CV 5 (between CV2 and CV3)
  newmesh%cellvtex%elem(ielempent)%elemvtex(icnp+2, 1:5) = (/ intv(3), facev(3), facev(4), intv(4), intv(1) /)
  ! CV 6 (between CV3 and CV1)
  newmesh%cellvtex%elem(ielempent)%elemvtex(icnp+3, 1:5) = (/ intv(4), facev(5), facev(6), intv(2), intv(1) /)

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
  icn = 6*(ic-1)
  intv(1)    = umesh%nvtex + ic                               
  intv(2)    = umesh%nvtex + umesh%ncell_int + (ic-1) * (defspat%svm%intnode - 1) + 1                         ! other internal vertices
  intv(3)    = umesh%nvtex + umesh%ncell_int + (ic-1) * (defspat%svm%intnode - 1) + 2
  intv(4)    = umesh%nvtex + umesh%ncell_int + (ic-1) * (defspat%svm%intnode - 1) + 3
  facev(1:cell_fvtex%nbfils) = cell_fvtex%fils(ic, 1:cell_fvtex%nbfils)     ! CV face  vertices

  ! internal face 1 (separate CV 1 & 4)
  newmesh%facevtex%fils(ifn+1, 1:2) = (/ facev(1), intv(2) /)
  newmesh%facecell%fils(ifn+1, 1:2) = (/ icn+1, icn+4 /)         ! neighbours are local CV 1 & 4
  newmesh%face_Ltag%fils(ifn+1, 1)  = 10
  newmesh%face_Rtag%fils(ifn+1, 1)  = 10

  ! internal face 2 (separate CV 4 & 2)
  newmesh%facevtex%fils(ifn+2, 1:2) = (/ facev(2), intv(3) /)
  newmesh%facecell%fils(ifn+2, 1:2) = (/ icn+4, icn+2 /)         ! neighbours are local CV 4 & 2
  newmesh%face_Ltag%fils(ifn+2, 1)  = 11
  newmesh%face_Rtag%fils(ifn+2, 1)  = 11

  ! internal face 3 (separate CV 2 & 5)
  newmesh%facevtex%fils(ifn+3, 1:2) = (/ facev(3), intv(3) /)
  newmesh%facecell%fils(ifn+3, 1:2) = (/ icn+2, icn+5 /)         ! neighbours are local CV 2 & 5
  newmesh%face_Ltag%fils(ifn+3, 1)  = 12
  newmesh%face_Rtag%fils(ifn+3, 1)  = 12

  ! internal face 4 (separate CV 5 & 3)
  newmesh%facevtex%fils(ifn+4, 1:2) = (/ facev(4), intv(4) /)
  newmesh%facecell%fils(ifn+4, 1:2) = (/ icn+5, icn+3 /)         ! neighbours are local CV 5 & 3
  newmesh%face_Ltag%fils(ifn+4, 1)  = 13
  newmesh%face_Rtag%fils(ifn+4, 1)  = 13

  ! internal face 5 (separate CV 3 & 6)
  newmesh%facevtex%fils(ifn+5, 1:2) = (/ facev(5), intv(4) /)
  newmesh%facecell%fils(ifn+5, 1:2) = (/ icn+3, icn+6 /)         ! neighbours are local CV 3 & 6
  newmesh%face_Ltag%fils(ifn+5, 1)  = 14
  newmesh%face_Rtag%fils(ifn+5, 1)  = 14

  ! internal face 6 (separate CV 6 & 1)
  newmesh%facevtex%fils(ifn+6, 1:2) = (/ facev(6), intv(2) /)
  newmesh%facecell%fils(ifn+6, 1:2) = (/ icn+6, icn+1 /)         ! neighbours are local CV 6 & 1
  newmesh%face_Ltag%fils(ifn+6, 1)  = 15
  newmesh%face_Rtag%fils(ifn+6, 1)  = 15

  ! internal face 7 (separate CV 6 & 4)
  newmesh%facevtex%fils(ifn+7, 1:2) = (/ intv(2), intv(1) /)
  newmesh%facecell%fils(ifn+7, 1:2) = (/ icn+6, icn+4 /)         ! neighbours are local CV 6 & 4
  newmesh%face_Ltag%fils(ifn+7, 1)  = 16
  newmesh%face_Rtag%fils(ifn+7, 1)  = 16

  ! internal face 8 (separate CV 4 & 5)
  newmesh%facevtex%fils(ifn+8, 1:2) = (/ intv(3), intv(1) /)
  newmesh%facecell%fils(ifn+8, 1:2) = (/ icn+4, icn+5 /)         ! neighbours are local CV 4 & 5
  newmesh%face_Ltag%fils(ifn+8, 1)  = 17
  newmesh%face_Rtag%fils(ifn+8, 1)  = 17

  ! internal face 9 (separate CV 5 & 6)
  newmesh%facevtex%fils(ifn+9, 1:2) = (/ intv(4), intv(1) /)
  newmesh%facecell%fils(ifn+9, 1:2) = (/ icn+5, icn+6 /)         ! neighbours are local CV 5 & 6
  newmesh%face_Ltag%fils(ifn+9, 1)  = 18
  newmesh%face_Rtag%fils(ifn+9, 1)  = 18

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

  ic0     = (ic-1)*6       ! CV index offset
  facev(1:cell_fvtex%nbfils) = cell_fvtex%fils(ic, 1:cell_fvtex%nbfils)  ! CV face  vertices
  cellv(1:cnv)               = umesh%cellvtex%elem(ielem)%elemvtex(ic, 1:cnv)

  icv  = ic0 + 1 !!!! CV1
  face = (/ cellv(1), facev(1) /)
  call ust_create_face(fnv, icv, face, 1, face_vtex, face_cell, Ltag, Rtag, vtex_face, nfaceofvtex)
  face = (/ facev(6), cellv(1) /)
  call ust_create_face(fnv, icv, face, 9, face_vtex, face_cell, Ltag, Rtag, vtex_face, nfaceofvtex)

  icv  = ic0 + 2 !!!! CV2
  face = (/ facev(2), cellv(2) /)
  call ust_create_face(fnv, icv, face, 3, face_vtex, face_cell, Ltag, Rtag, vtex_face, nfaceofvtex)
  face = (/ cellv(2), facev(3) /)
  call ust_create_face(fnv, icv, face, 4, face_vtex, face_cell, Ltag, Rtag, vtex_face, nfaceofvtex)

  icv  = ic0 + 3 !!!! CV3
  face = (/ facev(4), cellv(3) /)
  call ust_create_face(fnv, icv, face, 6, face_vtex, face_cell, Ltag, Rtag, vtex_face, nfaceofvtex)
  face = (/ cellv(3), facev(5) /)
  call ust_create_face(fnv, icv, face, 7, face_vtex, face_cell, Ltag, Rtag, vtex_face, nfaceofvtex)

  icv  = ic0 + 4 !!!! CV4
  face = (/ facev(1), facev(2) /)
  call ust_create_face(fnv, icv, face, 2, face_vtex, face_cell, Ltag, Rtag, vtex_face, nfaceofvtex)

  icv  = ic0 + 5 !!!! CV5
  face = (/ facev(3), facev(4) /)
  call ust_create_face(fnv, icv, face, 5, face_vtex, face_cell, Ltag, Rtag, vtex_face, nfaceofvtex)

  icv  = ic0 + 6 !!!! CV6
  face = (/ facev(5), facev(6) /)
  call ust_create_face(fnv, icv, face, 8, face_vtex, face_cell, Ltag, Rtag, vtex_face, nfaceofvtex)

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
  newmesh%face_Ltag%fils(iif, 1:nfgauss) = Ltag     %fils(if, 1:nfgauss)
  newmesh%face_Rtag%fils(iif, 1:nfgauss) = Rtag     %fils(if, 1:nfgauss)
enddo

if (ifR /= newmesh%nface_int) &
  call erreur("Spectral Volume Mesh creation", "bad number of internal faces")

!--------------------------------------------------------------------
! delete
call delete(cell_fvtex)


!!$!--------------------------------------------------------------------
!!$! BOCO transfer !!!!!!!!!!!!!!!!!!! a given cell can be neighbour of many BOCOs !!!!!!!!!!!!!!!!!!!!!!!!
!!$

! --- allocate SVM boco ---

call createboco(newmesh, umesh%nboco)

do ib = 1, umesh%nboco
  call new_ustboco(newmesh%boco(ib), umesh%boco(ib)%family, umesh%boco(ib)%nface*defspat%svm%svface_split)
  newmesh%boco(ib)%idefboco = umesh%boco(ib)%idefboco   ! save BOCO index in defsolver
  newmesh%boco(ib)%nface    = 0                         ! reinit face counter
enddo

!!$! --- define SVM boco ---
!!$

allocate(faceboco(newmesh%nface_int+1:newmesh%nface))
faceboco(newmesh%nface_int+1:newmesh%nface) = 0

call print_info(20, "    . creating BOCO tags")

iv0 = umesh%nvtex + umesh%ncell_int * defspat%svm%intnode         ! index offset

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
        rightface = ((CVface(iv) == iv0+(ifsv-1)*2+1).or.(any(CVface(iv)==SVface(1:fnv))).or.(CVface(iv) == iv0+(ifsv-1)*2+2))
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


endsubroutine convert_to_svm_cub

!------------------------------------------------------------------------------!
! Change history
!
! 2008 : created
!------------------------------------------------------------------------------!
