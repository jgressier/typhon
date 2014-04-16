!------------------------------------------------------------------------------!
! Procedure : convert_to_svm_4wang           
!                                           Date    :  JUNE 2008
! Fonction 
!   Split cells into 9quad and 1tri (defined as an hexa) Spectral Volume subcells
!
!------------------------------------------------------------------------------!
subroutine convert_to_svm_4wang(defmesh, defspat, umesh, newmesh)

use OUTPUT
use USTMESH
use MESHBASE
use MESHPARAMS
use MESHCONNECT
use MENU_NUM

implicit none

! -- INPUTS  --
type(mnu_mesh)        :: defmesh      ! mesh parameters
type(mnu_spat)        :: defspat      ! spatial numerical method parameters
type(st_ustmesh)      :: umesh        ! ustmesh to convert

! -- OUTPUTS --
type(st_ustmesh)      :: newmesh      ! new ustmesh (split and converted)

! -- Internal variables --
type(v3d)              :: node,dx1,dx2
integer                :: ic, ic0, icn, if, ifn, iv ,icnh,icnq ! cell, face, vtex index
integer                :: iv0, ic1, ic2, fnv, cnv, nRface, icv
integer                :: i, iif, ifR, ifl, iv1, iv2, ib, ifb, ibdef, ifsv
integer                :: cellv(8)         ! cell/vtex definition
integer                :: facev(9)         ! CV face vtex definition
integer                :: face (2), CVface(2), SVface(2)         ! face definition
integer                :: intv(8)          ! internal vtex definition
integer                :: nfgauss          ! number of integration points per face (defspat%svm)
integer                :: ielem, ielemquad, nquad, ielemhexa, nhexa
type(st_connect)       :: cell_fvtex       ! cell to face.midpoint connectivity
type(st_ustmesh)       :: umeshcon
integer, allocatable   :: faceboco(:)      ! face to boco type connectivity
logical                :: rightface
real(krp)              :: alpha, beta      ! geometric coefficient for the split
real(krp)              :: sqrt3,det

! -- BODY --

alpha = 1._krp / 15._krp
beta = 2._krp / 15._krp
sqrt3 = sqrt(3._krp)

call print_info(10, "  . converting to SVM mesh...")

fnv     = 2     ! nb of vertices per face
cnv     = 3     ! nb of vertices per SV cell
nfgauss = defspat%svm%nfgauss


! -- check there are only tri --
!
ielem = getindex_genelemvtex(umesh%cellvtex, elem_tri3)
if (ielem /= 0) then
  if (umesh%ncell_int /= umesh%cellvtex%elem(ielem)%nelem) &
    call error_stop("This REFINEMENT can only be used with original TRI cells (there are not only tri)")
else
  call error_stop("This REFINEMENT can only be used with original TRI cells (there is no tri)")
endif

!--------------------------------------------------------------------
! initialize and allocate new USTMESH

call init_ustmesh(newmesh, umesh%id)

newmesh%nvtex        = umesh%nvtex + &                                  ! existing vertices
                       umesh%ncell_int * defmesh%defsplit%intnode + &        ! internal SV nodes
                       umesh%nface     * (defmesh%defsplit%svface_split-1)   ! new face nodes 
newmesh%nface_intsvm = umesh%ncell_int * defmesh%defsplit%internal_faces     ! internal SV faces
newmesh%nface_int    = newmesh%nface_intsvm + &
                       umesh%nface_int * defmesh%defsplit%svface_split
newmesh%nface_lim    = umesh%nface_lim * defmesh%defsplit%svface_split
newmesh%ncell_int    = umesh%ncell_int * defmesh%defsplit%subcell
newmesh%ncell_lim    = umesh%ncell_lim * defmesh%defsplit%subcell

newmesh%nface = newmesh%nface_int + newmesh%nface_lim
newmesh%ncell = newmesh%ncell_int + newmesh%ncell_lim

newmesh%mesh%info = umesh%mesh%info

! -- allocate only vertices array --
!
call new_mesh(newmesh%mesh, 0, 0, newmesh%nvtex) 


! -- if needed, compute gradient interpolation coefficients for gradsvm --
if(defspat%gradmeth.eq.facegrad_svm)then
 call alloc_mesh_metricsvm(newmesh%mesh, 4*umesh%cellvtex%elem(ielem)%nelem)
do ic = 1, umesh%cellvtex%elem(ielem)%nelem
   
  iv0  = umesh%cellvtex%elem(ielem)%elemvtex(ic,1)
  iv1  = umesh%cellvtex%elem(ielem)%elemvtex(ic,2)
  iv2  = umesh%cellvtex%elem(ielem)%elemvtex(ic,3)

  dx1   = umesh%mesh%vertex(iv1, 1, 1)- umesh%mesh%vertex(iv0, 1, 1)       ! first face of cell
  dx2   = umesh%mesh%vertex(iv2, 1, 1)- umesh%mesh%vertex(iv0, 1, 1)       ! second face of cell
  det   = 1._krp/(dx1%x * dx2%y - dx1%y * dx2%x) 

 newmesh%mesh%metricsvm(4*(ic-1)+1,1,1)= det * (dx2%y - 0.5 * dx1%y) 
 newmesh%mesh%metricsvm(4*(ic-1)+2,1,1)= det * (-sqrt3 * 0.5 * dx1%y)
 newmesh%mesh%metricsvm(4*(ic-1)+3,1,1)= det * (dx2%x - 0.5 * dx1%x)
 newmesh%mesh%metricsvm(4*(ic-1)+4,1,1)= det * (sqrt3 * 0.5 * dx1%x)

enddo
endif


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

iv0 = umesh%nvtex                     ! index offset 

do ic = 1, umesh%cellvtex%elem(ielem)%nelem
  iv   = umesh%cellvtex%elem(ielem)%elemvtex(ic,1)          ! first node of cell
  node = umesh%mesh%vertex(iv, 1, 1)                        ! initialize sum of nodes
  do i = 2, umesh%cellvtex%elem(ielem)%nvtex                ! loop on nodes left (and sum)
    iv   = umesh%cellvtex%elem(ielem)%elemvtex(ic,i)
    node = node + umesh%mesh%vertex(iv, 1, 1)
  enddo
  node = node /  real(umesh%cellvtex%elem(ielem)%nvtex, krp)

  do i = 1, umesh%cellvtex%elem(ielem)%nvtex                ! loop on nodes left 
  iv   = umesh%cellvtex%elem(ielem)%elemvtex(ic,i)
  newmesh%mesh%vertex(iv0+(ic-1)*3+i, 1, 1) = (1._krp - 3._krp / 2._krp * beta) * umesh%mesh%vertex(iv, 1, 1) &                                   
       + 3._krp / 2._krp * beta * node
  enddo
enddo


iv0 = umesh%nvtex
iv  = umesh%nvtex + umesh%ncell_int * (defmesh%defsplit%intnode -3)        ! index offset
do ic = 1, umesh%cellvtex%elem(ielem)%nelem
  newmesh%mesh%vertex(iv+(ic-1)*3+1, 1, 1) = 0.5_krp * (newmesh%mesh%vertex(iv0+(ic-1)*3+1, 1, 1)&
                                                      + newmesh%mesh%vertex(iv0+(ic-1)*3+2, 1, 1))
  newmesh%mesh%vertex(iv+(ic-1)*3+2, 1, 1) = 0.5_krp * (newmesh%mesh%vertex(iv0+(ic-1)*3+2, 1, 1)&
                                                      + newmesh%mesh%vertex(iv0+(ic-1)*3+3, 1, 1))
  newmesh%mesh%vertex(iv+(ic-1)*3+3, 1, 1) = 0.5_krp * (newmesh%mesh%vertex(iv0+(ic-1)*3+3, 1, 1)&
                                                      + newmesh%mesh%vertex(iv0+(ic-1)*3+1, 1, 1))
enddo

! -- create new SV face nodes --
!
iv0 = umesh%nvtex + umesh%ncell_int * defmesh%defsplit%intnode         ! index offset

call new_connect(cell_fvtex, umesh%ncell_int, 3*(defmesh%defsplit%svface_split-1))
cell_fvtex%fils(1:cell_fvtex%nbnodes, 1:cell_fvtex%nbfils) = 0

do if = 1, umesh%nface
  facev(1:fnv) = umesh%facevtex%fils(if,1:fnv)
  newmesh%mesh%vertex(iv0+(if-1)*3+1, 1, 1) = (1._krp - alpha ) * umesh%mesh%vertex(facev(1), 1, 1) & !1st point
                                               + alpha * umesh%mesh%vertex(facev(2), 1, 1)

  newmesh%mesh%vertex(iv0+(if-1)*3+2, 1, 1) = 0.5_krp * umesh%mesh%vertex(facev(2), 1, 1) & !middle point
                                               + 0.5_krp * umesh%mesh%vertex(facev(1), 1, 1)

  newmesh%mesh%vertex(iv0+(if-1)*3+3, 1, 1) = (1._krp - alpha ) * umesh%mesh%vertex(facev(2), 1, 1) & !3nd point
                                               + alpha * umesh%mesh%vertex(facev(1), 1, 1)
  ! 

  ic1 = umesh%facecell%fils(if,1)
  ic2 = umesh%facecell%fils(if,2)

  cellv(1:3)= umesh%cellvtex%elem(ielem)%elemvtex(ic1, 1:3)
  if (facev(1).eq.cellv(1)) then
    if (facev(2).eq.cellv(2)) then
  cell_fvtex%fils(ic1,1) = iv0+(if-1)*3+1
  cell_fvtex%fils(ic1,2) = iv0+(if-1)*3+2
  cell_fvtex%fils(ic1,3) = iv0+(if-1)*3+3
    else 
  cell_fvtex%fils(ic1,9) = iv0+(if-1)*3+1
  cell_fvtex%fils(ic1,8) = iv0+(if-1)*3+2
  cell_fvtex%fils(ic1,7) = iv0+(if-1)*3+3
    endif  
  elseif(facev(1).eq.cellv(2)) then
    if (facev(2).eq.cellv(3)) then
  cell_fvtex%fils(ic1,4) = iv0+(if-1)*3+1
  cell_fvtex%fils(ic1,5) = iv0+(if-1)*3+2
  cell_fvtex%fils(ic1,6) = iv0+(if-1)*3+3
    else 
  cell_fvtex%fils(ic1,3) = iv0+(if-1)*3+1
  cell_fvtex%fils(ic1,2) = iv0+(if-1)*3+2
  cell_fvtex%fils(ic1,1) = iv0+(if-1)*3+3
    endif  
  elseif(facev(1).eq.cellv(3)) then
    if (facev(2).eq.cellv(1)) then
  cell_fvtex%fils(ic1,7) = iv0+(if-1)*3+1
  cell_fvtex%fils(ic1,8) = iv0+(if-1)*3+2
  cell_fvtex%fils(ic1,9) = iv0+(if-1)*3+3
    else 
  cell_fvtex%fils(ic1,6) = iv0+(if-1)*3+1
  cell_fvtex%fils(ic1,5) = iv0+(if-1)*3+2
  cell_fvtex%fils(ic1,4) = iv0+(if-1)*3+3
    endif  
   endif

  if (ic2 /= 0) then 
  cellv(1:3)= umesh%cellvtex%elem(ielem)%elemvtex(ic2, 1:3)
  if (facev(1).eq.cellv(1)) then
    if (facev(2).eq.cellv(2)) then
  cell_fvtex%fils(ic2,1) = iv0+(if-1)*3+1
  cell_fvtex%fils(ic2,2) = iv0+(if-1)*3+2
  cell_fvtex%fils(ic2,3) = iv0+(if-1)*3+3
    else 
  cell_fvtex%fils(ic2,9) = iv0+(if-1)*3+1
  cell_fvtex%fils(ic2,8) = iv0+(if-1)*3+2
  cell_fvtex%fils(ic2,7) = iv0+(if-1)*3+3
    endif  
  elseif(facev(1).eq.cellv(2)) then
    if (facev(2).eq.cellv(3)) then
  cell_fvtex%fils(ic2,4) = iv0+(if-1)*3+1
  cell_fvtex%fils(ic2,5) = iv0+(if-1)*3+2
  cell_fvtex%fils(ic2,6) = iv0+(if-1)*3+3
    else 
  cell_fvtex%fils(ic2,3) = iv0+(if-1)*3+1
  cell_fvtex%fils(ic2,2) = iv0+(if-1)*3+2
  cell_fvtex%fils(ic2,1) = iv0+(if-1)*3+3
    endif  
  elseif(facev(1).eq.cellv(3)) then
    if (facev(2).eq.cellv(1)) then
  cell_fvtex%fils(ic2,7) = iv0+(if-1)*3+1
  cell_fvtex%fils(ic2,8) = iv0+(if-1)*3+2
  cell_fvtex%fils(ic2,9) = iv0+(if-1)*3+3
    else 
  cell_fvtex%fils(ic2,6) = iv0+(if-1)*3+1
  cell_fvtex%fils(ic2,5) = iv0+(if-1)*3+2
  cell_fvtex%fils(ic2,4) = iv0+(if-1)*3+3
    endif  
   endif
  endif
enddo


!--------------------------------------------------------------------
! Create CONTROL VOLUMES (CV) as SV subcells


call addelem_genelemvtex(newmesh%cellvtex)                          ! add a ELEMVTEX section
call addelem_genelemvtex(newmesh%cellvtex)                          ! add a ELEMVTEX section

ielemquad = 1
ielemhexa = 2
nquad = (defmesh%defsplit%subcell -1) * umesh%cellvtex%elem(ielem)%nelem               ! define number of QUAD and PENT
nhexa =     umesh%cellvtex%elem(ielem)%nelem

call new_elemvtex(newmesh%cellvtex%elem(ielemquad), nquad, elem_quad4)      ! allocation
call new_elemvtex(newmesh%cellvtex%elem(ielemhexa), nhexa, elem_ngon, 6) 


do ic=1,umesh%ncell_int 
  icnq = (ic-1) * (defmesh%defsplit%subcell -1)
  icnh = (ic-1)
newmesh%cellvtex%elem(ielemquad)%ielem(icnq+1) = (ic-1) * defmesh%defsplit%subcell + 1
newmesh%cellvtex%elem(ielemquad)%ielem(icnq+2) = (ic-1) * defmesh%defsplit%subcell + 2
newmesh%cellvtex%elem(ielemquad)%ielem(icnq+3) = (ic-1) * defmesh%defsplit%subcell + 3
newmesh%cellvtex%elem(ielemquad)%ielem(icnq+4) = (ic-1) * defmesh%defsplit%subcell + 4
newmesh%cellvtex%elem(ielemquad)%ielem(icnq+5) = (ic-1) * defmesh%defsplit%subcell + 5
newmesh%cellvtex%elem(ielemquad)%ielem(icnq+6) = (ic-1) * defmesh%defsplit%subcell + 6
newmesh%cellvtex%elem(ielemquad)%ielem(icnq+7) = (ic-1) * defmesh%defsplit%subcell + 7
newmesh%cellvtex%elem(ielemquad)%ielem(icnq+8) = (ic-1) * defmesh%defsplit%subcell + 8
newmesh%cellvtex%elem(ielemquad)%ielem(icnq+9) = (ic-1) * defmesh%defsplit%subcell + 9
newmesh%cellvtex%elem(ielemhexa)%ielem(icnh+1) = (ic-1) * defmesh%defsplit%subcell + 10
enddo

call print_info(20, "    . creating"//strof(nquad + nhexa)//" CV cells")


do ic = 1, umesh%ncell_int
  icnq = (ic-1) * (defmesh%defsplit%subcell -1)
  icnh = (ic-1)
  cellv(1:3) = umesh%cellvtex%elem(ielem)%elemvtex(ic, 1:3)           ! original vertices                           
  intv(1)    = umesh%nvtex + (ic-1) * (defmesh%defsplit%intnode - 3) + 1                         ! 3 first internal vertices
  intv(2)    = umesh%nvtex + (ic-1) * (defmesh%defsplit%intnode - 3) + 2
  intv(3)    = umesh%nvtex + (ic-1) * (defmesh%defsplit%intnode - 3) + 3
  intv(4)    = umesh%nvtex + umesh%ncell_int * (defmesh%defsplit%intnode -3) + (ic-1) * (defmesh%defsplit%intnode - 3) + 1! 3 other internal vertices
  intv(5)    = umesh%nvtex + umesh%ncell_int * (defmesh%defsplit%intnode -3) + (ic-1) * (defmesh%defsplit%intnode - 3) + 2
  intv(6)    = umesh%nvtex + umesh%ncell_int * (defmesh%defsplit%intnode -3) + (ic-1) * (defmesh%defsplit%intnode - 3) + 3
  facev(1:cell_fvtex%nbfils) = cell_fvtex%fils(ic, 1:cell_fvtex%nbfils)     ! CV face  vertices

  ! CV 1 : connected to original vertex 1
  newmesh%cellvtex%elem(ielemquad)%elemvtex(icnq+1, 1:4) = (/ cellv(1), facev(1), intv(1), facev(9) /)
  ! CV 2 : connected to original vertex 2
  newmesh%cellvtex%elem(ielemquad)%elemvtex(icnq+2, 1:4) = (/ cellv(2), facev(4), intv(2), facev(3) /)
  ! CV 3 : connected to original vertex 3
  newmesh%cellvtex%elem(ielemquad)%elemvtex(icnq+3, 1:4) = (/ cellv(3), facev(7), intv(3), facev(6) /)
  ! CV 4 (between CV1 and CV5)
  newmesh%cellvtex%elem(ielemquad)%elemvtex(icnq+4, 1:4) = (/ intv(1), facev(1), facev(2), intv(4) /)
  ! CV 5 (between CV4 and CV1)
  newmesh%cellvtex%elem(ielemquad)%elemvtex(icnq+5, 1:4) = (/ intv(4), facev(2), facev(3), intv(2) /)
  ! CV 6 (between CV2 and CV7)
  newmesh%cellvtex%elem(ielemquad)%elemvtex(icnq+6, 1:4) = (/ intv(2), facev(4), facev(5), intv(5) /)
  ! CV 7 (between CV6 and CV3)
  newmesh%cellvtex%elem(ielemquad)%elemvtex(icnq+7, 1:4) = (/ intv(5), facev(5), facev(6), intv(3) /)
  ! CV 8 (between CV3 and CV9)
  newmesh%cellvtex%elem(ielemquad)%elemvtex(icnq+8, 1:4) = (/ intv(3), facev(7), facev(8), intv(6) /)
  ! CV 9 (between CV8 and CV1)
  newmesh%cellvtex%elem(ielemquad)%elemvtex(icnq+9, 1:4) = (/ intv(6), facev(8), facev(9), intv(1) /)
  ! CV 10 (Tri Cell in the middle of the SV defined as a hexa)
  newmesh%cellvtex%elem(ielemhexa)%elemvtex(icnh+1, 1:6) = (/ intv(1), intv(4), intv(2), intv(5), intv(3), intv(6) /)
enddo

!--------------------------------------------------------------------
! Create new mesh FACES : define face (facevtex) and connectivity (facecell)
!   tags are (SV local) Gauss points index

call new_connect(newmesh%facevtex,  newmesh%nface, 2)
call new_connect(newmesh%facecell,  newmesh%nface, 2)
call new_connect(newmesh%face_Ltag, newmesh%nface, nfgauss) ; newmesh%face_Ltag%fils(:,:) = 0
call new_connect(newmesh%face_Rtag, newmesh%nface, nfgauss) ; newmesh%face_Rtag%fils(:,:) = 0

! --- internal faces ---

call print_info(20, "    . creating"//strof(newmesh%nface_intsvm)//" internal CV faces")

do ic = 1, umesh%ncell_int
  ifn     = (ic-1)* defmesh%defsplit%internal_faces
  icn     = (ic-1) * defmesh%defsplit%subcell
  intv(1)    = umesh%nvtex + (ic-1) * (defmesh%defsplit%intnode - 3) + 1 !3 first internal vertices
  intv(2)    = umesh%nvtex + (ic-1) * (defmesh%defsplit%intnode - 3) + 2
  intv(3)    = umesh%nvtex + (ic-1) * (defmesh%defsplit%intnode - 3) + 3
  intv(4)    = umesh%nvtex + umesh%ncell_int * (defmesh%defsplit%intnode -3) + (ic-1) * (defmesh%defsplit%intnode - 3) + 1! 3 other internal vertices
  intv(5)    = umesh%nvtex + umesh%ncell_int * (defmesh%defsplit%intnode -3) + (ic-1) * (defmesh%defsplit%intnode - 3) + 2
  intv(6)    = umesh%nvtex + umesh%ncell_int * (defmesh%defsplit%intnode -3) + (ic-1) * (defmesh%defsplit%intnode - 3) + 3
  facev(1:cell_fvtex%nbfils) = cell_fvtex%fils(ic, 1:cell_fvtex%nbfils)     ! CV face  vertices

  ! internal face 1 (separate CV 1 & 4)
  newmesh%facevtex%fils(ifn+1, 1:2) = (/ facev(1), intv(1) /)
  newmesh%facecell%fils(ifn+1, 1:2) = (/ icn+1, icn+4 /)         ! neighbours are local CV 1 & 4
  newmesh%face_Ltag%fils(ifn+1, 1)  = 13
  newmesh%face_Rtag%fils(ifn+1, 1)  = 13
  ! internal face 2 (separate CV 4 & 5)
  newmesh%facevtex%fils(ifn+2, 1:2) = (/ facev(2), intv(4) /)
  newmesh%facecell%fils(ifn+2, 1:2) = (/ icn+4, icn+5 /)         ! neighbours are local CV 4 & 5
  newmesh%face_Ltag%fils(ifn+2, 1)  = 14
  newmesh%face_Rtag%fils(ifn+2, 1)  = 14
  ! internal face 3 (separate CV 5 & 2)
  newmesh%facevtex%fils(ifn+3, 1:2) = (/ facev(3), intv(2) /)
  newmesh%facecell%fils(ifn+3, 1:2) = (/ icn+5, icn+2 /)         ! neighbours are local CV 5 & 2
  newmesh%face_Ltag%fils(ifn+3, 1)  = 15
  newmesh%face_Rtag%fils(ifn+3, 1)  = 15
  ! internal face 4 (separate CV 2 & 6)
  newmesh%facevtex%fils(ifn+4, 1:2) = (/ facev(4), intv(2) /)
  newmesh%facecell%fils(ifn+4, 1:2) = (/ icn+2, icn+6 /)         ! neighbours are local CV 2 & 6
  newmesh%face_Ltag%fils(ifn+4, 1)  = 16
  newmesh%face_Rtag%fils(ifn+4, 1)  = 16
  ! internal face 5 (separate CV 6 & 7)
  newmesh%facevtex%fils(ifn+5, 1:2) = (/ facev(5), intv(5) /)
  newmesh%facecell%fils(ifn+5, 1:2) = (/ icn+6, icn+7 /)         ! neighbours are local CV 6 & 7
  newmesh%face_Ltag%fils(ifn+5, 1)  = 17
  newmesh%face_Rtag%fils(ifn+5, 1)  = 17
  ! internal face 6 (separate CV 7 & 3)
  newmesh%facevtex%fils(ifn+6, 1:2) = (/ facev(6), intv(3) /)
  newmesh%facecell%fils(ifn+6, 1:2) = (/ icn+7, icn+3 /)         ! neighbours are local CV 7 & 3
  newmesh%face_Ltag%fils(ifn+6, 1)  = 18
  newmesh%face_Rtag%fils(ifn+6, 1)  = 18
  ! internal face 7 (separate CV 3 & 8)
  newmesh%facevtex%fils(ifn+7, 1:2) = (/ facev(7), intv(3) /)
  newmesh%facecell%fils(ifn+7, 1:2) = (/ icn+3, icn+8 /)         ! neighbours are local CV 3 & 8
  newmesh%face_Ltag%fils(ifn+7, 1)  = 19
  newmesh%face_Rtag%fils(ifn+7, 1)  = 19
  ! internal face 8 (separate CV 8 & 9)
  newmesh%facevtex%fils(ifn+8, 1:2) = (/ facev(8), intv(6) /)
  newmesh%facecell%fils(ifn+8, 1:2) = (/ icn+8, icn+9 /)         ! neighbours are local CV 8 & 9
  newmesh%face_Ltag%fils(ifn+8, 1)  = 20
  newmesh%face_Rtag%fils(ifn+8, 1)  = 20
  ! internal face 9 (separate CV 9 & 1)
  newmesh%facevtex%fils(ifn+9, 1:2) = (/ facev(9), intv(1) /)
  newmesh%facecell%fils(ifn+9, 1:2) = (/ icn+9, icn+1 /)         ! neighbours are local CV 9 & 1
  newmesh%face_Ltag%fils(ifn+9, 1)  = 21
  newmesh%face_Rtag%fils(ifn+9, 1)  = 21
  ! internal face 10 (separate CV 4 & 10)
  newmesh%facevtex%fils(ifn+10, 1:2) = (/ intv(1), intv(4) /)
  newmesh%facecell%fils(ifn+10, 1:2) = (/ icn+4, icn+10 /)         ! neighbours are local CV 4 & 10
  newmesh%face_Ltag%fils(ifn+10, 1)  = 22
  newmesh%face_Rtag%fils(ifn+10, 1)  = 22
  ! internal face 11 (separate CV 5 & 10)
  newmesh%facevtex%fils(ifn+11, 1:2) = (/ intv(4), intv(2) /)
  newmesh%facecell%fils(ifn+11, 1:2) = (/ icn+5, icn+10 /)         ! neighbours are local CV 5 & 10
  newmesh%face_Ltag%fils(ifn+11, 1)  = 23
  newmesh%face_Rtag%fils(ifn+11, 1)  = 23
  ! internal face 12 (separate CV 6 & 10)
  newmesh%facevtex%fils(ifn+12, 1:2) = (/ intv(2), intv(5) /)
  newmesh%facecell%fils(ifn+12, 1:2) = (/ icn+6, icn+10 /)         ! neighbours are local CV 6 & 10
  newmesh%face_Ltag%fils(ifn+12, 1)  = 24
  newmesh%face_Rtag%fils(ifn+12, 1)  = 24
  ! internal face 13 (separate CV 7 & 10)
  newmesh%facevtex%fils(ifn+13, 1:2) = (/ intv(5), intv(3) /)
  newmesh%facecell%fils(ifn+13, 1:2) = (/ icn+7, icn+10 /)         ! neighbours are local CV 7 & 10
  newmesh%face_Ltag%fils(ifn+13, 1)  = 25
  newmesh%face_Rtag%fils(ifn+13, 1)  = 25
  ! internal face 14 (separate CV 8 & 10)
  newmesh%facevtex%fils(ifn+14, 1:2) = (/ intv(3), intv(6) /)
  newmesh%facecell%fils(ifn+14, 1:2) = (/ icn+8, icn+10 /)         ! neighbours are local CV 8 & 10
  newmesh%face_Ltag%fils(ifn+14, 1)  = 26
  newmesh%face_Rtag%fils(ifn+14, 1)  = 26
  ! internal face 15 (separate CV 9 & 10)
  newmesh%facevtex%fils(ifn+15, 1:2) = (/ intv(6), intv(1) /)
  newmesh%facecell%fils(ifn+15, 1:2) = (/ icn+9, icn+10 /)         ! neighbours are local CV 9 & 10
  newmesh%face_Ltag%fils(ifn+15, 1)  = 27
  newmesh%face_Rtag%fils(ifn+15, 1)  = 27
enddo

! --- Riemann faces from original mesh faces (temporary connectivities) ---

call init_ustmesh(umeshcon, 0)

nRface = newmesh%nface-newmesh%nface_intsvm

call new_connect(umeshcon%facecell, nRface, 2)       ; umeshcon%facecell%nbnodes = 0 ; umeshcon%facecell%fils = 0
call new_connect(umeshcon%facevtex, nRface, fnv)     ; umeshcon%facevtex%nbnodes = 0
call new_connect(umeshcon%face_Ltag,      nRface, nfgauss) ; umeshcon%face_Ltag%fils(:,:) = 0
call new_connect(umeshcon%face_Rtag,      nRface, nfgauss) ; umeshcon%face_Rtag%fils(:,:) = 0

call new_genconnect(umeshcon%vtexface, newmesh%nvtex, 10, initdim=0)    ! 10 face per vertex as initial guess
!umeshcon%vtexface%fils(:,:) = 0                           ! initialization

call print_info(20, "    . creating"//strof(nRface)//" Riemann  CV faces")

do ic = 1, umesh%ncell_int

  ic0     = (ic-1)* defmesh%defsplit%subcell       ! CV index offset
  facev(1:cell_fvtex%nbfils) = cell_fvtex%fils(ic, 1:cell_fvtex%nbfils)  ! CV face  vertices
  cellv(1:cnv)               = umesh%cellvtex%elem(ielem)%elemvtex(ic, 1:cnv)

  icv  = ic0 + 1 !!!! CV1
  face = (/ cellv(1), facev(1) /)
  call ust_create_face(fnv, icv, face, 1, umeshcon)
  face = (/ facev(9), cellv(1) /)
  call ust_create_face(fnv, icv, face, 12, umeshcon)

  icv  = ic0 + 2 !!!! CV2
  face = (/ facev(3), cellv(2) /)
  call ust_create_face(fnv, icv, face, 4, umeshcon)
  face = (/ cellv(2), facev(4) /)
  call ust_create_face(fnv, icv, face, 5, umeshcon)

  icv  = ic0 + 3 !!!! CV3
  face = (/ facev(6), cellv(3) /)
  call ust_create_face(fnv, icv, face, 8, umeshcon)
  face = (/ cellv(3), facev(7) /)
  call ust_create_face(fnv, icv, face, 9, umeshcon)

  icv  = ic0 + 4 !!!! CV4
  face = (/ facev(1), facev(2) /)
  call ust_create_face(fnv, icv, face, 2, umeshcon)

  icv  = ic0 + 5 !!!! CV5
  face = (/ facev(2), facev(3) /)
  call ust_create_face(fnv, icv, face, 3, umeshcon)

  icv  = ic0 + 6 !!!! CV6
  face = (/ facev(4), facev(5) /)
  call ust_create_face(fnv, icv, face, 6, umeshcon)

  icv  = ic0 + 7 !!!! CV7
  face = (/ facev(5), facev(6) /)
  call ust_create_face(fnv, icv, face, 7, umeshcon)

  icv  = ic0 + 8 !!!! CV8
  face = (/ facev(7), facev(8) /)
  call ust_create_face(fnv, icv, face, 10, umeshcon)

  icv  = ic0 + 9 !!!! CV9
  face = (/ facev(8), facev(9) /)
  call ust_create_face(fnv, icv, face, 11, umeshcon)

enddo

! --- check created faces ---

if (count(umeshcon%facecell%fils(:,2) == 0) /= newmesh%nface_lim) &
  call erreur("Spectral Volume Mesh creation", "bad number of boundering faces")

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
  newmesh%facecell %fils(iif, 1:2)       = umeshcon%facecell%fils(if, 1:2)
  newmesh%facevtex %fils(iif, 1:fnv)     = umeshcon%facevtex%fils(if, 1:fnv)
  newmesh%face_Ltag%fils(iif, 1:nfgauss) = umeshcon%face_Ltag     %fils(if, 1:nfgauss)
  newmesh%face_Rtag%fils(iif, 1:nfgauss) = umeshcon%face_Rtag     %fils(if, 1:nfgauss)
enddo

if (ifR /= newmesh%nface_int) &
  call erreur("Spectral Volume Mesh creation", "bad number of internal faces")

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
  call new_ustboco(newmesh%boco(ib), umesh%boco(ib)%family, umesh%boco(ib)%nface*defmesh%defsplit%svface_split)
  newmesh%boco(ib)%idefboco = umesh%boco(ib)%idefboco   ! save BOCO index in defsolver
  newmesh%boco(ib)%nface    = 0                         ! reinit face counter
enddo

!!$! --- define SVM boco ---
!!$

allocate(faceboco(newmesh%nface_int+1:newmesh%nface))
faceboco(newmesh%nface_int+1:newmesh%nface) = 0

call print_info(20, "    . creating BOCO tags")

iv0 = umesh%nvtex + umesh%ncell_int * defmesh%defsplit%intnode         ! index offset

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
        rightface = ((CVface(iv) == iv0+(ifsv-1)*3+1).or.(any(CVface(iv)==SVface(1:fnv)))&
                 .or.(CVface(iv) == iv0+(ifsv-1)*3+2).or.(CVface(iv) == iv0+(ifsv-1)*3+3))
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

call check_ustmesh_elements(newmesh)

endsubroutine convert_to_svm_4wang
!------------------------------------------------------------------------------!
! Change history
!
! 2008 : created
!------------------------------------------------------------------------------!
