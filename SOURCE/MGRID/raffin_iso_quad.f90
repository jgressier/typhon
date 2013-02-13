!------------------------------------------------------------------------------!
! Procedure : convert_to_svm                Authors : O. Chikhaoui
!
! Fonction 
!   Split quad cells into 4 isometric quads
!
!------------------------------------------------------------------------------!
subroutine raffin_iso_quad(defmesh, defspat, umesh, newmesh)

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
type(v3d)              :: node
integer                :: ic, ic0, icn, if, ifn, iv  ! cell, face, vtex index
integer                :: iv0, ic1, ic2, fnv, cnv, nRface, icv
integer                :: i, iif, ifR, ifl, iv1, iv2, ib, ifb, ibdef, ifsv
integer                :: cellv(4)         ! cell/vtex definition
integer                :: facev(4)         ! CV face vtex definition
integer                :: face(2), CVface(2), SVface(2)         ! face definition
integer                :: intv(2)          ! internal vtex definition
integer                :: nfgauss          ! number of integration points per face (defspat%svm)
integer                :: ielem, ielemtri, ielemquad, nquad
type(st_connect)       :: cell_fvtex       ! cell to face.midpoint connectivity
type(st_ustmesh)       :: umeshcon
integer, allocatable   :: faceboco(:)      ! face to boco type connectivity
logical                :: rightface

! -- BODY --

call print_info(10, "  . Isotropic (quad) mesh refinement...")

fnv     = 2     ! nb of vertices per face
cnv     = 4     ! nb of vertices per SV cell
nfgauss = 1

!--------------------------------------------------------------------
! initialize and allocate new USTMESH

call init_ustmesh(newmesh, umesh%id)

newmesh%nvtex        = umesh%nvtex + &                                  ! existing vertices
                       umesh%ncell_int + &                              ! internal SV nodes
                       umesh%nface                                      ! new face nodes 
newmesh%nface_intsvm = umesh%ncell_int * 4                              ! internal SV faces
newmesh%nface_int    = newmesh%nface_intsvm + &
                       umesh%nface_int * 2
newmesh%nface_lim    = umesh%nface_lim * 2
newmesh%ncell_int    = umesh%ncell_int * 4
newmesh%ncell_lim    = umesh%ncell_lim * 4

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

! -- check there are only quads --
!
ielem = getindex_genelemvtex(umesh%cellvtex, elem_quad4)
if (ielem /= 0) then
  if (umesh%ncell_int /= umesh%cellvtex%elem(ielem)%nelem) &
    call error_stop("This REFINEMENT can only be used with original QUAD cells")
else
  call error_stop("This REFINEMENT can only be used with original QUAD cells")
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

! -- create new SV face nodes (splitting dependant) --
!
iv0 = umesh%nvtex + umesh%ncell_int          ! index offset

call new_connect(cell_fvtex, umesh%ncell_int, 4)
cell_fvtex%fils(1:cell_fvtex%nbnodes, 1:cell_fvtex%nbfils) = 0

do if = 1, umesh%nface

  facev(1:fnv) = umesh%facevtex%fils(if,1:fnv)
  node = umesh%mesh%vertex(facev(1), 1, 1) + umesh%mesh%vertex(facev(2), 1, 1)
  newmesh%mesh%vertex(iv0+if, 1, 1) = 0.5_krp*node

  ic1 = umesh%facecell%fils(if,1)
  ic2 = umesh%facecell%fils(if,2)
  ! 

  cellv(1:4) = umesh%cellvtex%elem(ielem)%elemvtex(ic1, 1:4)
  if (facev(1).eq.cellv(1)) then
    if (facev(2).eq.cellv(2)) then
      cell_fvtex%fils(ic1,1) = iv0+if
    else 
      cell_fvtex%fils(ic1,4) = iv0+if
    endif  
  elseif(facev(1).eq.cellv(2)) then
    if (facev(2).eq.cellv(3)) then
      cell_fvtex%fils(ic1,2) = iv0+if
    else 
      cell_fvtex%fils(ic1,1) = iv0+if
    endif  
  elseif(facev(1).eq.cellv(3)) then
    if (facev(2).eq.cellv(4)) then
      cell_fvtex%fils(ic1,3) = iv0+if
    else 
      cell_fvtex%fils(ic1,2) = iv0+if
    endif  
  elseif(facev(1).eq.cellv(4)) then
    if (facev(2).eq.cellv(1)) then
      cell_fvtex%fils(ic1,4) = iv0+if
    else 
      cell_fvtex%fils(ic1,3) = iv0+if
    endif 
  endif

  if (ic2 /= 0) then 
    cellv(1:4) = umesh%cellvtex%elem(ielem)%elemvtex(ic2, 1:4)
    if (facev(1).eq.cellv(1)) then
      if (facev(2).eq.cellv(2)) then
        cell_fvtex%fils(ic2,1) = iv0+if
      else 
        cell_fvtex%fils(ic2,4) = iv0+if
      endif  
    elseif(facev(1).eq.cellv(2)) then
      if (facev(2).eq.cellv(3)) then
        cell_fvtex%fils(ic2,2) = iv0+if
      else 
        cell_fvtex%fils(ic2,1) = iv0+if
      endif  
    elseif(facev(1).eq.cellv(3)) then
      if (facev(2).eq.cellv(4)) then
        cell_fvtex%fils(ic2,3) = iv0+if
      else 
        cell_fvtex%fils(ic2,2) = iv0+if
      endif  
    elseif(facev(1).eq.cellv(4)) then
      if (facev(2).eq.cellv(1)) then
        cell_fvtex%fils(ic2,4) = iv0+if
      else 
        cell_fvtex%fils(ic2,3) = iv0+if
      endif 
    endif
  endif

enddo

!--------------------------------------------------------------------
! Create CONTROL VOLUMES (CV) as SV subcells

call addelem_genelemvtex(newmesh%cellvtex)                          ! add a ELEMVTEX section

ielemquad = 1
nquad = 4 *umesh%cellvtex%elem(ielem)%nelem               ! define number of QUAD (QUAD => 4 QUAD)

call new_elemvtex(newmesh%cellvtex%elem(ielemquad), nquad, elem_quad4)      ! allocation

newmesh%cellvtex%elem(ielemquad)%ielem(1:nquad) = (/ (ic, ic=1, nquad) /)   ! numbering

call print_info(20, "    . creating"//strof(nquad,7)//" CV cells")

do ic = 1, umesh%ncell_int
  icn = (ic-1)*4
  cellv(1:4) = umesh%cellvtex%elem(ielem)%elemvtex(ic, 1:4)                 ! original vertices
  intv(1)    = umesh%nvtex + ic                                             ! internal vertices
  facev(1:cell_fvtex%nbfils) = cell_fvtex%fils(ic, 1:cell_fvtex%nbfils)     ! CV face  vertices
  ! CV 1 : connected to original vertex 1
  newmesh%cellvtex%elem(ielemquad)%elemvtex(icn+1, 1:4) = (/ cellv(1), facev(1), intv(1), facev(4) /)
  ! CV 2 : connected to original vertex 2
  newmesh%cellvtex%elem(ielemquad)%elemvtex(icn+2, 1:4) = (/ cellv(2), facev(2), intv(1), facev(1) /)
  ! CV 3 : connected to original vertex 3
  newmesh%cellvtex%elem(ielemquad)%elemvtex(icn+3, 1:4) = (/ cellv(3), facev(3), intv(1), facev(2) /)
  ! CV 4 : connected to original vertex 4
  newmesh%cellvtex%elem(ielemquad)%elemvtex(icn+4, 1:4) = (/ cellv(4), facev(4), intv(1), facev(3) /)
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
  ifn     = (ic-1)*4
  intv(1) = umesh%nvtex  + ic                                            ! internal vertices
  facev(1:cell_fvtex%nbfils) = cell_fvtex%fils(ic, 1:cell_fvtex%nbfils)  ! CV face  vertices
  ! internal face 1 (separate CV 1 & 2)
  newmesh%facevtex%fils(ifn+1, 1:2) = (/ intv(1), facev(1) /)
  newmesh%facecell%fils(ifn+1, 1:2) = (/ ifn+1, ifn+2 /)         ! neighbours are local CV 1 & 2
  newmesh%face_Ltag%fils(ifn+1, 1)  = 9
  newmesh%face_Rtag%fils(ifn+1, 1)  = 9
  ! internal face 2 (separate CV 2 & 3)
  newmesh%facevtex%fils(ifn+2, 1:2) = (/ intv(1), facev(2) /)
  newmesh%facecell%fils(ifn+2, 1:2) = (/ ifn+2, ifn+3 /)         ! neighbours are local CV 2 & 3
  newmesh%face_Ltag%fils(ifn+2, 1)  = 10
  newmesh%face_Rtag%fils(ifn+2, 1)  = 10
  ! internal face 3 (separate CV 3 & 4)
  newmesh%facevtex%fils(ifn+3, 1:2) = (/ intv(1), facev(3) /)
  newmesh%facecell%fils(ifn+3, 1:2) = (/ ifn+3, ifn+4 /)         ! neighbours are local CV 3 & 4
  newmesh%face_Ltag%fils(ifn+3, 1)  = 11
  newmesh%face_Rtag%fils(ifn+3, 1)  = 11
  ! internal face 4 (separate CV 4 & 1)
  newmesh%facevtex%fils(ifn+4, 1:2) = (/ intv(1), facev(4) /)
  newmesh%facecell%fils(ifn+4, 1:2) = (/ ifn+4, ifn+1 /)         ! neighbours are local CV 4 & 1
  newmesh%face_Ltag%fils(ifn+4, 1)  = 12
  newmesh%face_Rtag%fils(ifn+4, 1)  = 12
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

  ic0     = (ic-1)*4      ! CV index offset
  facev(1:cell_fvtex%nbfils) = cell_fvtex%fils(ic, 1:cell_fvtex%nbfils)  ! CV face  vertices
  cellv(1:cnv)               = umesh%cellvtex%elem(ielem)%elemvtex(ic, 1:cnv)

  !
  ! -- 'CV 1' Riemann faces --
  icv  = ic0 + 1
  face = (/ facev(4), cellv(1) /)
  call ust_create_face(fnv, icv, face, 8, umeshcon)
  face = (/ cellv(1), facev(1) /)
  call ust_create_face(fnv, icv, face, 1, umeshcon)
  !
  ! -- 'CV 2' Riemann faces --
  icv  = ic0 + 2
  face = (/ facev(1), cellv(2) /)
  call ust_create_face(fnv, icv, face, 2, umeshcon)
  face = (/ cellv(2), facev(2) /)
  call ust_create_face(fnv, icv, face, 3, umeshcon)
  !
  ! -- 'CV 3' Riemann faces --
  icv  = ic0 + 3
  face = (/ facev(2), cellv(3) /)
  call ust_create_face(fnv, icv, face, 4, umeshcon)
  face = (/ cellv(3), facev(3) /)
  call ust_create_face(fnv, icv, face, 5, umeshcon)
  !
  ! -- 'CV 4' Riemann faces --
  icv  = ic0 + 4
  face = (/ facev(3), cellv(4) /)
  call ust_create_face(fnv, icv, face, 6, umeshcon)
  face = (/ cellv(4), facev(4) /)
  call ust_create_face(fnv, icv, face, 7, umeshcon)

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
  call new_ustboco(newmesh%boco(ib), umesh%boco(ib)%family, umesh%boco(ib)%nface*2)
  newmesh%boco(ib)%idefboco = umesh%boco(ib)%idefboco   ! save BOCO index in defsolver
  newmesh%boco(ib)%nface    = 0                         ! reinit face counter
enddo

allocate(faceboco(newmesh%nface_int+1:newmesh%nface))
faceboco(newmesh%nface_int+1:newmesh%nface) = 0

call print_info(20, "    . creating BOCO tags")

iv0 = umesh%nvtex + umesh%ncell_int

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

call check_ustmesh_elements(newmesh)

endsubroutine raffin_iso_quad

!------------------------------------------------------------------------------!
! Change history
!
! Oct  2011: created
!------------------------------------------------------------------------------!
