!------------------------------------------------------------------------------!
! Procedure : raffin_iso_tri                Authors : O. Chikhaoui
!
! Fonction 
!   Split triangular cells into 4 isometric triangles
!
!------------------------------------------------------------------------------!
subroutine raffin_iso_tri(defmesh, defspat, umesh, newmesh)

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
integer                :: cellv(8)         ! cell/vtex definition
integer                :: facev(8)         ! CV face vtex definition
integer                :: face (2), CVface(2), SVface(2)         ! face definition
integer                :: intv(8)          ! internal vtex definition
integer                :: ielem, ielemtri, ntri
type(st_connect)       :: cell_fvtex       ! cell to face.midpoint connectivity
type(st_ustmesh)       :: umeshcon
integer, allocatable   :: faceboco(:)      ! face to boco type connectivity
logical                :: rightface

! -- BODY --

call print_info(10, "  . Isotropic (tri) mesh refinement...")

fnv     = 2     ! nb of vertices per face
cnv     = 3     ! nb of vertices per SV cell

!--------------------------------------------------------------------
! initialize and allocate new USTMESH

call init_ustmesh(newmesh, umesh%id)

newmesh%nvtex        = umesh%nvtex + &                                  ! existing vertices
                       umesh%nface                                      ! new face nodes 
newmesh%nface_intsvm = umesh%ncell_int * 3                              ! internal SV faces
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
!   * new face nodes

! -- copy existing vertices --
!
newmesh%mesh%vertex(1:umesh%nvtex, 1, 1) = umesh%mesh%vertex(1:umesh%nvtex, 1, 1) 

! -- check there are only tris --
!
ielem = getindex_genelemvtex(umesh%cellvtex, elem_tri3)
if ((ielem /= 0).and.(umesh%ncell_int /= umesh%cellvtex%elem(ielem)%nelem)) then
  call error_stop("This REFINEMENT can only be used with original TRI cells")
endif


! -- create new SV face nodes (splitting dependant) --
!
iv0 = umesh%nvtex                                            ! index offset

call new_connect(cell_fvtex, umesh%ncell_int, 3)
cell_fvtex%fils(1:cell_fvtex%nbnodes, 1:cell_fvtex%nbfils) = 0

do if = 1, umesh%nface

  facev(1:fnv) = umesh%facevtex%fils(if,1:fnv)
  node = umesh%mesh%vertex(facev(1), 1, 1) + umesh%mesh%vertex(facev(2), 1, 1)
  newmesh%mesh%vertex(iv0+if, 1, 1) = 0.5_krp*node

  ic1 = umesh%facecell%fils(if,1)
  ic2 = umesh%facecell%fils(if,2)
  ! 

  cellv(1:3)= umesh%cellvtex%elem(ielem)%elemvtex(ic1, 1:3)
  if (facev(1).eq.cellv(1)) then
    if (facev(2).eq.cellv(2)) then
      cell_fvtex%fils(ic1,1) = iv0+if
    else 
      cell_fvtex%fils(ic1,3) = iv0+if
    endif  
  elseif(facev(1).eq.cellv(2)) then
    if (facev(2).eq.cellv(3)) then
      cell_fvtex%fils(ic1,2) = iv0+if
    else 
      cell_fvtex%fils(ic1,1) = iv0+if
    endif  
  elseif(facev(1).eq.cellv(3)) then
    if (facev(2).eq.cellv(1)) then
      cell_fvtex%fils(ic1,3) = iv0+if
    else 
      cell_fvtex%fils(ic1,2) = iv0+if
    endif  
  endif

  if (ic2 /= 0) then 
    cellv(1:3) = umesh%cellvtex%elem(ielem)%elemvtex(ic2, 1:3)
    if (facev(1).eq.cellv(1)) then
      if (facev(2).eq.cellv(2)) then
        cell_fvtex%fils(ic2,1) = iv0+if
      else 
        cell_fvtex%fils(ic2,3) = iv0+if
      endif  
    elseif(facev(1).eq.cellv(2)) then
      if (facev(2).eq.cellv(3)) then
        cell_fvtex%fils(ic2,2) = iv0+if
      else 
        cell_fvtex%fils(ic2,1) = iv0+if
      endif  
    elseif(facev(1).eq.cellv(3)) then
      if (facev(2).eq.cellv(1)) then
        cell_fvtex%fils(ic2,3) = iv0+if
      else 
        cell_fvtex%fils(ic2,2) = iv0+if
      endif  
    endif
  endif

enddo

!--------------------------------------------------------------------
! Create CONTROL VOLUMES (CV) as SV subcells

ntri = 4 *umesh%cellvtex%elem(ielem)%nelem               ! define number of  new TRI (TRI => 4TRI)

call addelem_genelemvtex(newmesh%cellvtex)                          ! add a ELEMVTEX section

ielemtri = newmesh%cellvtex%nsection
ntri = 4 *umesh%cellvtex%elem(ielem)%nelem               ! define number of  new TRI (TRI => 4TRI)

call new_elemvtex(newmesh%cellvtex%elem(ielemtri), ntri, elem_tri3)      ! allocation

newmesh%cellvtex%elem(ielemtri)%ielem(1:ntri) = (/ (ic, ic=1, ntri) /)   ! numbering

call print_info(20, "    . creating"//strof(ntri,7)//" CV cells")

do ic = 1, umesh%ncell_int
  icn = (ic-1)*4
  cellv(1:3) = umesh%cellvtex%elem(ielem)%elemvtex(ic, 1:3)                 ! original vertices
  facev(1:cell_fvtex%nbfils) = cell_fvtex%fils(ic, 1:cell_fvtex%nbfils)     ! CV face  vertices
  ! CV 1 : connected to original vertex 1
  newmesh%cellvtex%elem(ielem)%elemvtex(icn+1, 1:3) = (/ cellv(1), facev(1), facev(3) /)
  ! CV 2 : connected to original vertex 2
  newmesh%cellvtex%elem(ielem)%elemvtex(icn+2, 1:3) = (/ cellv(2), facev(2), facev(1) /)
  ! CV 3 : connected to original vertex 3
  newmesh%cellvtex%elem(ielem)%elemvtex(icn+3, 1:3) = (/ cellv(3), facev(3), facev(2) /)
  ! CV 4 : intrenal tri
  newmesh%cellvtex%elem(ielem)%elemvtex(icn+4, 1:3) = (/ facev(1), facev(2), facev(3) /)
enddo

!--------------------------------------------------------------------
! Create new mesh FACES : define face (facevtex) and connectivity (facecell)
!   tags are (SV local) Gauss points index

call new_connect(newmesh%facevtex,  newmesh%nface, 2)
call new_connect(newmesh%facecell,  newmesh%nface, 2)

! --- internal faces ---

call print_info(20, "    . creating"//strof(newmesh%nface_intsvm,7)//" internal CV faces")

do ic = 1, umesh%ncell_int
  ifn     = (ic-1)*3
  icn     = (ic-1)*4
  facev(1:cell_fvtex%nbfils) = cell_fvtex%fils(ic, 1:cell_fvtex%nbfils)  ! CV face  vertices

  ! internal face 1 (separate CV 1 & 4)
  newmesh%facevtex%fils(ifn+1, 1:2) = (/ facev(1), facev(3) /)
  newmesh%facecell%fils(ifn+1, 1:2) = (/ icn+1, icn+4 /)         ! neighbours are local CV 1 & 4
  ! internal face 2 (separate CV 2 & 4)
  newmesh%facevtex%fils(ifn+2, 1:2) = (/ facev(2), facev(1) /)
  newmesh%facecell%fils(ifn+2, 1:2) = (/ icn+2, icn+4 /)         ! neighbours are local CV 2 & 4
  ! internal face 3 (separate CV 3 & 4)
  newmesh%facevtex%fils(ifn+3, 1:2) = (/ facev(3), facev(2) /)
  newmesh%facecell%fils(ifn+3, 1:2) = (/ icn+3, icn+4 /)         ! neighbours are local CV 3 & 4

enddo

! --- Riemann faces from original mesh faces (temporary connectivities) ---

call init_ustmesh(umeshcon, 0)

nRface = newmesh%nface-newmesh%nface_intsvm

call new_connect(umeshcon%facecell, nRface, 2)       ; umeshcon%facecell%nbnodes = 0 ; umeshcon%facecell%fils = 0
call new_connect(umeshcon%facevtex, nRface, fnv)     ; umeshcon%facevtex%nbnodes = 0

call new_genconnect(umeshcon%vtexface, newmesh%nvtex, 10, initdim=0)    ! 10 face per vertex as initial guess
!umeshcon%vtexface%fils(:,:) = 0                           ! initialization

call print_info(20, "    . creating"//strof(nRface,7)//" Riemann  CV faces")

do ic = 1, umesh%ncell_int

  ic0     = (ic-1)*4      ! CV index offset
  facev(1:cell_fvtex%nbfils) = cell_fvtex%fils(ic, 1:cell_fvtex%nbfils)  ! CV face  vertices
  cellv(1:cnv)               = umesh%cellvtex%elem(ielem)%elemvtex(ic, 1:cnv)

  !
  ! -- 'CV 1' Riemann faces --
  icv  = ic0 + 1
  face = (/ facev(3), cellv(1) /)
  call ust_create_face(fnv, icv, face, 0, umeshcon)
  face = (/ cellv(1), facev(1) /)
  call ust_create_face(fnv, icv, face, 0, umeshcon)
  !
  ! -- 'CV 2' Riemann faces --
  icv  = ic0 + 2
  face = (/ facev(1), cellv(2) /)
  call ust_create_face(fnv, icv, face, 0, umeshcon)
  face = (/ cellv(2), facev(2) /)
  call ust_create_face(fnv, icv, face, 0, umeshcon)
  !
  ! -- 'CV 3' Riemann faces --
  icv  = ic0 + 3
  face = (/ facev(2), cellv(3) /)
  call ust_create_face(fnv, icv, face, 0, umeshcon)
  face = (/ cellv(3), facev(3) /)
  call ust_create_face(fnv, icv, face, 0, umeshcon)

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
  newmesh%facecell%fils(iif, 1:2)       = umeshcon%facecell%fils(if, 1:2)
  newmesh%facevtex%fils(iif, 1:fnv)     = umeshcon%facevtex%fils(if, 1:fnv)
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

!!$! --- define bocos ---
!!$

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

  if (ibdef == 0) call erreur("mesh refinement", "BOCO not found for CV face")

  faceboco(if) = ibdef

enddo

! --- redistribute boco defs ---

do if = newmesh%nface_int+1, newmesh%nface
  ib = faceboco(if)
  newmesh%boco(ib)%nface = newmesh%boco(ib)%nface + 1
  newmesh%boco(ib)%iface(newmesh%boco(ib)%nface) = if
enddo

call check_ustmesh_elements(newmesh)

endsubroutine raffin_iso_tri

!------------------------------------------------------------------------------!
! Change history
!
! Jul  2009: created
!------------------------------------------------------------------------------!
