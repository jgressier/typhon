!------------------------------------------------------------------------------!
! Procedure : extractpart_grid            Authors : J. Gressier
!                                         Date    : Oct. 2005
! Fonction 
!   METIS split : compute partition of a grid
!
!------------------------------------------------------------------------------!
subroutine extractpart_grid(fullgrid, ipart, ncell, partition, partgrid)

use OUTPUT
use CONNECTIVITY
use MGRID
use USTMESH
use GRID_CONNECT
use MESHBASE
!use SUBGRID
!use METIS

implicit none

! -- INPUTS  -- 
type(st_grid)  :: fullgrid             ! full grid before split
integer        :: ipart                ! index of part to keep
integer(kip)   :: ncell                ! number of interior cells
integer(kip)   :: partition(1:ncell)   ! partition of "fullgrid" cells

! -- OUTPUTS  -- 
type(st_grid)  :: partgrid             ! ipart-th grid of fullgrid

! -- Internal variables --
type(st_ustboco), pointer &
                      :: boco(:)
integer(kip)          :: i, if, ic, ic1, ic2, iv, ivf, if2, nicl, nf, ib, new_ib, new_bcface
integer(kip)          :: nface_int, nface_lim, nface_cut
integer(kip)          :: ncell_int, ncell_tot, nvtex
integer(kip)          :: ncomm, maxcom
integer(kip), allocatable, dimension(:) &
                      :: new_icell, new_iface, new_ivtex, facepart, cutface, cutcell

! -- BODY --

! ---------------------------------------------
! initializing

!partgrid%id = ipart

! ---------------------------------------------
! compute size of internal/ghost faces & reindexation of faces and cells

ncell_int = count(partition(1:ncell) == ipart)

nface_int = 0
nface_lim = 0
nface_cut = 0

allocate(new_iface(fullgrid%umesh%nface))       ! reindex faces (contains new index)
new_iface(1:fullgrid%umesh%nface) = 0           ! cut faces tagged with -1 (first then re-indexed)

allocate(new_icell(fullgrid%umesh%ncell))       ! to reindex cells
new_icell(1:fullgrid%umesh%ncell) = 0

if2  = 0             ! incremental index of new face
nicl = ncell_int     ! incremental index of new ghost (boco or cut) cells

do if = 1, fullgrid%umesh%nface_int
  ic1 = fullgrid%umesh%facecell%fils(if,1)
  ic2 = fullgrid%umesh%facecell%fils(if,2)
  if (partition(ic1) == ipart) then
    if (partition(ic2) == ipart) then
      ! internal face
      if2       = if2 + 1
      nface_int = nface_int +1
      new_iface(if) = if2
    else
      ! cut face
      nface_cut = nface_cut +1
      new_iface(if)  = -1
      new_icell(ic2) = -1    ! ic2 is a ghost cell
    endif
  elseif (partition(ic2) == ipart) then 
    ! cut face
    nface_cut = nface_cut +1
    new_iface(if)  = -1
    new_icell(ic1) = -1     ! ic1 is a ghost cell
  endif
enddo

do if = fullgrid%umesh%nface_int+1, fullgrid%umesh%nface
  ic1 = fullgrid%umesh%facecell%fils(if,1)
  ic2 = fullgrid%umesh%facecell%fils(if,2)
  if (partition(ic1) == ipart) then
    ! limit face
    if2       = if2 + 1
    nface_lim = nface_lim +1
    new_iface(if) = if2
    nicl = nicl + 1
    new_icell(ic2) = nicl   ! ic2 is a ghost cell
  endif
enddo

!print*,"part ",ipart,":",nface_int," internal faces"
!print*,"part ",ipart,":",nface_lim," limit    faces"
!print*,"part ",ipart,":",nface_cut," cut      faces"

if (if2 /= nface_int+nface_lim) &
  call erreur("internal error", "numbers of faces do not match (int+lim)")
if (nicl-ncell_int /= nface_lim) &
  call erreur("internal error", "numbers of faces do not match (lim)")

! ---------------------------------------------
! compute size of the mesh & re-indexation of internal cells

ncell_tot = ncell_int + nface_lim + nface_cut

ic2 = 0
do ic1 = 1, ncell                        ! compute reindexation of cells
  if (partition(ic1) == ipart) then
    ic2 = ic2 + 1
    new_icell(ic1) = ic2
  endif
enddo

! ---------------------------------------------
! parse cut faces (reindex & save other partition connection)
!   these faces are put to the end of face array

allocate(facepart(1:nface_cut))   ! partition index of the face connection
allocate(cutface (1:nface_cut))   ! face index in the original mesh
allocate(cutcell (1:nface_cut))   ! index of original cell in fullgrid

if2 = nface_int + nface_lim
nf  = 0

do if = 1, fullgrid%umesh%nface_int
  if(new_iface(if) == -1) then
    ic1           = fullgrid%umesh%facecell%fils(if,1)
    ic2           = fullgrid%umesh%facecell%fils(if,2)
    nf            = nf  + 1
    if2           = if2 + 1
    new_iface(if) = if2
    cutface(nf)   = if
    facepart(nf)  = partition(ic1)+partition(ic2)-ipart
    if (partition(ic1) == ipart) then
      cutcell(nf) = ic2
    else
      cutcell(nf) = ic1
    endif
  endif
enddo

!print*,"info :", if2, " faces in the partition"

! ---------------------------------------------
! compute size of the mesh & re-indexation of vtex

allocate(new_ivtex(fullgrid%umesh%nvtex))
new_ivtex(1:fullgrid%umesh%nvtex) = 0

do if = 1, fullgrid%umesh%nface
  if(new_iface(if) /= 0) then        ! if the face is in the partition
    do ivf = 1, fullgrid%umesh%facevtex%nbfils
      iv = fullgrid%umesh%facevtex%fils(if,ivf)     ! ivf-th vertex of the face
      if (iv /= 0) new_ivtex(iv) = -1               ! marked
    enddo
  endif
enddo

nvtex = 0
do iv = 1, fullgrid%umesh%nvtex          ! renumber vertices
  if (new_ivtex(iv) == -1) then
    nvtex = nvtex + 1
    new_ivtex(iv) = nvtex
  endif
enddo

! ---------------------------------------------
! Initialize ustmesh

partgrid%umesh%ncell_int = ncell_int
partgrid%umesh%ncell_lim = nface_lim+nface_cut
partgrid%umesh%ncell     = ncell_tot
partgrid%umesh%nvtex     = nvtex

!! call new(partgrid%umesh, ncell_int+nface_cut, nface_int+nface_lim+nface_cut, 0)
!! the allocation of ustmesh is not available and should be detailed here

! ---------------------------------------------
! extract/copy ustmesh meshbase (geometrical properties)

call new(partgrid%umesh%mesh, ncell_int+nface_lim+nface_cut, nface_int+nface_lim+nface_cut, nvtex)

partgrid%umesh%mesh%info = fullgrid%umesh%mesh%info

! -- faces --

do if = 1, fullgrid%umesh%nface
  if (new_iface(if) /= 0) then        ! if the face is in the partition
    ! -- copy face (surface & normal vector) --
    partgrid%umesh%mesh%iface(new_iface(if), 1, 1) = fullgrid%umesh%mesh%iface(if, 1, 1) 
  endif
enddo

! -- vertices --

do iv = 1, fullgrid%umesh%nvtex
  if (new_ivtex(iv) > 0) then        ! if the vertex is in the partition
    ! -- copy vertex --
    partgrid%umesh%mesh%vertex(new_ivtex(iv), 1, 1) = fullgrid%umesh%mesh%vertex(iv, 1, 1) 
  endif
enddo

! -- centers & volumes (internal cells & classical boundary conditions) --

do ic = 1, fullgrid%umesh%ncell
  if(new_icell(ic) > 0) then        ! if the cell is in the partition (internal cell & boco cell)
    ! -- copy center & volume --
    partgrid%umesh%mesh%centre(new_icell(ic), 1, 1) = fullgrid%umesh%mesh%centre(ic, 1, 1) 
    partgrid%umesh%mesh%volume(new_icell(ic), 1, 1) = fullgrid%umesh%mesh%volume(ic, 1, 1) 
  endif
enddo

do ic = 1, nface_cut
  partgrid%umesh%mesh%centre(ncell_int+nface_lim+ic, 1, 1) = fullgrid%umesh%mesh%centre(cutcell(ic), 1, 1) 
  partgrid%umesh%mesh%volume(ncell_int+nface_lim+ic, 1, 1) = fullgrid%umesh%mesh%volume(cutcell(ic), 1, 1) 
enddo

! ---------------------------------------------
! extract/copy fields (only conservative values)

!!! DEV !!! not necessary here !!! 
!!! allocated & initialized later in init_champ

! ---------------------------------------------
! extract/copy ustmesh connectivity

! -- face->cell --

partgrid%umesh%nface_int = nface_int
partgrid%umesh%nface_lim =             nface_lim + nface_cut
partgrid%umesh%nface     = nface_int + nface_lim + nface_cut

call new(partgrid%umesh%facecell, partgrid%umesh%nface, 2)
partgrid%umesh%facecell%fils(1:partgrid%umesh%nface, 1:2) = 0

!print*,"check:",count(0)," errors"

do if = 1, fullgrid%umesh%nface
  if(new_iface(if) > 0) then        ! if the face is in the partition
    ! -- copy face/cell connectivity while renumbering cells --
    partgrid%umesh%facecell%fils(new_iface(if), 1) = new_icell(fullgrid%umesh%facecell%fils(if, 1))
    partgrid%umesh%facecell%fils(new_iface(if), 2) = new_icell(fullgrid%umesh%facecell%fils(if, 2))
  endif
enddo

do if = 1, nface_cut
  ic1 = new_icell(fullgrid%umesh%facecell%fils(cutface(if), 1))
  ic2 = new_icell(fullgrid%umesh%facecell%fils(cutface(if), 2))
  if (ic1 /= -1) then
    partgrid%umesh%facecell%fils(nface_int+nface_lim+if, 1) = ic1
  else
    partgrid%umesh%facecell%fils(nface_int+nface_lim+if, 1) = ncell_int + nface_lim + if
  endif
  if (ic2 /= -1) then
    partgrid%umesh%facecell%fils(nface_int+nface_lim+if, 2) = ic2
  else
    partgrid%umesh%facecell%fils(nface_int+nface_lim+if, 2) = ncell_int + nface_lim + if
  endif
enddo

!print*,"check new face/cell connectivity:",count(partgrid%umesh%facecell%fils(1:partgrid%umesh%nface, 1:2)==0)," errors"

! -- face->vtex --

nvtex = fullgrid%umesh%facevtex%nbfils
call new(partgrid%umesh%facevtex, partgrid%umesh%nface, nvtex)
partgrid%umesh%facevtex%fils(1:partgrid%umesh%nface, 1:nvtex) = 0

do if = 1, fullgrid%umesh%nface
  if(new_iface(if) /= 0) then        ! if the face is in the partition
    ! -- copy face/vtex connectivity while renumbering vtex --
    partgrid%umesh%facevtex%fils(new_iface(if), 1:nvtex) = new_ivtex(fullgrid%umesh%facevtex%fils(if, 1:nvtex))
  endif
enddo

! -- cell->vtex --

partgrid%umesh%cellvtex%nbar   = count(partition(fullgrid%umesh%cellvtex%ibar  (1:fullgrid%umesh%cellvtex%nbar))  ==ipart)
partgrid%umesh%cellvtex%ntri   = count(partition(fullgrid%umesh%cellvtex%itri  (1:fullgrid%umesh%cellvtex%ntri))  ==ipart)
partgrid%umesh%cellvtex%nquad  = count(partition(fullgrid%umesh%cellvtex%iquad (1:fullgrid%umesh%cellvtex%nquad)) ==ipart)
partgrid%umesh%cellvtex%ntetra = count(partition(fullgrid%umesh%cellvtex%itetra(1:fullgrid%umesh%cellvtex%ntetra))==ipart)
partgrid%umesh%cellvtex%npyra  = count(partition(fullgrid%umesh%cellvtex%ipyra (1:fullgrid%umesh%cellvtex%npyra)) ==ipart)
partgrid%umesh%cellvtex%npenta = count(partition(fullgrid%umesh%cellvtex%ipenta(1:fullgrid%umesh%cellvtex%npenta))==ipart)
partgrid%umesh%cellvtex%nhexa  = count(partition(fullgrid%umesh%cellvtex%ihexa (1:fullgrid%umesh%cellvtex%nhexa)) ==ipart)

call new(partgrid%umesh%cellvtex)

ic2 = 0
do i = 1, fullgrid%umesh%cellvtex%nbar
  ic = fullgrid%umesh%cellvtex%ibar(i)
  if (partition(ic) == ipart) then
    ic2 = ic2 +1
    partgrid%umesh%cellvtex%ibar(ic2)       = new_icell(ic)
    partgrid%umesh%cellvtex%bar%fils(ic2,:) = new_ivtex(fullgrid%umesh%cellvtex%bar%fils(i,:))
  endif
enddo
ic2 = 0
do i = 1, fullgrid%umesh%cellvtex%ntri
  ic = fullgrid%umesh%cellvtex%itri(i)
  if (partition(ic) == ipart) then
    ic2 = ic2 +1
    partgrid%umesh%cellvtex%itri(ic2)       = new_icell(ic)
    partgrid%umesh%cellvtex%tri%fils(ic2,:) = new_ivtex(fullgrid%umesh%cellvtex%tri%fils(i,:))
  endif
enddo
ic2 = 0
do i = 1, fullgrid%umesh%cellvtex%nquad
  ic = fullgrid%umesh%cellvtex%iquad(i)
  if (partition(ic) == ipart) then
    ic2 = ic2 +1
    partgrid%umesh%cellvtex%iquad(ic2)       = new_icell(ic)
    partgrid%umesh%cellvtex%quad%fils(ic2,:) = new_ivtex(fullgrid%umesh%cellvtex%quad%fils(i,:))
  endif
enddo
ic2 = 0
do i = 1, fullgrid%umesh%cellvtex%ntetra
  ic = fullgrid%umesh%cellvtex%itetra(i)
  if (partition(ic) == ipart) then
    ic2 = ic2 +1
    partgrid%umesh%cellvtex%itetra(ic2)       = new_icell(ic)
    partgrid%umesh%cellvtex%tetra%fils(ic2,:) = new_ivtex(fullgrid%umesh%cellvtex%tetra%fils(i,:))
  endif
enddo
ic2 = 0
do i = 1, fullgrid%umesh%cellvtex%npenta
  ic = fullgrid%umesh%cellvtex%ipenta(i)
  if (partition(ic) == ipart) then
    ic2 = ic2 +1
    partgrid%umesh%cellvtex%ipenta(ic2)       = new_icell(ic)
    partgrid%umesh%cellvtex%penta%fils(ic2,:) = new_ivtex(fullgrid%umesh%cellvtex%penta%fils(i,:))
  endif
enddo
ic2 = 0
do i = 1, fullgrid%umesh%cellvtex%npyra
  ic = fullgrid%umesh%cellvtex%ipyra(i)
  if (partition(ic) == ipart) then
    ic2 = ic2 +1
    partgrid%umesh%cellvtex%ipyra(ic2)       = new_icell(ic)
    partgrid%umesh%cellvtex%pyra%fils(ic2,:) = new_ivtex(fullgrid%umesh%cellvtex%pyra%fils(i,:))
  endif
enddo
ic2 = 0
do i = 1, fullgrid%umesh%cellvtex%nhexa
  ic = fullgrid%umesh%cellvtex%ihexa(i)
  if (partition(ic) == ipart) then
    ic2 = ic2 +1
    partgrid%umesh%cellvtex%ihexa(ic2)       = new_icell(ic)
    partgrid%umesh%cellvtex%hexa%fils(ic2,:) = new_ivtex(fullgrid%umesh%cellvtex%hexa%fils(i,:))
  endif
enddo

! ---------------------------------------------
! check cut faces orientation (ghost cell is facecell(if,2) index)

do if = 1, nface_cut
  if2 = new_iface(cutface(if))
  if (partgrid%umesh%facecell%fils(if2,1) > partgrid%umesh%facecell%fils(if2,2)) then
    new_ib = partgrid%umesh%facecell%fils(if2,2)
    partgrid%umesh%facecell%fils(if2,2) = partgrid%umesh%facecell%fils(if2,1)
    partgrid%umesh%facecell%fils(if2,1) = new_ib
    partgrid%umesh%mesh%iface(if2,1,1)%normale = -partgrid%umesh%mesh%iface(if2,1,1)%normale
  endif
enddo

!do if = 1, nface_int
!  print*,"facecell int",if,partgrid%umesh%facecell%fils(if,:)
!enddo
!do if = nface_int+1, nface_int+nface_lim
!  print*,"facecell lim",if,partgrid%umesh%facecell%fils(if,:)
!enddo
!do if = nface_int+nface_lim+1, nface_int+nface_lim+nface_cut
!  print*,"facecell cut",if,partgrid%umesh%facecell%fils(if,:)
!enddo

! ---------------------------------------------
! extract/copy ustmesh boco connectivity

allocate(boco(fullgrid%umesh%nboco+maxval(facepart(1:nface_cut))))   ! allocate maximum nb of boco
new_ib = 0

do ib = 1, fullgrid%umesh%nboco   ! loop on original bocos
  nf = count(new_iface(fullgrid%umesh%boco(ib)%iface(1:fullgrid%umesh%boco(ib)%nface))/=0)
  write(str_w,*) "extract boco",ib," - "//fullgrid%umesh%boco(ib)%family(1:20)//":",nf, " faces"
  call print_info(10, trim(str_w))
  if (nf /= 0) then               ! new boco (at least one face) in extracted part
    new_ib = new_ib + 1
    call new(boco(new_ib), fullgrid%umesh%boco(ib)%family, nf)
    boco(new_ib)%idefboco = fullgrid%umesh%boco(ib)%idefboco

    ! translate face index
    if2 = 0
    do if = 1, fullgrid%umesh%boco(ib)%nface
      new_bcface = new_iface(fullgrid%umesh%boco(ib)%iface(if))
      if (new_bcface /= 0) then
        if2 = if2 + 1
        boco(new_ib)%iface(if2) = new_bcface
      endif
    enddo
    !print*,"check",if2,nf
  endif
enddo

! ---------------------------------------------
! define new boco with grid connectivity

maxcom = maxval(facepart(1:nface_cut))

do ib = 1, maxcom   ! loop on all needed parts

  nf = count(facepart(1:nface_cut) == ib)

  if (nf /= 0) then  
    ! if it exists faces then
    ! define a new boco
    new_ib = new_ib + 1
    call new(boco(new_ib), "", nf)
    boco(new_ib)%idefboco = defboco_connect  ! not a reference to defsolver boco but internal connection

    !print*,"create connectivity boco",new_ib," :",nf, " faces"
    if2 = 0
    do if = 1, nface_cut
      if (facepart(if) == ib) then
        if2 = if2 + 1
        boco(new_ib)%iface(if2) = new_iface(cutface(if))
      endif
    enddo

    ! -- define associated grid connection --
    
    boco(new_ib)%gridcon%zone_id = 0               ! same zone
    boco(new_ib)%gridcon%grid_id = ib              ! grid id is assumed to be partition id
    boco(new_ib)%gridcon%contype = gdcon_match     ! matching connection

  endif
enddo

! ---------------------------------------------
! pack boco array

!print*,"pack", new_ib, "boco conditions"
partgrid%umesh%nboco = new_ib
allocate(partgrid%umesh%boco(new_ib))
partgrid%umesh%boco(1:new_ib) = boco(1:new_ib)

deallocate(boco)

! ---------------------------------------------
! extract/copy boco fields

!!! DEV !!! not necessary here !!! 
!!! allocated & initialized later in ???

! ---------------------------------------------
deallocate(new_icell)
deallocate(new_iface)
deallocate(new_ivtex)
deallocate(facepart, cutface, cutcell)


endsubroutine extractpart_grid

!------------------------------------------------------------------------------!
! Change history
!
! Oct  2005 : created
!------------------------------------------------------------------------------!
