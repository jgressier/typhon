!------------------------------------------------------------------------------!
! Procedure : cgns2typhon_ustmesh         Auteur : J. Gressier
!                                         Date   : Novembre 2002
! Fonction                                Modif  : (cf historique)
!   Conversion d'une zone CGNS en structure Maillage NON structure pour Typhon
!   Creation des connectivites FACE->SOMMETS et FACES->CELLULES
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

subroutine cgns2typhon_ustmesh(defmesh, cgnszone, mesh) 

use CGNS_STRUCT   ! Definition des structures CGNS
use DEFZONE       ! Definition des structures ZONE
use USTMESH       ! Definition des structures maillages non structures
use OUTPUT        ! Sorties standard TYPHON
use MENU_MESH

implicit none 

! -- INPUTS --
type(mnu_mesh)     :: defmesh           ! mesh parameters
type(st_cgns_zone) :: cgnszone          ! structure ZONE des donnees CGNS

! -- OUTPUTS --
type(st_ustmesh)   :: mesh              ! structure USTMESH des donnees TYPHON

! -- Internal variables --
integer, parameter  :: nmax_cell = 20        ! max nb of cells in vtex->cell connectivity
type(st_connect)    :: face_vtex, &          ! temporary face->vtex connectivity
                       face_cell, &          ! temporary face->cell connectivity
                       face_Ltag, face_Rtag  ! left & right tag for face
integer             :: ntotcell              ! calcul du nombre total de cellules
integer             :: maxvtex, maxface      ! nombre de sommets/face, face/cellule
integer             :: nface                 ! estimation du nombre de faces
integer             :: iconn, icell, ivtex   ! indices courants

! -- BODY --

call init_ustmesh(mesh)   ! default values initialization

call print_info(8,". converting mesh and computing connectivity")

if (cgnszone%type /= Unstructured) then
  call erreur("Missing feature","only converting unstructured grids")
endif

select case(cgnszone%imesh)    ! transfer mesh dimension 
case(2)
  mesh%mesh%info%geom = msh_2dplan
case(3)
  mesh%mesh%info%geom = msh_3d
endselect

!--------------------------------------------------------------------------
! reading vertices cloud 
!--------------------------------------------------------------------------

call print_info(8,"  mesh points coordinates")

mesh%mesh%nvtex  = cgnszone%mesh%ni                  ! nb of vertices

mesh%nvtex       = mesh%mesh%nvtex                   ! nb of vertices (redundant)
allocate(mesh%mesh%vertex(mesh%mesh%nvtex, 1, 1))
mesh%mesh%vertex = cgnszone%mesh%vertex              ! copy vertex cloud

!--------------------------------------------------------------------------
! compute arrays approximate dimensions
!--------------------------------------------------------------------------

ntotcell = 0    ! total number of cells
maxvtex  = 0    ! max. number of vtex  by cell
maxface  = 0    ! max. number of faces by cell
nface    = 0    ! estimate of face number

call init(mesh%cellvtex)

do iconn = 1, cgnszone%ncellfam          ! boucle sur les sections de cellules
 
  ! cumul du nombre de cellules
  ntotcell = ntotcell + cgnszone%cellfam(iconn)%nbnodes  
  !print*,iconn," ",cgnszone%cellfam(iconn)%type !! DEBUG
  select case(cgnszone%cellfam(iconn)%type)
  case(NODE)
    call erreur("Developement", "unexpected type of cell (NODE)")
  case(BAR_2,BAR_3)
    call erreur("Developement", "unexpected type of cell (BAR)")
  case(TRI_3,TRI_6)
    maxvtex = max(maxvtex, 2)
    maxface = max(maxface, 3)
    nface   = nface + 3*cgnszone%cellfam(iconn)%nbnodes
    mesh%cellvtex%ntri = mesh%cellvtex%ntri + cgnszone%cellfam(iconn)%nbnodes
  case(QUAD_4,QUAD_8,QUAD_9)
    maxvtex = max(maxvtex, 2)
    maxface = max(maxface, 4)
    nface   = nface + 4*cgnszone%cellfam(iconn)%nbnodes
    mesh%cellvtex%nquad = mesh%cellvtex%nquad + cgnszone%cellfam(iconn)%nbnodes
  case(TETRA_4,TETRA_10)
    maxvtex = max(maxvtex, 3)
    maxface = max(maxface, 4)
    nface   = nface + 4*cgnszone%cellfam(iconn)%nbnodes
    mesh%cellvtex%ntetra = mesh%cellvtex%ntetra + cgnszone%cellfam(iconn)%nbnodes
  case(PYRA_5,PYRA_14)
    maxvtex = max(maxvtex, 4)
    maxface = max(maxface, 5)
    nface   = nface + 5*cgnszone%cellfam(iconn)%nbnodes
    mesh%cellvtex%npyra = mesh%cellvtex%npyra + cgnszone%cellfam(iconn)%nbnodes
  case(PENTA_6,PENTA_15,PENTA_18)
    maxvtex = max(maxvtex, 4)
    maxface = max(maxface, 5)
    nface   = nface + 5*cgnszone%cellfam(iconn)%nbnodes
    mesh%cellvtex%npenta = mesh%cellvtex%npenta + cgnszone%cellfam(iconn)%nbnodes
  case(HEXA_8,HEXA_20,HEXA_27)
    maxvtex = max(maxvtex, 4)
    maxface = max(maxface, 6)
    nface   = nface + 6*cgnszone%cellfam(iconn)%nbnodes
    mesh%cellvtex%nhexa = mesh%cellvtex%nhexa + cgnszone%cellfam(iconn)%nbnodes
  case(MIXED, NGON_n)
    call erreur("Development", "MIXED & NGON_n elements not yet handled")
  case default
    call erreur("CGNS conversion", "unknown type of element section")
  endselect
enddo


!--------------------------------------------------------------------------
! copy of CELL->VTEX connectivity
!--------------------------------------------------------------------------

call new(mesh%cellvtex)

do iconn = 1, cgnszone%ncellfam          ! boucle sur les sections de cellules
 
  select case(cgnszone%cellfam(iconn)%type)

  case(NODE)
    call erreur("Development", "Unexpected type of cell (NODE)")

  case(BAR_2,BAR_3)
    call erreur("Development", "Unexpected type of cell (BAR)")

  case(TRI_3)
    mesh%cellvtex%tri%fils(:,1:3) = cgnszone%cellfam(iconn)%fils(:,1:3)
    mesh%cellvtex%itri(:) = (/ (icell, icell = cgnszone%cellfam(iconn)%ideb, &
                                               cgnszone%cellfam(iconn)%ifin  ) /)
  case(TRI_6)
    call erreur("Development", "Unexpected type of cell (TRI_6)")
    !mesh%cellvtex%tri%fils(:,1:3) = cgnszone%cellfam(iconn)%fils(:,1:3)

  case(QUAD_4)
    mesh%cellvtex%quad%fils(:,1:4) = cgnszone%cellfam(iconn)%fils(:,1:4)
    mesh%cellvtex%iquad(:) = (/ (icell, icell = cgnszone%cellfam(iconn)%ideb, &
                                                cgnszone%cellfam(iconn)%ifin  ) /)

  case(QUAD_8,QUAD_9)
    call erreur("Development", "Unexpected type of cell (QUAD_8/9)")

  case(TETRA_4)
    mesh%cellvtex%tetra%fils(:,1:4) = cgnszone%cellfam(iconn)%fils(:,1:4)
    mesh%cellvtex%itetra(:) = (/ (icell, icell = cgnszone%cellfam(iconn)%ideb, &
                                                 cgnszone%cellfam(iconn)%ifin  ) /)

  case(TETRA_10)
    call erreur("Development", "Unexpected type of cell (TETRA_10)")

  case(PYRA_5)
    mesh%cellvtex%pyra%fils(:,1:5) = cgnszone%cellfam(iconn)%fils(:,1:5)
    mesh%cellvtex%ipyra(:) = (/ (icell, icell = cgnszone%cellfam(iconn)%ideb, &
                                                cgnszone%cellfam(iconn)%ifin  ) /)
  case(PYRA_14)
    call erreur("Development", "Unexpected type of cell (PYRA_14)")

  case(PENTA_6)
    mesh%cellvtex%penta%fils(:,1:6) = cgnszone%cellfam(iconn)%fils(:,1:6)
    mesh%cellvtex%ipenta(:) = (/ (icell, icell = cgnszone%cellfam(iconn)%ideb, &
                                                 cgnszone%cellfam(iconn)%ifin  ) /)

  case(PENTA_15,PENTA_18)
    call erreur("Development", "Unexpected type of cell (PENTA_15/18)")

  case(HEXA_8)
    mesh%cellvtex%hexa%fils(:,1:8) = cgnszone%cellfam(iconn)%fils(:,1:8)
    mesh%cellvtex%ihexa(:) = (/ (icell, icell = cgnszone%cellfam(iconn)%ideb, &
                                                cgnszone%cellfam(iconn)%ifin  ) /)
  case(HEXA_20,HEXA_27)
    call erreur("Development", "Unexpected type of cell (HEXA_20/27)")

  case(MIXED, NGON_n)
    call erreur("Gestion CGNS", "Elements MIXED et NFON_n non traites")

  case default
    call erreur("Gestion CGNS", "Type d'element non reconnu dans CGNSLIB")
  endselect

enddo

!--------------------------------------------------------------------------
! creating FACES (face->vtex) and cell connectivity (face->cell)
!--------------------------------------------------------------------------

! initialize face array

nullify(mesh%mesh%iface)
mesh%mesh%nface = 0

call print_info(8,"  creating faces and associated connectivity")

! -- connectivite intermediaire face->sommets --
call new(face_vtex, nface, maxvtex)         ! nface is only is estimated size
face_vtex%nbnodes   = 0                     ! set face counter to zero (not yet created)
face_vtex%fils(:,:) = 0                     ! initialization

! -- connectivite intermediaire face->cellules --
call new(face_cell, nface, 2)               ! nface is only is estimated size
face_cell%nbnodes   = 0                     ! reinitialisation : nombre de faces crees
face_cell%fils(:,:) = 0                     ! initialisation de la connectivite

if (defmesh%splitmesh /= split_none) then
  call new_connect(face_Ltag, nface, 1)
  call new_connect(face_Rtag, nface, 1)
  face_Ltag%fils(:,:) = 0
  face_Ltag%fils(:,:) = 0
endif  

!-------------------------------------------------------------------
! creation of faces (face->vtex) and connectivities (face->cell)
!-------------------------------------------------------------------

call createface_fromcgns(defmesh%splitmesh, mesh%nvtex, cgnszone, &
                         face_cell, face_vtex, face_Ltag, face_Rtag)

! Recopie des connectivites dans la structure TYPHON
! avec le nombre exact de faces reconstruites

nface          = face_vtex%nbnodes     ! meme valeur que face_cell%nbnodes aussi
mesh%nface     = nface
mesh%ncell_int = ntotcell
write(str_w,'(i10,a)') nface,"created faces"
call print_info(8,str_w)

call new(mesh%facevtex, nface, maxvtex)
mesh%facevtex%fils(1:nface,1:maxvtex) = face_vtex%fils(1:nface,1:maxvtex)

call new(mesh%facecell, nface, 2)
mesh%facecell%fils(1:nface,1:2)       = face_cell%fils(1:nface,1:2)

if (defmesh%splitmesh /= split_none) then
  call new_connect(mesh%face_Ltag, nface, 1)
  mesh%face_Ltag%fils(1:nface,1)       = face_Ltag%fils(1:nface,1)
  call new_connect(mesh%face_Rtag, nface, 1)
  mesh%face_Rtag%fils(1:nface,1)       = face_Rtag%fils(1:nface,1)
endif

! -- desallocation --

call delete(face_cell)
call delete(face_vtex)
call delete(face_Ltag)
call delete(face_Rtag)

! -- Renumerotation des faces --

call reorder_ustconnect(0, mesh)    ! action sur les connectivites uniquement

! -- Converting boundary conditions --

call cgns2typhon_ustboco(cgnszone, mesh)

mesh%ncell_lim = mesh%nface_lim
mesh%ncell     = mesh%ncell_int + mesh%ncell_lim

!-------------------------
endsubroutine cgns2typhon_ustmesh

!------------------------------------------------------------------------------!
! Changes history
!
! nov  2002 : creation de la procedure
! fev  2004 : renseignements dans structure INFO_MESH
! juin 2004 : memorisation de la connectivite cell->vtex
!             construction de faces avec toutes les familles
! oct  2007 : SVM oriented mesh creation (subcells)
!------------------------------------------------------------------------------!



