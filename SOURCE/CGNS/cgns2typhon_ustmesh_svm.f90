!------------------------------------------------------------------------------!
! Procedure : cgns2typhon_ustmesh_svm           Author : J. Gressier
!                           
! Function  
!   create SVM mesh from typhon zone
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

subroutine cgns2typhon_ustmeshsvm(defsolver, defmesh, cgnszone, mesh) 

use CGNS_STRUCT   ! Definition des structures CGNS
use DEFZONE       ! Definition des structures ZONE
use USTMESH       ! Definition des structures maillages non structures
use OUTPUT        ! Sorties standard TYPHON
use MENU_SOLVER
use MENU_MESH

implicit none 

! -- INPUTS --
type(mnu_solver)   :: defsolver         ! solver parameters
type(mnu_mesh)     :: defmesh           ! mesh parameters
type(st_cgns_zone) :: cgnszone          ! structure ZONE des donnees CGNS

! -- OUTPUTS --
type(st_ustmesh)   :: mesh           ! structure USTMESH des donnees TYPHON

! -- Internal variables --
integer, parameter  :: nmax_cell = 20   ! nb max de cellules dans la connectivite vtex->cell
type(st_connect)    :: face_vtex, &     ! connectivite intermediaire faces   -> sommets
                       face_cell        ! connectivite intermediaire faces   -> cellules
integer             :: ntotcell         ! calcul du nombre total de cellules
integer             :: maxvtex, maxface ! nombre de sommets/face, face/cellule
integer             :: nface            ! estimation du nombre de faces
integer             :: iconn, icell, ivtex   ! indices courants

! -- BODY --

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
! reading vertices cloud 
!--------------------------------------------------------------------------

call print_info(8,"  mesh points coordinates")

! -- estimation of the number of vertices --

mesh%mesh%nvtex  = cgnszone%mesh%ni + ntotcell*defsolver%defspat%svm%sub_node               ! nb of vertices
allocate(mesh%mesh%vertex(mesh%mesh%nvtex, 1, 1))
mesh%mesh%vertex(1:cgnszone%mesh%ni, 1, 1) = cgnszone%mesh%vertex(1:cgnszone%mesh%ni, 1, 1) ! copy vertex cloud
mesh%nvtex       = mesh%mesh%nvtex                                                          ! nb of vertices (redundant)


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

! allocation des tableaux de connectivites

! -- connectivite intermediaire face->sommets --
call new(face_vtex, nface, maxvtex)
face_vtex%nbnodes   = 0                     ! reinitialisation : nombre de faces crees
face_vtex%fils(:,:) = 0                     ! initialisation de la connectivite

! -- connectivite intermediaire face->cellules --
call new(face_cell, nface, 2)
face_cell%nbnodes   = 0                     ! reinitialisation : nombre de faces crees
face_cell%fils(:,:) = 0                     ! initialisation de la connectivite

! -- creation des faces et des connectivites --

call createface_fromcgns(mesh%nvtex, cgnszone, face_cell, face_vtex)

! Recopie des connectivites dans la structure TYPHON
! avec le nombre exact de faces reconstruites

nface          = face_vtex%nbnodes     ! meme valeur que face_cell%nbnodes aussi
mesh%nface     = nface
mesh%ncell_int = ntotcell
write(str_w,'(i10,a)') nface,"created faces"
call print_info(8,str_w)

call new(mesh%facevtex, nface, maxvtex)
call new(mesh%facecell, nface, 2)

mesh%facevtex%fils(1:nface,1:maxvtex) = face_vtex%fils(1:nface,1:maxvtex)
mesh%facecell%fils(1:nface,1:2)       = face_cell%fils(1:nface,1:2)

! desallocation

!deallocate(vtex_cell, ncell) !!! is it possible to deallocate ?

call delete(face_cell)
call delete(face_vtex)

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



