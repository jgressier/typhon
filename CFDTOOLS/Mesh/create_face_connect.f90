!------------------------------------------------------------------------------!
! Procedure : create_face_connect
!
! Fonction
!   creation of TYPHON USTMESH connectivities
!   . face->cells
!   . face->vtex
!   . boundary condition links (tags->faces)
!------------------------------------------------------------------------------!
subroutine create_face_connect(facetag, umesh) 

use IOCFD   
use USTMESH  

implicit none 

! -- INPUTS --
logical            :: facetag

! -- OUTPUTS --
type(st_ustmesh)   :: umesh             ! unstructured mesh

! -- Internal variables --
integer, parameter  :: nmax_cell = 20        ! max nb of cells in vtex->cell connectivity
type(st_connect)    :: face_vtex, &          ! temporary face->vtex connectivity
                       face_cell, &          ! temporary face->cell connectivity
                       face_Ltag, face_Rtag  ! left & right tag for face
integer             :: ntotcell              ! calcul du nombre total de cellules
integer             :: maxvtex, maxface      ! nombre de sommets/face, face/cellule
integer             :: nface, nfacesec       ! estimated face number
integer             :: ielem, nelem , nvtex    
integer             :: ista, iend, itype, geodim
integer             :: iv, if     

! -- BODY --

call cfd_print("* compute FACE connectivity")

!--------------------------------------------------------------------------
! initialization
!--------------------------------------------------------------------------

! initialize face array

nullify(umesh%mesh%iface)
umesh%mesh%nface = 0

call cfd_print("  creating faces and associated connectivity")

! -- estimate of number of faces and number of vtex per face--

nface   = 0
maxvtex = 0
geodim  = 0
do ielem = 1, umesh%cellvtex%nsection
  nfacesec = umesh%cellvtex%elem(ielem)%nelem * ( nface_element(umesh%cellvtex%elem(ielem)%elemtype) -1)
  if (nfacesec < 0) then
    call cfd_error("inconsistent value, corrupted data in estimated face number")
  endif
  nface   = nface + nfacesec
  maxvtex = max(maxvtex, nvtexperface_element(umesh%cellvtex%elem(ielem)%elemtype))
  geodim  = max(geodim, dim_element(umesh%cellvtex%elem(ielem)))
enddo
umesh%geodim = geodim

! -- connectivite intermediaire face->sommets --

call new(face_vtex, nface, maxvtex)         ! nface is only is estimated size
face_vtex%nbnodes   = 0                     ! set face counter to zero (not yet created)
face_vtex%fils(:,:) = 0                     ! initialization

! -- connectivite intermediaire face->cellules --
call new(face_cell, nface, 2)               ! nface is only is estimated size
face_cell%nbnodes   = 0                     ! reinitialisation : nombre de faces crees
face_cell%fils(:,:) = 0                     ! initialisation de la connectivite

if (facetag) then
  call new_connect(face_Ltag, nface, 1)
  call new_connect(face_Rtag, nface, 1)
  face_Ltag%fils(:,:) = 0
  face_Rtag%fils(:,:) = 0
endif  

!-------------------------------------------------------------------
! creation of faces (face->vtex) and connectivities (face->cell)
!-------------------------------------------------------------------

call create_facevtex(facetag, umesh%nvtex, umesh%cellvtex, &
                         face_cell, face_vtex, face_Ltag, face_Rtag)

! Recopie des connectivites dans la structure TYPHON
! avec le nombre exact de faces reconstruites

nface           = face_vtex%nbnodes     ! meme valeur que face_cell%nbnodes aussi
umesh%nface     = nface
umesh%ncell_int = number_element(umesh%cellvtex, dim=geodim)

call cfd_print("  >"//strof(nface,9)//" created faces")

call new(umesh%facevtex, nface, maxvtex)
do if = 1, nface   ! loop because of stack size problem
  umesh%facevtex%fils(if,1:maxvtex) = face_vtex%fils(if,1:maxvtex)
enddo

call new(umesh%facecell, nface, 2)
do if = 1, nface   ! loop because of stack size problem
  umesh%facecell%fils(if,1:2)       = face_cell%fils(if,1:2)
enddo

call cfd_print("  >"//strof(count(umesh%facecell%fils(1:nface,2)==0),9)//" external faces")

if (facetag) then
  call new_connect(umesh%face_Ltag, nface, 1)
  umesh%face_Ltag%fils(1:nface,1)       = face_Ltag%fils(1:nface,1)
  call new_connect(umesh%face_Rtag, nface, 1)
  umesh%face_Rtag%fils(1:nface,1)       = face_Rtag%fils(1:nface,1)
endif

! -- desallocation --

call delete(face_cell)
call delete(face_vtex)
if (facetag) then
  call delete(face_Ltag)
  call delete(face_Rtag)
endif

! -- Renumerotation des faces --

call reorder_ustconnect(0, umesh)    ! action sur les connectivites uniquement

! -- Converting boundary conditions --

call create_ustboco(umesh)

umesh%ncell_lim = umesh%nface_lim
umesh%ncell     = umesh%ncell_int + umesh%ncell_lim

! -- Deleting unused element/vtex connectivities --

do ielem = 1, umesh%cellvtex%nsection
  if (dim_element(umesh%cellvtex%elem(ielem)) < umesh%geodim) &
    call delete_elemvtex(umesh%cellvtex%elem(ielem))
enddo
call pack_genelemvtex(umesh%cellvtex)

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



