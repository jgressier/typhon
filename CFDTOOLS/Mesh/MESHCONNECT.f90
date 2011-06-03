!------------------------------------------------------------------------------!
! MODULE : MESHCONNECT
! 
! Fonction
!   Computation of mesh connectivity
!------------------------------------------------------------------------------!
module MESHCONNECT

use IOCFD   
use USTMESH  

implicit none

! -- Variables globales du module -------------------------------------------

! -- DECLARATIONS -----------------------------------------------------------

! -- INTERFACES -------------------------------------------------------------

! -- Fonctions et Operateurs ------------------------------------------------

! -- IMPLEMENTATION ---------------------------------------------------------
contains

!------------------------------------------------------------------------------!
! Procedure : create_face_connect
!
! Fonction
!   creation of TYPHON USTMESH connectivities
!   . face->cells
!   . face->vtex
!   . boundary condition links (tags->faces)
!------------------------------------------------------------------------------!
subroutine create_face_connect(facetag, umesh, verbose) 

use IOCFD   
use USTMESH  

implicit none 

! -- INPUTS --
logical            :: facetag
integer, optional  :: verbose

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
integer             :: ista, iend, itype, igeodim
integer             :: iv, if, iverb     

! -- BODY --

if (present(verbose)) then
  iverb = verbose
else
  iverb = 0
endif

!--------------------------------------------------------------------------
! initialization
!--------------------------------------------------------------------------

! initialize face array

nullify(umesh%mesh%iface)
umesh%mesh%nface = 0

if (iverb > 0) call cfd_print("  creating faces and associated connectivity")

! -- estimate of number of faces and number of vtex per face--

nface   = 0
maxvtex = 0
do ielem = 1, umesh%cellvtex%nsection
  nfacesec = umesh%cellvtex%elem(ielem)%nelem * ( nface_element(umesh%cellvtex%elem(ielem)%elemtype) -1)
  if (nfacesec < 0) then
    call cfd_error("inconsistent value, corrupted data in estimated face number")
  endif
  nface   = nface + nfacesec
  maxvtex = max(maxvtex, nvtexperface_element(umesh%cellvtex%elem(ielem)%elemtype))
enddo

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

if (iverb > 0) call cfd_print("  >"//strof(nface,9)//" created faces")

call new(umesh%facevtex, nface, maxvtex)
do if = 1, nface   ! loop because of stack size problem
  umesh%facevtex%fils(if,1:maxvtex) = face_vtex%fils(if,1:maxvtex)
enddo

call new(umesh%facecell, nface, 2)
do if = 1, nface   ! loop because of stack size problem
  umesh%facecell%fils(if,1:2)       = face_cell%fils(if,1:2)
enddo

if (iverb > 0) call cfd_print("  >"//strof(count(umesh%facecell%fils(1:nface,2)==0),9)//" external faces")

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

call create_ustboco(umesh, verbose=iverb)

umesh%ncell_lim = umesh%nface_lim
umesh%ncell     = umesh%ncell_int + umesh%ncell_lim

! -- Deleting unused element/vtex connectivities --

call delete_ustmesh_subelements(umesh)

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

!------------------------------------------------------------------------------!
! Procedure : create_ustboco
!  
! Fonction
!   Creation des conditions aux limites par conversion CGNS->TYPHON
!   Test de coherence entre les faces CGNS et les faces creees dans TYPHON
!
! Defauts/Limitations/Divers :
!   ATTENTION : les faces sont supposees ordonnees (faces limites en fin de tab)
!
!------------------------------------------------------------------------------!
subroutine create_ustboco(umesh, verbose) 

use USTMESH       ! Definition des structures maillage non structure
use IOCFD  

implicit none 

! -- INPUTS --
integer, optional   :: verbose

! -- INPUTS/OUTPUTS --
type(st_ustmesh)    :: umesh           ! unstructured mesh

! -- Internal variables --
integer             :: nface_int       ! nombre de faces internes
integer             :: nface_lim       ! nombre de faces limites
integer             :: ib, if, nf, iib
integer             :: iverb

! -- BODY --

if (present(verbose)) then
  iverb = verbose
else
  iverb = 0
endif

! -- Creation des conditions aux limites --

do ib = 1, umesh%nboco

  umesh%boco(ib)%family = uppercase(umesh%boco(ib)%family)
  if (iverb > 0) &
    call cfd_print("  . linking boundary condition marks"//strof(ib,3)//"/"//trim(strof(umesh%nboco))// &
                    ": "//trim(umesh%boco(ib)%family))

  select case(umesh%boco(ib)%ilocation)
  case(iloc_vtex)
    if (iverb > 0) call cfd_print('      vertex tagging method')
    call seek_bcface_vtex(umesh%boco(ib), umesh)
  case(iloc_elemcell)
    if (iverb > 0) call cfd_print('      cell tagging method')
    call cfd_error('tagging method not yet implemented')
    !call seek_bcface_vtex(ib, umesh%boco(ib), umesh, listface)
  case(iloc_elemface)
    if (iverb > 0) call cfd_print('      face element tagging method')
    call seek_bcface_face(umesh%boco(ib), umesh)
  case(iloc_face)
    if (iverb > 0) call cfd_print('      genuine face tagging method (nothing to do)')
  case default
    call cfd_error("boundary condition links: unknown tagging method ("//trim(strof(umesh%boco(ib)%ilocation))//")")
  endselect

  if (iverb > 0) then
    if (umesh%boco(ib)%nface /= 0) then
      call cfd_print('      > '//trim(strof(umesh%boco(ib)%nface))//' mesh faces tagged')
    else
      call cfd_print("    > no marked face has been found, skipping boundary condition mark section...")
    endif
  endif

enddo

! --- Check empty boco marks ---

nf  = 0
iib = 0

do ib = 1, umesh%nboco
  if (umesh%boco(ib)%nface /= 0) then
    iib = iib + 1
    nf  = nf + umesh%boco(ib)%nface
    if (iib < ib) umesh%boco(iib) = umesh%boco(ib)   ! if staggered, then transfer
  else
    call delete_ustboco(umesh%boco(ib))
  endif
enddo

umesh%nboco = iib

nf = 0
do ib = 1, umesh%nboco
  nf = nf + umesh%boco(ib)%nface
enddo

! --- check bounding faces == tagged faces ---

if (nf /= umesh%nface_lim) &
  call cfd_error("number of tagged faces does not correspond to number of boundaring faces")

!-------------------------
endsubroutine create_ustboco
!------------------------------------------------------------------------------!
! Changes history
!
! fev  2003: creation
! june 2004: construction des connectivites BOCO-> faces, generalisation
!            procedure intrinseque transferee dans USTMESH
! Nov  2007: check empty boco marks
! Dec  2010: TYPHON(cgns2typhon_ustboco)->CFDTOOLS(create_ustboco)
!------------------------------------------------------------------------------!

endmodule MESHCONNECT
!------------------------------------------------------------------------------!
! Changes history
!
! June 2011: created from create_face_connect
!------------------------------------------------------------------------------!



