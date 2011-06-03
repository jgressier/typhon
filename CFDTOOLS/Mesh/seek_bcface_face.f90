!------------------------------------------------------------------------------!
! Procedure : seek_bcface_face.f90
!          
! Fonction 
!   Looking for "typhon internal" boundary faces with element face tags
!
!------------------------------------------------------------------------------!
subroutine seek_bcface_face(ustboco, umesh) 

use USTMESH       ! Definition des structures maillage non structure
use IOCFD
use STRING

implicit none 

! -- INPUTS --

! -- INPUTS/OUTPUTS --
type(st_ustboco)  :: ustboco      ! original tags
type(st_ustmesh)  :: umesh        ! unstructured mesh

! -- OUTPUTS --

! -- Internal variables --
integer               :: it, ie, ielem, iface
integer               :: ielemface      ! iloc_elemface tag
integer, allocatable  :: face(:)        ! local face to search
integer               :: nvtex          ! number of vertices of local face (size(face))
integer               :: nf             ! number of tagged face
integer, allocatable  :: listface(:)    ! list of marked faces 
integer               :: notfound1, notfound2

! --- BODY ---

allocate(listface(ustboco%ntag))

! -- replace iloc_elemface tag by iloc_face tag --

notfound1 = 0
notfound2 = 0
nf        = 0

do it = 1, ustboco%ntag

  ielemface = ustboco%itag(it)

  ! -- seek tag in face elements --
  do ielem = 1, umesh%cellvtex%nsection
    if (dim_element(umesh%cellvtex%elem(ielem)) == geodim(umesh)-1 ) then ! only "FACE" elements
      do ie = 1, umesh%cellvtex%elem(ielem)%nelem
        if (umesh%cellvtex%elem(ielem)%ielem(ie) == ielemface) then
          nvtex = umesh%cellvtex%elem(ielem)%nvtex
          allocate(face(nvtex))
          face(1:nvtex) = umesh%cellvtex%elem(ielem)%elemvtex(ie,1:nvtex)
          exit ! exit loop
        endif
      enddo
    endif
    if (allocated(face)) exit ! if allocated (face found), then exit loop
  enddo ! loop on element section

  if (allocated(face)) then
    iface = scan_bocoface(face(1:nvtex), nvtex, umesh)
    if (iface > 0) then
      nf = nf + 1
      listface(nf) = iface
    else
      notfound1 = notfound1 + 1
    endif
    deallocate(face)
  else
    ! -- tag does not refer to (dim-1) element
    notfound2 = notfound2 + 1
    !call cfd_error("tag not found (inconsistent mesh definition)")
  endif

enddo ! loop on tags

if (notfound1 > 0) call cfd_print('      '//trim(strof(notfound1))//' tags not found as original face element !')
if (notfound2 > 0) call cfd_print('      '//trim(strof(notfound2))//' face element not found as boundary face !')

call deletetag_ustboco(ustboco)
call new_ustboco(ustboco, ustboco%family, nf)
ustboco%ilocation = iloc_face
if (nf > 0) ustboco%iface(1:nf) = listface(1:nf)
  
!-------------------------
endsubroutine seek_bcface_face
!------------------------------------------------------------------------------!
! Changes history
!
! Dec  2010: created (fully rewritten)
!------------------------------------------------------------------------------!
