!------------------------------------------------------------------------------!
! Procedure : seek_bcface_vtex.f90        Auteur : J. Gressier
!                                         Date   : Juin 2004
! Fonction                                Modif  : (cf historique)
!   Recherche des faces a partir des listes de VERTEX marques
!
!------------------------------------------------------------------------------!
subroutine seek_bcface_vtex(ustboco, umesh) 

use USTMESH       ! Definition des structures maillage non structure
use IOCFD

implicit none 

! -- INPUTS --

! -- INPUTS/OUTPUTS --
type(st_ustboco)  :: ustboco      ! original tags
type(st_ustmesh)  :: umesh        ! unstructured mesh

! -- OUTPUTS --

! -- Internal variables --
integer               :: if
integer               :: nf             ! number of tagged face
integer, allocatable  :: listface(:)    ! list of marked faces 

! --- BODY ---

allocate(listface(umesh%nface_lim))   ! temporary allocation

! -- Creation des conditions aux limites --

nf = 0  

! recherche des faces limites concernees

do if = umesh%nface_int+1, umesh%nface_int+umesh%nface_lim

  if (face_invtexlist(umesh%facevtex%nbfils, umesh%facevtex%fils(if,:), &
                      ustboco%ntag,          ustboco%itag(:)) ) then
    nf = nf + 1
    listface(nf) = if
  endif
enddo

call cfd_print('      '//trim(strof(nf))//' mesh faces tagged')

call deletetag_ustboco(ustboco)
call new_ustboco(ustboco, ustboco%family, nf)
ustboco%ilocation   = iloc_face
if (nf > 0) ustboco%iface(1:nf) = listface(1:nf)

deallocate(listface)
  
!-------------------------
endsubroutine seek_bcface_vtex
!------------------------------------------------------------------------------!
! Changes history
!
! june 2004: creation
! Dec  2010: transfered to CFDTOOLS (direct use of USTMESH structures)
!------------------------------------------------------------------------------!
