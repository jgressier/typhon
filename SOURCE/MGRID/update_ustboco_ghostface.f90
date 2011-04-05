!------------------------------------------------------------------------------!
! Procedure : update_ustboco_ghostface               Authors : JG + A. Gardi
!                                                    Created : Mar 2011
! Fonction                                           Modif  :
!   Update the ghostcell parameters after mesh movement due to ALE
!
!------------------------------------------------------------------------------!
subroutine update_ustboco_ghostface(ib, defboco, umesh)
!DEV: procedure vastly similar to init_ustboco_ghostface, to be rearranged in future!

use TYPHMAKE
use OUTPUT
use USTMESH
use MENU_BOCO

implicit none

! -- Declaration des entrees --
integer        :: ib                     ! numero de condition aux limites
type(mnu_boco) :: defboco                ! parametres du solveur

! -- Declaration des entrees/sorties --
type(st_ustmesh) :: umesh                ! unstructured mesh

! -- Declaration des sorties --

! -- Declaration des variables internes --
integer :: if                            ! indice boco et de face
integer :: icell, iface                  ! index de cellule et de face

! --- BODY ---

! -- boucle sur la liste des faces de la condition limite --

do if = 1, umesh%boco(ib)%nface    
  
  ! affectation de connectivite face limites -> cellules fictives
  iface = umesh%boco(ib)%iface(if)          ! index de face
  icell = umesh%facecell%fils(iface,2)      ! index de cellule limite

  ! definition geometrique de la cellule fictive
  umesh%mesh%volume(icell,1,1) = 0._krp
  umesh%mesh%centre(icell,1,1) = umesh%mesh%iface(iface,1,1)%centre
enddo

endsubroutine update_ustboco_ghostface
!------------------------------------------------------------------------------!
! changes history
!
! mars 2011 : creation from init_ustboco_ghostface
!------------------------------------------------------------------------------!
