!------------------------------------------------------------------------------!
! Procedure : calc_ust_checkface          Auteur : J. Gressier
!                                         Date   : Janvier 2003
! Fonction                                Modif  : see history
!   1. Face->cell connectivity is changed so that cell 1 index is lower than cell 2
!   2. Check face normals direction accordingg to the connectivity face->cell
!     (normal vector is oriented from cell 1 to cell 2)
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine calc_ust_checkface(facecell, mesh)

use TYPHMAKE
use OUTPUT
use USTMESH
use MESHBASE

implicit none

! -- Declaration des entrées --
type(st_connect)  :: facecell   ! connectivité face->cellules

! -- Declaration des entrées/sorties --
type(st_mesh)        :: mesh       ! maillages

! -- Declaration des sorties --

! -- Declaration des variables internes --
integer   :: if       ! index de face
integer   :: ic1, ic2 ! indices de cellules
type(v3d) :: v12      ! vecteur du centre de cellule 1 à centre de cellule 2

! -- Debut de la procedure --

! -- change of connectivity indexes (except boundary faces where ic2 = 0) --

do if = 1, facecell%nbnodes
  if ((facecell%fils(if,2) /= 0).and.(facecell%fils(if,1) > facecell%fils(if,2))) then
    ic1                 = facecell%fils(if,1)
    facecell%fils(if,1) = facecell%fils(if,2)
    facecell%fils(if,2) = ic1
  endif
enddo

! -- check normal vector direction --

do if = 1, facecell%nbnodes     ! boucle sur les faces

  ic1 = facecell%fils(if,1)     ! index de cellules voisines à la face
  ic2 = facecell%fils(if,2)     ! par convention, la normale va de ic1 à ic2

  ! v12 est le vecteur du centre ic1 au centre ic2. 
  ! Si la face est limite, v12 est le centre ic1 vers le centre de la face

  if (ic2 /= 0) then
    v12 = mesh%centre(ic2,1,1) - mesh%centre(ic1,1,1)
  else
    v12 = mesh%iface(if,1,1)%centre - mesh%centre(ic1,1,1)
  endif

  ! si v12 et la normale sont inversées, on corrige la normale pour 
  ! être en accord avec la convention des connectivités

  if ((v12.scal.mesh%iface(if,1,1)%normale) < 0._krp) then
    mesh%iface(if,1,1)%normale = - mesh%iface(if,1,1)%normale
  endif

enddo

endsubroutine calc_ust_checkface

!------------------------------------------------------------------------------!
! Change history
!
! jan  2003 : création de la procédure
! mai  2003 : correction de l'orientation des normales de faces limites
! july 2004 : change of connectivity indexes (from lower to upper)
!------------------------------------------------------------------------------!
