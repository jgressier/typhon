!------------------------------------------------------------------------------!
! Procedure : calc_ust_cell               Auteur : J. Gressier
!                                         Date   : Janvier 2003
! Fonction                                Modif  :
!   Calcul des cellules (centres et volumes) a partir des volumes
!   elementaires
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine calc_ust_cell(ncell, nface, facecell, cg_elem, vol_elem, mesh)

use TYPHMAKE
use GEO3D
use OUTPUT
use USTMESH

implicit none

! -- Declaration des entrees --
integer                   :: ncell      ! nombre de cellules
integer                   :: nface      ! nombre de faces
type(st_connect)       :: facecell   ! connectivite face->cellules
type(v3d), dimension(nface,2) :: cg_elem    ! centres de volume elementaire
real(krp), dimension(nface,2) :: vol_elem   ! volumes de volume elementaire

! -- Declaration des entrees/sorties --
!type(st_ustmesh) :: ust_mesh

! -- Declaration des sorties --
type(st_mesh)    :: mesh                

! -- Declaration des variables internes --
integer                 :: if           ! indice  de face
integer                 :: ic, ic1, ic2 ! indices de cellules

! -- Debut de la procedure --

! les centres de gravite et les volumes des cellules elementaires sont
! indexes selon l'index de face. On doit utiliser la connectivite
! face->cellules pour faire la somme des volumes elementaires de chaque
! cellule.

! Initialisation

!print*,"!! DEBUG : initialisation"
!mesh%centre(1:ncell,1,1) = v3d(0.,0.,0.)
!mesh%volume(1:ncell,1,1) = 0._krp

! boucle sur la liste des faces et sommation pour les cellules

!print*,"!! DEBUG : sommation"
do if = 1, nface

  ic1 = facecell%fils(if,1)
  ic2 = facecell%fils(if,2)

  ! premiere cellule connectee
  mesh%centre(ic1,1,1) = mesh%centre(ic1,1,1) + vol_elem(if,1)*cg_elem(if,1)
  mesh%volume(ic1,1,1) = mesh%volume(ic1,1,1) + vol_elem(if,1)

  ! seconde cellule connectee (si existante)
  if (ic2 /= 0) then
    mesh%centre(ic2,1,1) = mesh%centre(ic2,1,1) + vol_elem(if,2)*cg_elem(if,2)
    mesh%volume(ic2,1,1) = mesh%volume(ic2,1,1) + vol_elem(if,2)
  endif

enddo

! calcul de la moyenne sur les cellules
do ic = 1, ncell
  mesh%centre(ic,1,1) = mesh%centre(ic,1,1) /  mesh%volume(ic,1,1)
enddo


endsubroutine calc_ust_cell
