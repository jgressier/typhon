!------------------------------------------------------------------------------!
! Procedure : calc_ustmesh                Auteur : J. Gressier
!                                         Date   : Novembre 2002
! Fonction                                Modif  : Mars     2003 (cf historique)
!   Calcul et initialisation d'un maillage non structuré, nécessite
!     en entrée : liste des sommets et coordonnées
!                 connectivités faces->sommets et faces->cellules
!
! Defauts/Limitations/Divers :
!   Après avoir calculé les faces (centres Hi, normales, surfaces), on découpe
!   les cellules en (n) volumes élémentaires chacun défini par un centre (midcell)
!   de cellule et une face (pour chacune des (n) faces).
!   On calcule le volume et le centre de gravité de chacun des volumes élémentaires
!   pour ensuite calculer le volume et le centre de gravité de la cellule complète.
!   (la méthode est indifférente en 2D ou 3D, seules les formules de calcul
!   du volume (surface en 2D) et des centres de gravité des volumes élémentaires
!   changent).
!
!------------------------------------------------------------------------------!
subroutine calc_ustmesh(ust_mesh)

use TYPHMAKE
use OUTPUT
use USTMESH

implicit none

! -- Declaration des entrées --

! -- Declaration des entrées/sorties --
type(st_ustmesh) :: ust_mesh

! -- Declaration des sorties --

! -- Declaration des variables internes --
integer                                :: i
type(v3d), dimension(:),   allocatable :: cgface
type(v3d), dimension(:),   allocatable :: midcell   ! centres approchés de cellule
type(v3d), dimension(:,:), allocatable :: cg_elem   ! centres de volume élémentaire
real(krp), dimension(:,:), allocatable :: vol_elem  ! volumes élémentaires

! -- Debut de la procedure --

call test_ustmesh(ust_mesh)

! -- allocation --
! les allocations de faces et cellules géométriques sont de taille (nface) et (ncell)
! (nombre d'éléments limites ou fictifs inclus)
! le calcul se fait sur toutes les faces mais uniquement sur les cellules internes.

ust_mesh%mesh%nface = ust_mesh%nface                  ! copie du nombre de faces
allocate(ust_mesh%mesh%iface(ust_mesh%nface,1,1))     ! allocation des faces
allocate(cgface(ust_mesh%nface))                      ! tab. interm. centre G des faces
  ! les centres G des faces sont maintenant mémorisées dans la liste de faces
  ! il n'est pas utile d'allouer un tableau séparément

! -- Calcul des faces (centres, normales et surfaces)

!print*,"!! DEBUG : calcul des faces : ",ust_mesh%nface," faces" !! DEBUG
call calc_ust_face(ust_mesh%facevtex, ust_mesh%mesh, cgface)

! -- Calcul de centres de cellules (centres approximatifs)

!print*,"!! DEBUG : calcul des centres de cellules" !! DEBUG
allocate(midcell(ust_mesh%ncell))
call calc_ust_midcell(ust_mesh%ncell_int, ust_mesh%facecell, cgface, midcell)

! -- Calcul des volumes élémentaires (volume et centre de gravité)

!print*,"!! DEBUG : calcul des volumes élémentaires" !! DEBUG
allocate(cg_elem (ust_mesh%nface,2))
allocate(vol_elem(ust_mesh%nface,2))
call calc_ust_elemvol(ust_mesh%nbdim, ust_mesh%ncell_int, ust_mesh%nface, &
                      midcell, ust_mesh%facecell,                     &
                      cgface, ust_mesh%mesh%iface, cg_elem, vol_elem)

! -- Calcul des cellules (volumes et centre de gravité)

!print*,"!! DEBUG : calcul des cellules" !! DEBUG
! attention : les allocations se font sur (ncell) et les calculs sur (ncell_int)
! on choisit d'allouer par défaut toutes les cellules y compris les cellules fictives,
! même si elles ne sont pas utilisées par le code (économie en mémoire à rechercher)

allocate(ust_mesh%mesh%centre(ust_mesh%ncell,1,1))
allocate(ust_mesh%mesh%volume(ust_mesh%ncell,1,1))

print*,"!! DEBUG : initialisation"
ust_mesh%mesh%centre(1:ust_mesh%ncell,1,1) = v3d(0.,0.,0.)
ust_mesh%mesh%volume(1:ust_mesh%ncell,1,1) = 0._krp

call calc_ust_cell(ust_mesh%ncell_int, ust_mesh%nface, &
                   ust_mesh%facecell, cg_elem, vol_elem, ust_mesh%mesh)

! -- Vérification de l'orientation des normales et connectivités face->cellules

!print*,"!! DEBUG : vérification" !! DEBUG
call calc_ust_checkface(ust_mesh%facecell, ust_mesh%mesh)

! désallocation tableaux intermédiaires

deallocate(cgface, midcell, cg_elem, vol_elem)

!!do i = 1, ust_mesh%nface
!!  write(*,"(a,i3,a,4i3,7f7.2)") &
!!    "face",i,":",ust_mesh%facevtex%fils(i,:),ust_mesh%facecell%fils(i,:), &
!!    ust_mesh%mesh%iface(i,1,1)%normale, &
!!    ust_mesh%mesh%iface(i,1,1)%surface,ust_mesh%mesh%iface(i,1,1)%centre !! DEBUG
!!enddo

!!do i = 1, ust_mesh%ncell
!!  write(*,"(a,i3,a,3f10.2)") "cell",i,":",ust_mesh%mesh%centre(i,1,1) !! DEBUG
!!enddo

endsubroutine calc_ustmesh

!------------------------------------------------------------------------------!
! Historique des modifications
!
! nov  2002 (v0.0.1b): création de la procédure
! fév  2003          : intégration du calcul des métriques
!------------------------------------------------------------------------------!
