!------------------------------------------------------------------------------!
! Procedure : calc_ustmesh                Auteur : J. Gressier
!                                         Date   : Novembre 2002
! Fonction                                Modif  : (cf historique)
!   Calcul et initialisation d'un maillage non structure, necessite
!     en entree : liste des sommets et coordonnees
!                 connectivites faces->sommets et faces->cellules
!
! Defauts/Limitations/Divers :
!   Apres avoir calcule les faces (centres Hi, normales, surfaces), on decoupe
!   les cellules en (n) volumes elementaires chacun defini par un centre (midcell)
!   de cellule et une face (pour chacune des (n) faces).
!   On calcule le volume et le centre de gravite de chacun des volumes elementaires
!   pour ensuite calculer le volume et le centre de gravite de la cellule complete.
!   (la methode est indifferente en 2D ou 3D, seules les formules de calcul
!   du volume (surface en 2D) et des centres de gravite des volumes elementaires
!   changent).
!
!------------------------------------------------------------------------------!
subroutine calc_ustmesh(ust_mesh, defmesh)

use TYPHMAKE
use OUTPUT
use USTMESH
use MENU_MESH

implicit none

! -- Inputs --
type(mnu_mesh) :: defmesh

! -- Inputs/Outputs --
type(st_ustmesh) :: ust_mesh

! -- Ouputs --

! -- Internal variables --
integer                                :: i
type(v3d), dimension(:),   allocatable :: cgface
type(v3d), dimension(:),   allocatable :: midcell   ! centres approches de cellule
type(v3d), dimension(:,:), allocatable :: cg_elem   ! centres de volume elementaire
real(krp), dimension(:,:), allocatable :: vol_elem  ! volumes elementaires

! -- BODY --

call scale_mesh(ust_mesh%mesh, defmesh%scale)

call test_ustmesh(ust_mesh)

! -- allocation --
! les allocations de faces et cellules geometriques sont de taille (nface) et (ncell)
! (nombre d'elements limites ou fictifs inclus)
! le calcul se fait sur toutes les faces mais uniquement sur les cellules internes.

! allocation des faces geometriques si necessaire
if (ust_mesh%mesh%nface == 0) then
  ust_mesh%mesh%nface = ust_mesh%nface                  ! copie du nombre de faces
  allocate(ust_mesh%mesh%iface(ust_mesh%nface,1,1))     ! allocation des faces
endif

allocate(cgface(ust_mesh%nface))                      ! tab. interm. centre G des faces
  ! les centres G des faces sont maintenant memorisees dans la liste de faces
  ! il n'est pas utile d'allouer un tableau separement

!-------------------------------------------------------------------
! Calcul des faces (centres, normales et surfaces)

call calc_ust_face(ust_mesh%facevtex, ust_mesh%mesh, cgface)


select case(typgeo(ust_mesh))

!-------------------------------------------------------------------
! maillage de cellules + faces

case(msh_2dplan, msh_3d)

  ! -- Calcul de centres de cellules (centres approximatifs)

  allocate(midcell(ust_mesh%ncell))
  call calc_ust_midcell(ust_mesh%ncell_int, ust_mesh%facecell, cgface, midcell)

  ! -- Calcul des volumes elementaires (volume et centre de gravite)

  allocate(cg_elem (ust_mesh%nface,2))
  allocate(vol_elem(ust_mesh%nface,2))
  call calc_ust_elemvol(typgeo(ust_mesh), ust_mesh%ncell_int, ust_mesh%nface, &
                        midcell, ust_mesh%facecell,                     &
                        cgface, ust_mesh%mesh%iface, cg_elem, vol_elem)

  ! -- Calcul des cellules (volumes et centre de gravite)

  ! attention : les allocations se font sur (ncell) et les calculs sur (ncell_int)
  ! on choisit d'allouer par defaut toutes les cellules y compris les cellules fictives,
  ! meme si elles ne sont pas utilisees par le code (economie en memoire a rechercher)

  allocate(ust_mesh%mesh%centre(ust_mesh%ncell,1,1))
  allocate(ust_mesh%mesh%volume(ust_mesh%ncell,1,1))

  ust_mesh%mesh%centre(1:ust_mesh%ncell,1,1) = v3d(0.,0.,0.)
  ust_mesh%mesh%volume(1:ust_mesh%ncell,1,1) = 0._krp

  call calc_ust_cell(ust_mesh%ncell_int, ust_mesh%nface, &
                     ust_mesh%facecell, cg_elem, vol_elem, ust_mesh%mesh)

  ! -- Verification de l'orientation des normales et connectivites face->cellules

  call calc_ust_checkface(ust_mesh%facecell, ust_mesh%mesh)

  ! desallocation tableaux intermediaires

  deallocate(cgface, midcell, cg_elem, vol_elem)

!-------------------------------------------------------------------
! maillage de facettes uniquement (solveur VORTEX)

case(msh_1dcurv, msh_2dcurv)

! DEV ! VERIFIER L'ORIENTATION DES NORMALES A L'EXTERIEUR DES CORPS (INTERIEUR FLUIDE)

case default

  call erreur("Developpement","cas inattendu (calc_ustmesh)")

endselect


endsubroutine calc_ustmesh

!------------------------------------------------------------------------------!
! Historique des modifications
!
! nov  2002 : creation de la procedure
! fev  2003 : integration du calcul des metriques
!------------------------------------------------------------------------------!
