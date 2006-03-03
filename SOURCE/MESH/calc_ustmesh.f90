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
subroutine calc_ustmesh(umesh, defmesh)

use TYPHMAKE
use OUTPUT
use USTMESH
use MENU_MESH
use MESHBASE

implicit none

! -- Inputs --
type(mnu_mesh) :: defmesh

! -- Inputs/Outputs --
type(st_ustmesh) :: umesh

! -- Ouputs --

! -- Internal variables --
integer                                :: i
type(v3d), dimension(:),   allocatable :: cgface
type(v3d), dimension(:),   allocatable :: midcell   ! centres approches de cellule
type(v3d), dimension(:,:), allocatable :: cg_elem   ! centres de volume elementaire
real(krp), dimension(:,:), allocatable :: vol_elem  ! volumes elementaires

! -- BODY --

call scale_mesh(umesh%mesh, defmesh%scale)

call test_ustmesh(umesh)

! -- allocation --
! les allocations de faces et cellules geometriques sont de taille (nface) et (ncell)
! (nombre d'elements limites ou fictifs inclus)
! le calcul se fait sur toutes les faces mais uniquement sur les cellules internes.

! -- allocate cells & face (vertices are already allocated) --

call new(umesh%mesh, umesh%ncell, umesh%nface, 0)

!! allocation des faces geometriques si necessaire
!if (umesh%mesh%nface == 0) then
!  umesh%mesh%nface = umesh%nface                  ! copie du nombre de faces
!  allocate(umesh%mesh%iface(umesh%nface,1,1))     ! allocation des faces
!endif

allocate(cgface(umesh%nface))                      ! tab. interm. centre G des faces
  ! les centres G des faces sont maintenant memorisees dans la liste de faces
  ! il n'est pas utile d'allouer un tableau separement

!-------------------------------------------------------------------
! Calcul des faces (centres, normales et surfaces)

call print_info(10, "  . computing face geometry...")
call calc_ust_face(umesh%facevtex, umesh%mesh, cgface(1:umesh%nface))


select case(typgeo(umesh))

!-------------------------------------------------------------------
! maillage de cellules + faces

case(msh_2dplan, msh_3d)

  call print_info(10, "  . computing cell geometry...")

  ! -- Calcul de centres de cellules (centres approximatifs)

  allocate(midcell(umesh%ncell))
  call calc_ust_midcell(umesh%ncell_int, umesh%facecell, cgface, midcell)

  ! -- Calcul des volumes elementaires (volume et centre de gravite)

  allocate(cg_elem (umesh%nface,2))
  allocate(vol_elem(umesh%nface,2))
  call calc_ust_elemvol(typgeo(umesh), umesh%ncell_int, umesh%nface, &
                        midcell, umesh%facecell,                     &
                        cgface, umesh%mesh%iface, cg_elem, vol_elem)

  ! -- Calcul des cellules (volumes et centre de gravite)

  ! attention : les allocations se font sur (ncell) et les calculs sur (ncell_int)
  ! on choisit d'allouer par defaut toutes les cellules y compris les cellules fictives,
  ! meme si elles ne sont pas utilisees par le code (economie en memoire a rechercher)

  !allocate(umesh%mesh%centre(umesh%ncell,1,1))
  !allocate(umesh%mesh%volume(umesh%ncell,1,1))

  umesh%mesh%centre(1:umesh%ncell,1,1) = v3d_zero
  umesh%mesh%volume(1:umesh%ncell,1,1) = 0._krp

  call calc_ust_cell(umesh%ncell_int, umesh%nface, &
                     umesh%facecell, cg_elem, vol_elem, umesh%mesh)

  ! -- Verification de l'orientation des normales et connectivites face->cellules

  call calc_ust_checkface(umesh%facecell, umesh%mesh)

  ! desallocation tableaux intermediaires

  deallocate(cgface, midcell, cg_elem, vol_elem)

  ! -- compute info --

  call print_info(10, "  mesh information:")

  call calc_mesh_info(umesh%mesh)

  write(str_w, '(a,e12.4,a,e10.4)') "    min & max volume:",umesh%mesh%info%minvol, &
                                                                    " to ",umesh%mesh%info%maxvol
  call print_info(10, str_w)
  write(str_w, '(a,e12.4)')         "        total volume:",umesh%mesh%info%totvol
  call print_info(10, str_w)
  write(str_w, '(a,3e12.4)')        "      gravity center:",umesh%mesh%info%center
  call print_info(10, str_w)


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
