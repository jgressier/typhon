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
subroutine calc_ustmesh(defmesh, fctenv, umesh)

use TYPHMAKE
use OUTPUT
use USTMESH
use MESHPARAMS
use MESHGEOM
use FCT_FUNC

implicit none

! -- Inputs --
type(mnu_mesh)      :: defmesh
type(st_fctfuncset) :: fctenv

! -- Inputs/Outputs --
type(st_ustmesh) :: umesh

! -- Ouputs --

! -- Internal variables --
integer     :: i

! -- BODY --

call scale_mesh(defmesh, fctenv, umesh%mesh)   ! scale or morph mesh NODES

call test_ustmesh(umesh)

! -- allocation --
! les allocations de faces et cellules geometriques sont de taille (nface) et (ncell)
! (nombre d'elements limites ou fictifs inclus)
! le calcul se fait sur toutes les faces mais uniquement sur les cellules internes.

! -- allocate cells & face (vertices are already allocated) --

call alloc_mesh(umesh%mesh, umesh%ncell, umesh%nface, 0)

!-------------------------------------------------------------------
! geometry computation

call print_info(10, "  . computing face and cell geometry...")

call calc_meshgeom(defmesh, umesh)

! -- display mesh info --

call print_info(10, "  mesh information:")

write(str_w, '(a,e12.4,a,e10.4)') "    min & max volume:",umesh%mesh%info%minvol, &
                                                   " to ",umesh%mesh%info%maxvol
call print_info(10, str_w)
write(str_w, '(a,e12.4)')         "        total volume:",umesh%mesh%info%totvol
call print_info(10, str_w)
write(str_w, '(a,3e12.4)')        "      gravity center:",umesh%mesh%info%center
call print_info(10, str_w)

endsubroutine calc_ustmesh
!------------------------------------------------------------------------------!
! Changes history
!
! nov  2002 : creation de la procedure
! fev  2003 : integration du calcul des metriques
!------------------------------------------------------------------------------!
