!------------------------------------------------------------------------------!
! Procedure : calc_ns_timestep            Auteur : J. Gressier
!                                         Date   : July 2004
! Fonction                                Modif  : (cf historique)
!   Calcul du pas de temps local et global par zone selon solveur
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

subroutine calc_ns_timestep(deftime, fluid, umesh, field, dtloc, ncell)

use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_NUM
use DEFFIELD
use USTMESH
use EQNS

implicit none

! -- Declaration des entrées --
type(mnu_time)    :: deftime       ! paramètres pour le calcul du pas de temps
type(st_espece)   :: fluid         ! données du fluide
type(st_ustmesh)  :: umesh         ! données géométriques
type(st_field)    :: field         ! données champs
integer           :: ncell         ! nombre de cellules internes (taille de dtloc)

! -- Declaration des sorties --
real(krp), dimension(1:ncell) :: dtloc    ! tableau de pas de temps local

! -- Declaration des variables internes --
integer   :: if, ic
real(krp) :: gg1, a2, rv2, irho

! -- Debut de la procedure --

! -- Calcul de somme S_i --
! pour faire la somme des surfaces des faces, on boucle d'abord sur les faces
! internes pour les contributions aux deux cellules voisines, puis on boucle
! sur les faces limites pour uniquement ajouter la contribution aux cellules 
! internes

! initialisation avant somme des faces

dtloc(1:ncell) = 0._krp

! somme des surfaces de faces internes sur chaque cellule (boucle sur faces)

do if = 1, umesh%nface_int
  ic  = umesh%facecell%fils(if,1)
  dtloc(ic) = dtloc(ic) + umesh%mesh%iface(if,1,1)%surface 
  ic  = umesh%facecell%fils(if,2)
  dtloc(ic) = dtloc(ic) + umesh%mesh%iface(if,1,1)%surface
enddo

! somme des surfaces de faces limites sur chaque cellule (boucle sur faces)

do if = umesh%nface_int+1, umesh%nface
  ic  = umesh%facecell%fils(if,1)
  dtloc(ic) = dtloc(ic) + umesh%mesh%iface(if,1,1)%surface
enddo

! -- Calcul de V / somme_i S_i et prise en compte du nombre de CFL --

do ic = 1, ncell
  dtloc(ic) =  deftime%stabnb * 2._krp * umesh%mesh%volume(ic,1,1) / dtloc(ic)
enddo

gg1 = fluid%gamma*(fluid%gamma -1._krp)
do ic = 1, ncell
  rv2  = sqrabs(field%etatcons%tabvect(1)%vect(ic))
  irho = 1._krp/field%etatcons%tabscal(1)%scal(ic)
  a2   = (field%etatcons%tabscal(2)%scal(ic)-.5_krp*rv2*irho)*gg1*irho
  dtloc(ic) = dtloc(ic) / (sqrt(rv2)*irho+sqrt(a2))
enddo

! dans le cas de pas de temps global, le pas de temps minimum est calculé et imposé
! dans la routine appelante

endsubroutine calc_ns_timestep

!------------------------------------------------------------------------------!
! Historique des modifications

! July 2004 : création, calcul par CFL
!------------------------------------------------------------------------------!

