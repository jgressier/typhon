!------------------------------------------------------------------------------!
! Procedure : calc_ns_timestep          Auteur : J. Gressier
!                                         Date   : Octobre 2003
! Fonction                                Modif  : (cf historique)
!   Calcul du pas de temps local et global par zone selon solveur
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

subroutine calc_ns_timestep(deftime,umesh, field, dtloc, ncell)

use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_NUM
use DEFFIELD
use USTMESH

implicit none

! -- Declaration des entrées --
type(mnu_time)    :: deftime       ! paramètres pour le calcul du pas de temps
type(st_ustmesh)  :: umesh         ! données géométriques
type(st_field)    :: field         ! données champs
integer           :: ncell         ! nombre de cellules internes (taille de dtloc)

! -- Declaration des sorties --
real(krp), dimension(1:ncell) :: dtloc    ! tableau de pas de temps local

! -- Declaration des variables internes --
integer   :: if, ic
real(krp) :: tc

! -- Debut de la procedure --

! -- Calcul de somme S_i ** 2 --
! pour faire la somme des surfaces des faces, on boucle d'abord sur les faces
! internes pour les contributions aux deux cellules voisines, puis on boucle
! sur les faces limites pour uniquement ajouter la contribution aux cellules 
! internes

! initialisation avant somme des faces (**2)

dtloc(1:ncell) = 0._krp

! somme des surfaces de faces internes sur chaque cellule (boucle sur faces)

do if = 1, umesh%nface_int
  ic  = umesh%facecell%fils(if,1)
  dtloc(ic) = dtloc(ic) + umesh%mesh%iface(if,1,1)%surface **2
  ic  = umesh%facecell%fils(if,2)
  dtloc(ic) = dtloc(ic) + umesh%mesh%iface(if,1,1)%surface **2
enddo

! somme des surfaces de faces limites sur chaque cellule (boucle sur faces)

do if = umesh%nface_int+1, umesh%nface
  ic  = umesh%facecell%fils(if,1)
  dtloc(ic) = dtloc(ic) + umesh%mesh%iface(if,1,1)%surface **2
enddo

! -- Calcul de V**2 / somme_i S_i **2 et prise en compte du nombre de Fourier --

do ic = 1, ncell
  dtloc(ic) =  2._krp * deftime%stabnb * umesh%mesh%volume(ic,1,1)**2 / dtloc(ic)
enddo

! -- Calcul des coefficients spécifiques du matériau --



endsubroutine calc_ns_timestep

!------------------------------------------------------------------------------!
! Historique des modifications
!   oct 2003 : création
!------------------------------------------------------------------------------!

