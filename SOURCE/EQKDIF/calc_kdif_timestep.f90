!------------------------------------------------------------------------------!
! Procedure : calc_kdif_timestep          Auteur : J. Gressier
!                                         Date   : Septembre 2003
! Fonction                                Modif  : (cf historique)
!   Calcul du pas de temps local et global par zone selon solveur
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

subroutine calc_kdif_timestep(deftime, mat, umesh, field, dtloc, ncell)

use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_NUM
use DEFFIELD
use USTMESH
use MATERIAU
use MATER_LOI

implicit none

! -- Declaration des entrees --
type(mnu_time)    :: deftime       ! parametres pour le calcul du pas de temps
type(st_materiau) :: mat           ! donnees du materiau
type(st_ustmesh)  :: umesh         ! donnees geometriques
type(st_field)    :: field         ! donnees champs
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

! -- Calcul des coefficients specifiques du materiau --

select case(mat%type)
case(mat_LIN)
  dtloc(1:ncell) = dtloc(1:ncell) * mat%Cp / mat%Kd%valeur
case(mat_KNL)
  do ic = 1, ncell
    tc        = field%etatprim%tabscal(1)%scal(ic)
    dtloc(ic) = dtloc(ic) * mat%Cp / valeur_loi(mat%Kd, tc)
  enddo
case(mat_XMAT)
  call erreur("Calcul de materiau","Materiau non lineaire interdit")
endselect

! dans le cas de pas de temps global, le pas de temps minimum est calcule et impose
! dans la routine appelante

endsubroutine calc_kdif_timestep

!------------------------------------------------------------------------------!
! Historique des modifications
!   sept 2003 : creation, calcul par nombre de Fourier, formule generale 3D
!------------------------------------------------------------------------------!

