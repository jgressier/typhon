!------------------------------------------------------------------------------!
! Procedure : calc_kdif_fourier           Auteur : E. Radenac / J. Gressier
!                                         Date   : Janvier 2004
! Fonction                                Modif  : (cf historique)
!   Calcul du nombre de Fourier global
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

subroutine calc_kdif_fourier(dt, mat, umesh, field, fourierloc, ncell)

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
real(krp)         :: dt            ! temps pour le calcul du nombre de Fourier
type(st_materiau) :: mat           ! donnees du materiau
type(st_ustmesh)  :: umesh         ! donnees geometriques
type(st_field)    :: field         ! donnees champs
integer           :: ncell         ! nombre de cellules internes (taille de dtloc)

! -- Declaration des sorties --
real(krp), dimension(1:ncell) :: fourierloc    ! tableau de nb de Fourier local

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

fourierloc(1:ncell) = 0._krp

! somme des surfaces de faces internes sur chaque cellule (boucle sur faces)

do if = 1, umesh%nface_int
  ic  = umesh%facecell%fils(if,1)
  fourierloc(ic) = fourierloc(ic) + umesh%mesh%iface(if,1,1)%surface **2
  ic  = umesh%facecell%fils(if,2)
  fourierloc(ic) = fourierloc(ic) + umesh%mesh%iface(if,1,1)%surface **2
enddo

! somme des surfaces de faces limites sur chaque cellule (boucle sur faces)

do if = umesh%nface_int+1, umesh%nface
  ic  = umesh%facecell%fils(if,1)
  fourierloc(ic) = fourierloc(ic) + umesh%mesh%iface(if,1,1)%surface **2
enddo

! -- Calcul de somme_i S_i **2 dt /  V**2 

do ic = 1, ncell
  fourierloc(ic) =  fourierloc(ic)/ (2._krp*umesh%mesh%volume(ic,1,1)**2 )*dt 
enddo

! -- Calcul des coefficients specifiques du materiau --

select case(mat%type)
case(mat_LIN)
  fourierloc(1:ncell) = fourierloc(1:ncell) * mat%Kd%valeur / mat%Cp
case(mat_KNL)
  do ic = 1, ncell
    tc        = field%etatprim%tabscal(1)%scal(ic)
    fourierloc(ic) = fourierloc(ic) * valeur_loi(mat%Kd, tc) / mat%Cp
  enddo
case(mat_XMAT)
  call erreur("Calcul de materiau","Materiau non lineaire interdit")
endselect

! le nombre de Fourier global d'un domaine est calcule et impose
! dans la routine appelante

endsubroutine calc_kdif_fourier

!------------------------------------------------------------------------------!
! Historique des modifications
!   jan 2003 : creation, formule generale 3D
!------------------------------------------------------------------------------!
