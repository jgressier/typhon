!------------------------------------------------------------------------------!
! Procedure :calc_flux_fluxface_compact   Auteur : E. Radenac
!                                         Date   : Mai 2003
! Fonction                                Modif  : Juin 2003
!   Calcul du flux a l'interface par la methode du flux de face, par le 
!   biais d'une interpolation compacte
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

subroutine calc_flux_fluxface_compact(temp1, temp2, conduct1, conduct2, d1, d2, &
					vecinter, flux_inter)

use TYPHMAKE
use OUTPUT
use GEO3D

implicit none

! -- Declaration des entrees --
real(krp)             :: temp1, temp2   ! temperatures a gauche et a droite de l'interface
real(krp)	      :: conduct1, conduct2  ! conductivites a gauche et a droite
real(krp)	      :: d1, d2  ! distance entre les centres des cellules gauche, droite et
		      		 !  l'interface
type(v3d)             :: vecinter           ! vecteur unitaire "intercellules"		      		   
		      		   
! -- Declaration des entrees/sorties --
type(v3d) :: flux_inter

! -- Declaration des variables internes --
real(krp) :: conduct 
type(v3d) :: gradtemp 


! -- Debut de la procedure --

! "conductivite" a l'interface :

conduct = (d1+d2) / (d1/conduct1 + d2/conduct2)

! "gradient de temperature" a l'interface :

gradtemp = (temp2-temp1) / (d1+d2) * vecinter

flux_inter = -conduct * gradtemp

endsubroutine calc_flux_fluxface_compact
