!------------------------------------------------------------------------------!
! Procedure :calc_flux_fluxspe_compact    Auteur : E. Radenac
!                                         Date   : Avril 2003
! Fonction                                Modif  : Juin 2003
!   Calcul du flux a l'interface par la methode du flux specifique, par le 
!   biais d'une interpolation compacte
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

subroutine calc_flux_fluxspe_compact(temp1, temp2, conduct1, conduct2, d1, d2,&
					vecinter, flux_inter)

use TYPHMAKE
use OUTPUT
use GEO3D

implicit none

! -- Declaration des entrees --
real(krp)             :: temp1, temp2   ! temperatures echangees
real(krp)	      :: conduct1, conduct2  ! conductivites echangees
real(krp)	      :: d1, d2  ! distance entre les centres des cellules et l'interface
type(v3d)             :: vecinter                ! vecteur unitaire "intercellules"		      		   
		      		   
! -- Declaration des entrees/sorties --
type(v3d) :: flux_inter

! -- Declaration des variables internes --
real(krp) :: a, b
real(krp) :: conduct


! -- Debut de la procedure --


!Ponderation de la conductivite : parametres a et b

a = conduct1/d1
b = conduct2/d2

conduct = (a*conduct1 + b*conduct2)/(a+b)

flux_inter = -conduct*(temp2 - temp1)&
		/(d1 + d2) * vecinter

endsubroutine calc_flux_fluxspe_compact
