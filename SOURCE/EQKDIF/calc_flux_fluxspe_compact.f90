!------------------------------------------------------------------------------!
! Procedure :calc_flux_fluxspe_compact    Auteur : E. Radenac
!                                         Date   : Avril 2003
! Fonction                                Modif  : Juin 2003
!   Calcul du flux à l'interface par la méthode du flux spécifique, par le 
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

! -- Declaration des entrées --
real(krp)             :: temp1, temp2   ! températures échangées
real(krp)	      :: conduct1, conduct2  ! conductivités échangées
real(krp)	      :: d1, d2  ! distance entre les centres des cellules et l'interface
type(v3d)             :: vecinter                ! vecteur unitaire "intercellules"		      		   
		      		   
! -- Declaration des entrées/sorties --
type(v3d) :: flux_inter

! -- Declaration des variables internes --
real(krp) :: a, b
real(krp) :: conduct


! -- Debut de la procedure --


!Pondération de la conductivité : paramètres a et b

a = conduct1/d1
b = conduct2/d2

conduct = (a*conduct1 + b*conduct2)/(a+b)

flux_inter = -conduct*(temp2 - temp1)&
		/(d1 + d2) * vecinter

endsubroutine calc_flux_fluxspe_compact
