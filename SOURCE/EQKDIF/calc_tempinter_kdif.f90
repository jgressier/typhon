
!------------------------------------------------------------------------------!
! Procedure :  calc_tempinter_kdif        Auteur : E. Radenac
!                                         Date   : Avril 2003
! Fonction                                Modif  : Juin 2003
!   Calcul de la temperature de l'interface entre deux cellules appartenant a 
!   deux zones differentes
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

subroutine calc_tempinter_kdif(temp1, temp2, conduct1, conduct2, d1, d2, &
					temp_inter)

use TYPHMAKE
use OUTPUT

implicit none

! -- Declaration des entrees --
integer               :: if
real(krp)             :: temp1, temp2   ! temperatures echangees
real(krp)             :: conduct1, conduct2  ! conductivites echangees
real(krp)             :: d1, d2  ! distance entre les centres des cellules et l'interface      		   
		      		 
! -- Declaration des entrees/sorties --
real(krp) :: temp_inter

! -- Declaration des variables internes --
real(krp) :: a, b


! -- Debut de la procedure --

a = conduct1/d1
b = conduct2/d2

temp_inter = (a*temp1+b*temp2)/(a+b)
!print*, "temperatures CALC_TEMPINTER_KDIF :", temp1, " et ", temp2
!print*, "temperature a l'interface :", temp_inter

endsubroutine calc_tempinter_kdif
