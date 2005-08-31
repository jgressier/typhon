
!------------------------------------------------------------------------------!
! Procedure :  calc_cpl_temp              Auteur : E. Radenac
!                                         Date   : April 2003
! Fonction                                Modif  : August 2005
!   Computation of interface temperature at the interface of two cells 
!   belonging to different zones
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

subroutine calc_cpl_temp(temp1, temp2, conduct1, conduct2, d1, d2, &
					temp_inter)

use TYPHMAKE
use OUTPUT

implicit none

! -- Declaration des entrees --
integer               :: if
real(krp)             :: temp1, temp2   ! exchanged temperatures
real(krp)             :: conduct1, conduct2  ! exchanged conductivities 
real(krp)             :: d1, d2  ! distance from cell center to interface
		      		 
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

endsubroutine calc_cpl_temp

!------------------------------------------------------------------------------!
! Changes history
!
! April 2003 : creation of routine, named calc_tempinter_kdif 
!              (directory EQKDIF)
! August 2005 : routine renamed calc_cpl_temp, moved to MAIN, used by both KDIF
!               and NS applications
!------------------------------------------------------------------------------!
