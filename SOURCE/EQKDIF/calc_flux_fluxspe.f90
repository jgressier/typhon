!------------------------------------------------------------------------------!
! Procedure :  calc_flux_fluxspe          Auteur : E. Radenac
!                                         Date   : Avril 2003
! Fonction                                Modif  : Juin 2003
!   Calcul du flux a l'interface par la methode du flux specifique
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

subroutine calc_flux_fluxspe(temp1, temp2, conduct1, conduct2, d1, d2, &
			     vecinter, flux1, flux2, normale, flux_inter, &
			     typecalc)
use TYPHMAKE
use OUTPUT
use GEO3D
use VARCOM

implicit none

! -- Declaration des entrees --
real(krp)             :: temp1, temp2   ! temperatures echangees
real(krp)	      :: conduct1, conduct2  ! conductivites echangees
real(krp)	      :: d1, d2  ! distance entre les centres des cellules et l'interface
type(v3d)             :: vecinter                ! vecteur unitaire "intercellules"	
type(v3d)             :: flux1, flux2 ! densites de flux echangees                        
type(v3d)             :: normale ! normales a l'interface
integer               :: typecalc

! -- Declaration des entrees/sorties --
real(krp) :: flux_inter  ! scalaire

! -- Declaration des variables internes --
type(v3d) :: flux_interf      ! vectoriel

! -- Debut de la procedure --
select case(typecalc)
case(consistant)
  call calc_flux_fluxspe_consistant(d1, d2, flux1, flux2, flux_interf)
case(compact)
  call calc_flux_fluxspe_compact(temp1, temp2, conduct1, conduct2, d1, d2, &
					vecinter, flux_interf)
case(threed)
  call calc_flux_fluxspe_3D(d1, d2, flux1, flux2, temp1, temp2, & 
				conduct1, conduct2, vecinter, flux_interf)
case default
  call erreur("Lecture de menu raccord","type de calcul de raccord non reconnu")
endselect

flux_inter = flux_interf.scal.normale

endsubroutine calc_flux_fluxspe
