!------------------------------------------------------------------------------!
! Procedure :calc_flux_fluxspe_3D         Auteur : E. Radenac
!                                         Date   : Avril 2003
! Fonction                                Modif  : Juin 2003
!   Calcul du flux a l'interface par la methode du flux specifique, par le 
!   biais d'une interpolation 3D
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

subroutine calc_flux_fluxspe_3D(d1, d2, flux1, flux2, temp1, temp2, &
				conduct1, conduct2, vecinter, flux_inter)

use TYPHMAKE
use OUTPUT
use GEO3D

implicit none

! -- Declaration des entrees --
real(krp)             :: temp1, temp2   ! temperatures echangees
real(krp)	      :: conduct1, conduct2  ! conductivites echangees
real(krp)	      :: d1, d2  ! distance entre les centres des cellules et l'interface
type(v3d)             :: vecinter                ! vecteur unitaire "intercellules"	
type(v3d)             :: flux1, flux2 ! densites de flux echangees

! -- Declaration des entrees/sorties --
type(v3d) :: flux_inter

! -- Declaration des variables internes --
real(krp) :: theta = 1                         ! aller chercher la valeur ailleurs
type(v3d) :: flux_compact, flux_consistant     ! densites de flux calulees par les methodes
                                               ! compacte et consistante resp.
                         
! -- Debut de la procedure --
call calc_flux_fluxspe_compact(temp1, temp2, conduct1, conduct2, d1, d2, &
					vecinter, flux_compact)
call calc_flux_fluxspe_consistant(d1, d2, flux1, flux2, flux_consistant)			       

flux_inter = flux_consistant + theta*(flux_compact - (flux_consistant.scal.vecinter) * vecinter)

! mieux avec la definition vectorielle

endsubroutine calc_flux_fluxspe_3D
