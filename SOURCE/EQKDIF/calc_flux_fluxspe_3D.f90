!------------------------------------------------------------------------------!
! Procedure :calc_flux_fluxspe_3D         Auteur : E. Radenac
!                                         Date   : Avril 2003
! Fonction                                Modif  : Juin 2003
!   Calcul du flux à l'interface par la méthode du flux spécifique, par le 
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

! -- Declaration des entrées --
real(krp)             :: temp1, temp2   ! températures échangées
real(krp)	      :: conduct1, conduct2  ! conductivités échangées
real(krp)	      :: d1, d2  ! distance entre les centres des cellules et l'interface
type(v3d)             :: vecinter                ! vecteur unitaire "intercellules"	
type(v3d)             :: flux1, flux2 ! densités de flux échangées

! -- Declaration des entrées/sorties --
type(v3d) :: flux_inter

! -- Declaration des variables internes --
real(krp) :: theta = 1                         ! aller chercher la valeur ailleurs
type(v3d) :: flux_compact, flux_consistant     ! densités de flux calulées par les méthodes
                                               ! compacte et consistante resp.
                         
! -- Debut de la procedure --
call calc_flux_fluxspe_compact(temp1, temp2, conduct1, conduct2, d1, d2, &
					vecinter, flux_compact)
call calc_flux_fluxspe_consistant(d1, d2, flux1, flux2, flux_consistant)			       

flux_inter = flux_consistant + theta*(flux_compact - (flux_consistant.scal.vecinter) * vecinter)

! mieux avec la définition vectorielle

endsubroutine calc_flux_fluxspe_3D
