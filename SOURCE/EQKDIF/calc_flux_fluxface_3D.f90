!------------------------------------------------------------------------------!
! Procedure :calc_flux_fluxface_3D        Auteur : E. Radenac
!                                         Date   : Mai 2003
! Fonction                                Modif  : Juin 2003
!   Calcul du flux a l'interface par la methode du flux de face, par le 
!   biais d'une interpolation 3D
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

subroutine calc_flux_fluxface_3D(temp1, temp2, gradtemp1, gradtemp2, conduct1, conduct2, d1, d2, &
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
type(v3d)	      :: gradtemp1, gradtemp2 ! gradients de temperature echanges

! -- Declaration des entrees/sorties --
type(v3d) :: flux_inter

! -- Declaration des variables internes --
real(krp) :: theta = 1                         ! aller chercher la valeur ailleurs
type(v3d) :: flux_consistant, flux_compact     ! flux calcules par les methodes
                                               ! consistante et compacte resp.
                         
! -- Debut de la procedure --

!calcul de la partie compacte

call calc_flux_fluxface_compact(temp1, temp2, conduct1, conduct2, d1, d2, &
					vecinter, flux_compact)

!calcul de la partie consistante

call calc_flux_fluxface_consistant(gradtemp1, gradtemp2, conduct1, conduct2, d1, d2, &
					flux_consistant)			       

flux_inter = flux_consistant + theta*(flux_compact - (flux_consistant.scal.vecinter) * vecinter)

! mieux avec la definition vectorielle

endsubroutine calc_flux_fluxface_3D

