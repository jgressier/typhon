!------------------------------------------------------------------------------!
! Procedure : calc_fluxinter_kdif         Auteur : E. Radenac
!                                         Date   : Avril 2003
! Fonction                                Modif  : Juin 2003
!   Calcul du flux de conduction de la chaleur a l'interface entre deux cellules
!   appartenant a deux zones differentes
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

subroutine calc_fluxinter_kdif(temp1, temp2, gradtemp1, gradtemp2, conduct1, &
			       conduct2, d1, d2, vecinter, flux1, flux2, normale, &
			       flux_inter, typecalcul, typemethode)

use TYPHMAKE 
use OUTPUT
use GEO3D
use VARCOM

implicit none

! -- Declaration des entrees --
real(krp)             :: temp1, temp2   ! temperatures echangees
type(v3d)             :: gradtemp1, gradtemp2 ! gradients de temperature echanges
real(krp)	      :: conduct1, conduct2  ! conductivites echangees
real(krp)	      :: d1, d2  ! distance entre les centres des cellules et l'interface
type(v3d)             :: vecinter ! vecteur unitaire "intercellules"	
type(v3d)             :: flux1, flux2 ! densites de flux echangees                         
type(v3d)             :: normale ! normales a l'interface
integer               :: typecalcul
integer               :: typemethode

! -- Declaration des entrees/sorties --
real(krp) :: flux_inter

! -- Declaration des variables internes --

! -- Debut de la procedure --

select case(typemethode)
case(bc_calc_flux)
  call calc_flux_fluxspe(temp1, temp2, conduct1, conduct2, d1, d2, &
			     vecinter, flux1, flux2, normale, flux_inter, typecalcul )
case(bc_calc_ghostface)
  call calc_flux_fluxface(temp1, temp2, gradtemp1, gradtemp2, conduct1, &
  			  conduct2, d1, d2, vecinter, normale, &
  			  flux_inter, typecalcul)
case(bc_calc_ghostcell)
!  call calc_flux_cellfict(temp1, temp2, gradtemp1, gradtemp2, conduct1, &
!  			  conduct2, d1, d2, vecinter, normale, &
!  			  flux_interface, typecalcul)
case default
  call erreur("Lecture de menu raccord","methode de calcul de raccord non reconnue")

endselect

  
endsubroutine calc_fluxinter_kdif
