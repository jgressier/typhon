!------------------------------------------------------------------------------!
! Procedure : calc_fluxinter_kdif         Auteur : E. Radenac
!                                         Date   : Avril 2003
! Fonction                                Modif  : Juin 2003
!   Calcul du flux de conduction de la chaleur à l'interface entre deux cellules
!   appartenant à deux zones différentes
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

! -- Declaration des entrées --
real(krp)             :: temp1, temp2   ! températures échangées
type(v3d)             :: gradtemp1, gradtemp2 ! gradients de température échangés
real(krp)	      :: conduct1, conduct2  ! conductivités échangées
real(krp)	      :: d1, d2  ! distance entre les centres des cellules et l'interface
type(v3d)             :: vecinter ! vecteur unitaire "intercellules"	
type(v3d)             :: flux1, flux2 ! densités de flux échangées                         
type(v3d)             :: normale ! normales à l'interface
integer               :: typecalcul
integer               :: typemethode

! -- Declaration des entrées/sorties --
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
  call erreur("Lecture de menu raccord","méthode de calcul de raccord non reconnue")

endselect

  
endsubroutine calc_fluxinter_kdif
