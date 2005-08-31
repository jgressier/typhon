!------------------------------------------------------------------------------!
! Procedure : calc_cpl_flux               Auteur : E. Radenac
!                                         Date   : April 2003
! Fonction                                Modif  : August 2005
!   Computation of heat flux at the interface of two cells belonging to
!   different zones
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

subroutine calc_cpl_flux(temp1, temp2, gradtemp1, gradtemp2, conduct1, &
			       conduct2, d1, d2, vecinter, flux1, flux2, normale, &
			       flux_inter, typecalcul, typemethode)

use TYPHMAKE 
use OUTPUT
use GEO3D
use VARCOM

implicit none

! -- Declaration des entrees --
real(krp)             :: temp1, temp2   ! exchanged temperatures
type(v3d)             :: gradtemp1, gradtemp2 ! exchanged temperature gradients
real(krp)	      :: conduct1, conduct2  ! exchanged conductivities
real(krp)	      :: d1, d2  ! distance from cell center to interface
type(v3d)             :: vecinter ! unitary vector "intercells"	
type(v3d)             :: flux1, flux2 ! exchanged flux densities              
type(v3d)             :: normale ! interface normales
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

  
endsubroutine calc_cpl_flux

!------------------------------------------------------------------------------!
! Changes history
!
! April 2003 : creation of routine, named calc_fluxinter_kdif 
!              (directory EQKDIF)
! August 2005 : routine renamed calc_cpl_flux, moved to MAIN, used by both KDIF
!               and NS applications
!------------------------------------------------------------------------------!
