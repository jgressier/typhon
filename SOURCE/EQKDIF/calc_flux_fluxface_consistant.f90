!------------------------------------------------------------------------------!
! Procedure :calc_flux_fluxface_consistantAuteur : E. Radenac
!                                         Date   : Mai 2003
! Fonction                                Modif  : Juin 2003
!   Calcul du flux a l'interface par la methode du flux de face, par le 
!   biais d'une interpolation consistante
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

subroutine calc_flux_fluxface_consistant(gradtemp1, gradtemp2, conduct1, conduct2, d1, d2, &
					flux_inter)

use TYPHMAKE
use OUTPUT
use GEO3D

implicit none

! -- Declaration des entrees --
real(krp)  :: d1, d2         ! distances des centres de cellules a l'interface
real(krp)  :: conduct1, conduct2 !  conductivites des materiaux                                         
type(v3d)  :: gradtemp1, gradtemp2 ! gradients de temperature echanges

! -- Declaration des entrees/sorties --
type(v3d) :: flux_inter

! -- Declaration des variables internes --

real(krp) :: a, b
real(krp) :: conduct
type(v3d) :: gradtemp

! -- Debut de la procedure --


!attribution des valeurs des parametres de ponderation a et b
!la ponderation est effectuee par les distances

a = conduct1/d1
b = conduct2/d2

! "conductivite" a l'interface :

conduct = (d2*conduct1 + d1*conduct2)/(d1 + d2)

! "gradient de temperature" a l'interface :

gradtemp = (a*gradtemp1 + b*gradtemp2) / (a+b)

flux_inter = -conduct * gradtemp

endsubroutine calc_flux_fluxface_consistant
