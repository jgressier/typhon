!------------------------------------------------------------------------------!
! Procedure :calc_flux_fluxspe_consistant Auteur : E. Radenac
!                                         Date   : Avril 2003
! Fonction                                Modif  : Juin 2003
!   Calcul du flux a l'interface par la methode du flux specifique, par le 
!   biais d'une interpolation consistante
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

subroutine calc_flux_fluxspe_consistant(d1, d2, flux1, flux2, flux_inter)

use TYPHMAKE
use OUTPUT
use GEO3D

implicit none

! -- Declaration des entrees --
real(krp)  :: d1, d2         ! distances des centres de cellules a l'interface                                           
type(v3d)  :: flux1, flux2 ! densite de flux echangees

! -- Declaration des entrees/sorties --
type(v3d) :: flux_inter

! -- Declaration des variables internes --

real(krp) :: a, b

! -- Debut de la procedure --


!attribution des valeurs des parametres de ponderation a et b
!la ponderation est effectuee par les distances

a = d2
b = d1


! flux1 et flux2 doivent etre vectoriels

flux_inter = (a*flux1+b*flux2) /(a+b)

endsubroutine calc_flux_fluxspe_consistant
