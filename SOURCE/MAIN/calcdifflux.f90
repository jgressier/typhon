!------------------------------------------------------------------------------!
! Procedure : calcdifflux                 Auteur : E. Radenac
!                                         Date   : Juillet 2003
! Fonction                                Modif  : 
!  Calcul de la difference de flux a appliquer lorsque l'echange entre deux
!  zones n'est pas a chaque pas de temps (correction de flux). Orientation 
!  selon solver.
! Defauts/Limitations/Divers :
!------------------------------------------------------------------------------!
 
subroutine calcdifflux(etatcons1, etatcons2, nfacelim, solvercoupling, &
                       corcoef, connface2)

use OUTPUT
use VARCOM
use DEFZONE
use DEFFIELD
use GEO3D
use TYPHMAKE

implicit none

! -- Declaration des entrees --
integer                    :: nfacelim            ! nombre de faces limites
integer                    :: solvercoupling      ! solvers utilises
real(krp), dimension(nfacelim) &
                           :: corcoef             ! coeff correction de flux
integer, dimension(nfacelim) &
                           :: connface2

! -- Declaration des entrees/sorties --
type(st_scafield), dimension(2) &
                           :: etatcons1, etatcons2 ! stockage des flux cumules
                                                   ! et des differences de flux
                                                   ! pour les deux zones

! -- Declaration des variables internes --

! -- Debut de la procedure --

select case(solvercoupling)
case(kdif_kdif)
  call calcdifflux_kdif(etatcons1, etatcons2, nfacelim, corcoef, connface2)
case default
  call erreur("Incoherence interne (calcdifflux)","type de solveur inconnu")
endselect 

endsubroutine calcdifflux

!------------------------------------------------------------------------------!
! Historique des modifications
!
! juillet 2003 (v0.0.1b): creation de la procedure
! oct 2003              : ajout coefficient de correction de flux
!------------------------------------------------------------------------------!
