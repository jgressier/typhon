!------------------------------------------------------------------------------!
! Procedure : calcdifflux                 Auteur : E. Radenac
!                                         Date   : Juillet 2003
! Fonction                                Modif  : 
!  Calcul de la différence de flux à appliquer lorsque l'échange entre deux
!  zones n'est pas à chaque pas de temps (correction de flux). Orientation 
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

! -- Declaration des entrées --
integer                    :: nfacelim            ! nombre de faces limites
integer                    :: solvercoupling      ! solvers utilisés
real(krp), dimension(nfacelim) &
                           :: corcoef             ! coeff correction de flux
integer, dimension(nfacelim) &
                           :: connface2

! -- Declaration des entrées/sorties --
type(st_scafield), dimension(2) &
                           :: etatcons1, etatcons2 ! stockage des flux cumulés
                                                   ! et des différences de flux
                                                   ! pour les deux zones

! -- Declaration des variables internes --

! -- Debut de la procedure --

select case(solvercoupling)
case(kdif_kdif)
  call calcdifflux_kdif(etatcons1, etatcons2, nfacelim, corcoef, connface2)
case default
  call erreur("Incohérence interne (calcdifflux)","type de solveur inconnu")
endselect 

endsubroutine calcdifflux

!------------------------------------------------------------------------------!
! Historique des modifications
!
! juillet 2003 (v0.0.1b): création de la procédure
! oct 2003              : ajout coefficient de correction de flux
!------------------------------------------------------------------------------!
