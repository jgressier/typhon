!------------------------------------------------------------------------------!
! Procedure : calcdifflux_kdif            Auteur : E. Radenac
!                                         Date   : Juillet 2003
! Fonction                                Modif  : 
!  Calcul de la différence de flux à appliquer lorsque l'échange entre deux
!  zones n'est pas à chaque pas de temps (correction de flux). Solver thermique
!
! Defauts/Limitations/Divers :
!------------------------------------------------------------------------------!

subroutine calcdifflux_kdif(etatcons1, etatcons2, nfacelim, corcoef)

use OUTPUT
use DEFZONE
use DEFFIELD
use GEO3D
use TYPHMAKE

implicit none

! -- Declaration des entrées --
integer                    :: nfacelim            ! nombre de faces limites
real(krp)                  :: corcoef             ! coeff correction de flux

! -- Declaration des entrées/sorties --
type(st_scafield), dimension(2) &
                           :: etatcons1, etatcons2 ! stockage des flux cumulés
                                                   ! et des différences de flux
                                                   ! pour les deux zones

! -- Declaration des variables internes --
integer                        :: i              ! indice de face
real(krp)                      :: dif_enflux     ! différence des énergies d'interface dans les deux zones

! -- Debut de la procedure --

! Supplément de flux pour les échanges espacés : calcul de la différence à appliquer

do i=1, nfacelim

! La différence est la "somme" des flux cumulés car ce sont les valeurs algébriques dont on dispose
! (les flux sortant de part et d'autre)
dif_enflux = etatcons2(1)%scal(i) + etatcons1(1)%scal(i)

  etatcons1(2)%scal(i) = -corcoef*dif_enflux
  etatcons2(2)%scal(i) = (-1._krp + corcoef)*dif_enflux

enddo

endsubroutine calcdifflux_kdif

!------------------------------------------------------------------------------!
! Historique des modifications
!
! juillet 2003 (v0.0.1b): création de la procédure
! oct 2003              : ajout coefficient correction de flux
!------------------------------------------------------------------------------!
