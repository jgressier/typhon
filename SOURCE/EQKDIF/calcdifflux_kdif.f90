!------------------------------------------------------------------------------!
! Procedure : calcdifflux_kdif            Auteur : E. Radenac
!                                         Date   : Juillet 2003
! Fonction                                Modif  : 
!  Calcul de la différence de flux à appliquer lorsque l'échange entre deux
!  zones n'est pas à chaque pas de temps (correction de flux). Solver thermique
!
! Defauts/Limitations/Divers :
!------------------------------------------------------------------------------!

subroutine calcdifflux_kdif(etatcons1, etatcons2, nfacelim)

use OUTPUT
use DEFZONE
use DEFFIELD
use GEO3D
use TYPHMAKE

implicit none

! -- Declaration des entrées --
integer                    :: nfacelim            ! nombre de faces limites

! -- Declaration des entrées/sorties --
type(st_scafield), dimension(2) &
                           :: etatcons1, etatcons2 ! stockage des flux cumulés
                                                   ! et des différences de flux
                                                   ! pour les deux zones

! -- Declaration des variables internes --
integer                        :: i              ! indice de face
real(krp)                      :: a              ! coefficient correcteur
real(krp)                      :: dif_enflux     ! différence des énergies d'interface dans les deux zones

! -- Debut de la procedure --

! Supplément de flux pour les échanges espacés : calcul de la différence à appliquer

do i=1, nfacelim

! La différence est la "somme" des flux cumulés car ce sont les valeurs algébriques dont on dispose
! (les flux sortant de part et d'autre)
dif_enflux = etatcons2(1)%scal(i) + etatcons1(1)%scal(i)

  !1ere possibilité
   a = -0.5_krp

  !2eme possibilité : on garde le flux maximum
!  a = -1._krp

  !3eme possibilité : on garde le flux minimum
!  a = 0._krp

  etatcons1(2)%scal(i) = a*dif_enflux
  etatcons2(2)%scal(i) = (-1._krp-a)*dif_enflux

  !4eme possibilite : maximum et minimum en alternance : à faire

enddo

endsubroutine calcdifflux_kdif

!------------------------------------------------------------------------------!
! Historique des modifications
!
! juillet 2003 (v0.0.1b): création de la procédure
!------------------------------------------------------------------------------!
