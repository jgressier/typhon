!------------------------------------------------------------------------------
! Procedure : choixcorrection             Auteur : E. Radenac
!                                         Date   : Février 2004
! Fonction                                Modif  : 
!   Choix de la correction de flux
!
! Defauts/Limitations/Divers : pour l'instant, une methode de calcul commune 
!				aux deux zones
!------------------------------------------------------------------------------

subroutine choixcorrection(zone1, zone2, dtexch, placement, corcoef)

use OUTPUT
use VARCOM
use DEFZONE
use DEFFIELD
use GEO3D
use TYPHMAKE

implicit none

! -- Declaration des entrées --
type(st_zone)              :: zone1, zone2
real(krp)                  :: dtexch             ! pas de temps entre 
                                                 ! deux échanges

! -- Declaration des sorties --
integer                    :: placement     ! variable pour le
                                            ! placement des corrections
real(krp)                  :: corcoef       ! coefficient de correction

! -- Declaration des variables internes --
real(krp)                      :: rap_f ! rapport des nb de Fourier des 2 zones
real(krp)                      :: fcycle1, fcycle2 ! nb de Fourier de cycle
                                                   ! des deux zones

! -- Debut de la procedure --

! Calcul du nombre de Fourier des deux zones
call calc_fourier(zone1, zone1%deftime%stabnb)
call calc_fourier(zone2, zone2%deftime%stabnb)

! Rapport de ces nombres de Fourier
! En fonction de la valeur, orientation vers une correction AVANT ou APRES
! et choix de la valeur du coefficient de correction
rap_f = zone2%deftime%stabnb / zone1%deftime%stabnb

if (rap_f == 1) then

  ! Calcul du nombre de Fourier de cycle (basé sur la durée entre deux 
  ! échanges 
  ! et sur le pas de maillage)
  call calc_fouriercycle(zone1, zone1%deftime%stabnb, dtexch, fcycle1)
  call calc_fouriercycle(zone2, zone2%deftime%stabnb, dtexch, fcycle2)

  ! si les nb de Fourier de cycle des deux zones sont inférieurs à 3 
  ! (en théorie 1D : 4), on effectue la correction avant, plus précise
  ! avec un coefficient de correction de 0.5
  ! sinon : correction après avec le meme coef de correction
  if ((fcycle1 .lt. 3).and.(fcycle2 .lt. 3)) then
    placement = avant
    corcoef = 0.5
  endif

else if (rap_f .lt. 0.1) then
  ! cas où on peut placer la correction avant avec un coef de correction de 0
  placement = avant
  corcoef = 0

else if (rap_f .gt. 10) then
  ! cas où on peut placer la correction avant avec un coef de correction de 1
  placement = avant
  corcoef = 1

endif

endsubroutine choixcorrection

!------------------------------------------------------------------------------
! Historique des modifications
!
! février 2003 : création de la procédure
!
!------------------------------------------------------------------------------
