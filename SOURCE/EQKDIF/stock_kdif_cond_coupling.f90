!------------------------------------------------------------------------------!
! Procedure :  stock_kdif_cond_coupling   Auteur : E. Radenac
!                                         Date   : Mai 2003
! Fonction                                Modif  : Juin 2003
!   Attribution de ses conditions aux limites de couplage à une zone.
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

subroutine stock_kdif_cond_coupling(bocokdif, temp, flux, if)

use TYPHMAKE
use OUTPUT
use DEFFIELD
use MENU_KDIF

implicit none

! -- Declaration des entrées --
real(krp) :: temp     ! température attribuée
real(krp) :: flux     ! flux attribué
integer ::   if       ! indice de la face concernée

! -- Declaration des entrées/sorties --
type(st_boco_kdif) :: bocokdif ! stockage des conditions

! -- Declaration des variables internes --

! -- Debut de la procedure --

bocokdif%temp(if) = temp
bocokdif%flux_nunif(if) = flux



endsubroutine stock_kdif_cond_coupling
