!------------------------------------------------------------------------------!
! Procedure :  stock_kdif_cond_coupling   Auteur : E. Radenac
!                                         Date   : Mai 2003
! Fonction                                Modif  : Juin 2003
!   Attribution de ses conditions aux limites de couplage a une zone.
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

subroutine stock_kdif_cond_coupling(bocokdif, temp, flux, if, t_op)

use TYPHMAKE
use OUTPUT
use DEFFIELD
use MENU_KDIF

implicit none

! -- Declaration des entrees --
real(krp) :: temp     ! temperature attribuee
real(krp) :: flux     ! flux attribue
integer ::   if       ! indice de la face concernee
real(krp) :: t_op    ! temperature de la cellule opposee

! -- Declaration des entrees/sorties --
type(st_boco_kdif) :: bocokdif ! stockage des conditions

! -- Declaration des variables internes --

! -- Debut de la procedure --

bocokdif%temp(if) = temp
!bocokdif%flux_nunif(if) = flux
bocokdif%flux_nunif(if) = 0

if ((temp-t_op).ne.0) then
  bocokdif%h_nunif(if) = flux/(temp-t_op)
else
  bocokdif%h_nunif(if) = 0._krp
endif

bocokdif%tconv_nunif(if) = t_op

endsubroutine stock_kdif_cond_coupling
