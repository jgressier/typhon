!------------------------------------------------------------------------------!
! Procedure :  stock_kdif_cond_coupling   Auteur : E. Radenac
!                                         Date   : Mai 2003
! Fonction                                Modif  : Juin 2003
!   Attribution de ses conditions aux limites de couplage à une zone.
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

! -- Declaration des entrées --
real(krp) :: temp     ! température attribuée
real(krp) :: flux     ! flux attribué
integer ::   if       ! indice de la face concernée
real(krp) :: t_op    ! température de la cellule opposée

! -- Declaration des entrées/sorties --
type(st_boco_kdif) :: bocokdif ! stockage des conditions

! -- Declaration des variables internes --

! -- Debut de la procedure --

bocokdif%temp(if) = temp
bocokdif%flux_nunif(if) = flux
if ((temp-t_op).ne.0) then
  bocokdif%h_nunif(if) = flux/(temp-t_op)
else
  bocokdif%h_nunif(if) = 0._krp
endif

bocokdif%tconv_nunif(if) = t_op

endsubroutine stock_kdif_cond_coupling
