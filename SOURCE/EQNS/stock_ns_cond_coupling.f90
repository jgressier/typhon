!------------------------------------------------------------------------------!
! Procedure :  stock_ns_cond_coupling     Auteur : E. Radenac
!                                         Date   : June 2005
! Fonction                                Modif  : 
!   Attribution de ses conditions aux limites de couplage a une zone.
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

subroutine stock_ns_cond_coupling(bocons, temp, flux, if, t_op)

use TYPHMAKE
use OUTPUT
use DEFFIELD
use MENU_NS

implicit none

! -- Declaration des entrees --
real(krp) :: temp     ! temperature attribuee
real(krp) :: flux     ! flux attribue
integer ::   if       ! indice de la face concernee
real(krp) :: t_op    ! temperature de la cellule opposee

! -- Declaration des entrees/sorties --
type(st_boco_ns) :: bocons ! stockage des conditions

! -- Declaration des variables internes --

! -- Debut de la procedure --

bocons%temp(if) = temp
bocons%flux_nunif(if) = flux
if ((temp-t_op).ne.0) then
  bocons%h_nunif(if) = flux/(temp-t_op)
else
  bocons%h_nunif(if) = 0._krp
endif

bocons%tconv_nunif(if) = t_op

endsubroutine stock_ns_cond_coupling

!------------------------------------------------------------------------------!
! Historique des modifications
!
! june 2005 : creation of routine
!------------------------------------------------------------------------------!

