!------------------------------------------------------------------------------!
! Procedure :  stock_kdif_cond_coupling   Auteur : E. Radenac
!                                         Date   : Mai 2003
! Fonction                                Modif  : Juin 2003
!   Attribution de ses conditions aux limites de couplage à une zone.
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

subroutine stock_kdif_cond_coupling(conditions_coupling, temp, flux, if)

use TYPHMAKE
use OUTPUT
use DEFFIELD

implicit none

! -- Declaration des entrées --
real(krp) :: temp     ! température attribuée
real(krp) :: flux     ! flux attribué
integer ::   if       ! indice de la face concernée

! -- Declaration des entrées/sorties --
type(st_genericfield) :: conditions_coupling ! stockage des conditions

! -- Declaration des variables internes --

! -- Debut de la procedure --

conditions_coupling%tabscal(1)%scal(if) = temp
conditions_coupling%tabscal(2)%scal(if) = flux

endsubroutine stock_kdif_cond_coupling
