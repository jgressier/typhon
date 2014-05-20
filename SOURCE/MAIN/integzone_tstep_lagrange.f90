!------------------------------------------------------------------------------!
! Procedure : integzone_tstep_lagrange        Auteur : J. Gressier
!                                         Date   : Mars 2004
!> @brief
!   Integration de tous les domaines d'une zone sur un pas de temps correspondant 
!   a une iteration
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine integzone_tstep_lagrange(dt, zone)

use TYPHMAKE
use OUTPUT
use VARCOM
use DEFZONE
use LAPACK      ! linear algebra

implicit none

! -- Declaration des entrees --
real(krp)     :: dt              ! pas de temps propre a la zone
type(st_zone) :: zone            ! zone a integrer

! -- Declaration des sorties --
! retour des residus a travers le champ field de la structure zone ??

! -- Declaration des variables internes --

! -- Debut de la procedure --


call error_stop("DEV: no longer available")

!-----------------------------
endsubroutine integzone_tstep_lagrange

!------------------------------------------------------------------------------!
! Historique des modifications
!
! mars 2004 : creation de la procedure
!------------------------------------------------------------------------------!
