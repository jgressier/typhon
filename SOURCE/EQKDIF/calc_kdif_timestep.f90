!------------------------------------------------------------------------------!
! Procedure : calc_kdif_timestep          Auteur : J. Gressier
!                                         Date   : Septembre 2003
! Fonction                                Modif  : (cf historique)
!   Calcul du pas de temps local et global par zone selon solveur
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

subroutine calc_kdif_timestep(lzone, dtloc, ncell)

use TYPHMAKE
use OUTPUT
use VARCOM
use MODWORLD

implicit none

! -- Declaration des entrées --
type(st_zone) :: lzone
integer       :: ncell                    ! nombre de cellules internes

! -- Declaration des sorties --
real(krp), dimension(1:ncell) :: dtloc    ! tableau de pas de temps local

! -- Declaration des variables internes --

! -- Debut de la procedure --



endsubroutine calc_kdif_timestep

!------------------------------------------------------------------------------!
! Historique des modifications
!
!------------------------------------------------------------------------------!
