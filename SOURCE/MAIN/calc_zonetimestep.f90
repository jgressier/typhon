!------------------------------------------------------------------------------!
! Procedure : calc_zonetimestep           Auteur : J. Gressier
!                                         Date   : Septembre 2003
! Fonction                                Modif  : (cf historique)
!   Calcul du pas de temps local et global par zone selon solveur
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

subroutine calc_zonetimestep(lzone, dt)

use TYPHMAKE
use OUTPUT
use VARCOM
use MODWORLD

implicit none

! -- Declaration des entrées --
type(st_zone) :: lzone

! -- Declaration des sorties --
real(krp)      :: dt

! -- Declaration des variables internes --
real(krp), dimension(:), allocatable :: dtloc    ! tableau de pas de temps local
integer                              :: ncell    ! nombre de cellules pour le calcul


! -- Debut de la procedure --

ncell = lzone%ust_mesh%ncell_int
allocate(dtloc(ncell))

select case(lzone%defsolver%typ_solver)
case(solKDIF)
  call calc_kdif_timestep(lzone, dtloc, ncell)
case default
  call erreur("incohérence interne (calc_zonetimestep)", "solveur inconnu")
endselect

deallocate(dtloc)

endsubroutine calc_zonetimestep

!------------------------------------------------------------------------------!
! Historique des modifications
!
!------------------------------------------------------------------------------!
