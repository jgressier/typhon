!------------------------------------------------------------------------------!
! Procedure : calc_fouriercycle           Auteur : E. Radenac / J. Gressier
!                                         Date   : janvier 2004
! Fonction                                Modif  : (cf historique)
!   Calcul du nombre de Fourier de cycle d'une zone
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

subroutine calc_fouriercycle(lzone, fint, dtcycle, fcycle)

use TYPHMAKE
use OUTPUT
use VARCOM
use MODWORLD

implicit none

! -- Declaration des entrées --
type(st_zone) :: lzone
real(krp)     :: fint ! nombre de Fourier d'intégration
real(krp)     :: dtcycle ! durée de cycle

! -- Declaration des sorties --
real(krp)      :: fcycle

! -- Declaration des variables internes --
real(krp), dimension(:), allocatable :: fourierloc    ! tableau de pas de temps local
integer                              :: ncell    ! nombre de cellules pour le calcul


! -- Debut de la procedure --

select case(lzone%deftime%stab_meth)

case(given_dt)   ! -- Pas de temps imposé --
  fcycle = fint * dtcycle / lzone%deftime%dt

case(stab_cond)  ! -- Calcul par condition de stabilité (deftim%stabnb) --

  select case(lzone%defsolver%typ_solver)
  
  case(solKDIF)

    ncell = lzone%ust_mesh%ncell_int
    allocate(fourierloc(ncell))

    call calc_kdif_fourier(dtcycle, lzone%defsolver%defkdif%materiau, &
                            lzone%ust_mesh, lzone%field, fourierloc, ncell)
    ! -- DEV -- choix du nombre de Fourier global encore à faire

    ! valeur maximale des cellules de la zone
    fcycle = maxval(fourierloc)

    ! moyenne :
    !fcycle = 0
    !do ic = 1, ncell
    !  fcycle = fcycle + fourierloc(ic)
    !enddo
    !fcycle = fcycle/ncell
    
    deallocate(fourierloc)

  case default
    call erreur("incohérence interne (calc_fourier)", "solveur inconnu")
  endselect

endselect


endsubroutine calc_fouriercycle

!------------------------------------------------------------------------------!
! Historique des modifications
!   jan 2004 : création, appel des procédures spécifiques aux solveurs
!------------------------------------------------------------------------------!
