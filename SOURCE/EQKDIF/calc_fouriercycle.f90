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

! -- Declaration des entrees --
type(st_zone) :: lzone
real(krp)     :: fint ! nombre de Fourier d'integration
real(krp)     :: dtcycle ! duree de cycle

! -- Declaration des sorties --
real(krp)      :: fcycle

! -- Declaration des variables internes --
real(krp), dimension(:), allocatable :: fourierloc    ! tableau de pas de temps local
integer                              :: ncell    ! nombre de cellules pour le calcul


! -- Debut de la procedure --

select case(lzone%defsolver%deftime%stab_meth)

case(given_dt)   ! -- Pas de temps impose --
  fcycle = fint * dtcycle / lzone%defsolver%deftime%dt

case(stab_cond)  ! -- Calcul par condition de stabilite (deftim%stabnb) --

  select case(lzone%defsolver%typ_solver)
  
  case(solKDIF)

    ncell = lzone%gridlist%first%umesh%ncell_int
    allocate(fourierloc(ncell))

    call calc_kdif_fourier(dtcycle, lzone%defsolver%defkdif%materiau, &
                            lzone%gridlist%first%umesh, lzone%gridlist%first%field, fourierloc, ncell)
    ! -- DEV -- choix du nombre de Fourier global encore a faire

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
    call erreur("incoherence interne (calc_fourier)", "solveur inconnu")
  endselect

endselect


endsubroutine calc_fouriercycle

!------------------------------------------------------------------------------!
! Historique des modifications
!   jan 2004 : creation, appel des procedures specifiques aux solveurs
!------------------------------------------------------------------------------!
