!------------------------------------------------------------------------------!
! Procedure : calc_fourier                Auteur : E. Radenac / J. Gressier
!                                         Date   : janvier 2004
! Fonction                                Modif  : (cf historique)
!   Calcul du nombre de Fourier global d'une zone
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

subroutine calc_fourier(lzone, fourier)

use TYPHMAKE
use OUTPUT
use VARCOM
use MODWORLD

implicit none

! -- Declaration des entrées --
type(st_zone) :: lzone

! -- Declaration des sorties --
real(krp)      :: fourier

! -- Declaration des variables internes --
real(krp), dimension(:), allocatable :: fourierloc    ! tableau de nb de Fourier local
integer                              :: ncell    ! nombre de cellules pour le calcul


! -- Debut de la procedure --


! -- Calcul des nombres de Fourier locaux --

ncell = lzone%grid%umesh%ncell_int
allocate(fourierloc(ncell))

select case(lzone%deftime%stab_meth)
case(stab_cond)   ! -- Pas de temps imposé --
  fourierloc(1:ncell) = lzone%deftime%stabnb

case(given_dt)  ! -- Calcul par condition de stabilité (deftim%stabnb) --
  select case(lzone%defsolver%typ_solver)
  case(solKDIF)
    call calc_kdif_fourier(lzone%deftime%dt, lzone%defsolver%defkdif%materiau,&
                            lzone%grid%umesh, lzone%grid%field, fourierloc, ncell)
  case default
    call erreur("incohérence interne (calc_fourier)", "solveur inconnu")
  endselect

endselect

! -- DEV -- choix du nombre de Fourier global encore à faire

! valeur maximale des cellules de la zone
fourier = maxval(fourierloc)

! moyenne :
!fourier = 0
!do ic = 1, ncell
!  fourier = fourier + fourierloc(ic)
!enddo
!fourier = fourier/ncell

deallocate(fourierloc)



endsubroutine calc_fourier

!------------------------------------------------------------------------------!
! Historique des modifications
!   jan 2004 : création, appel des procédures spécifiques aux solveurs
!------------------------------------------------------------------------------!
