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
use MGRID

implicit none

! -- Declaration des entrées --
type(st_zone) :: lzone

! -- Declaration des sorties --
real(krp)      :: dt

! -- Declaration des variables internes --
type(st_grid), pointer               :: pgrid    ! pointeur sur grille
real(krp), dimension(:), allocatable :: dtloc    ! tableau de pas de temps local
integer                              :: ncell    ! nombre de cellules pour le calcul


! -- Debut de la procedure --


! -- Calcul des pas de temps locaux --

select case(lzone%defsolver%typ_solver)

!--------------------------------------------------------
! méthode VOLUMES FINIS
!--------------------------------------------------------
case(solKDIF)

  pgrid => lzone%grid
  dt    =  1.e20        ! DEV (max de real)

  do while (associated(pgrid))   ! BOUCLE sur les grilles (liste chaînée)

    ncell = pgrid%umesh%ncell_int
    allocate(dtloc(ncell))

    select case(lzone%deftime%stab_meth)

    case(given_dt)   ! -- Pas de temps imposé --
      dtloc(1:ncell) = lzone%deftime%dt

    case(stab_cond)  ! -- Calcul par condition de stabilité (deftim%stabnb) --
      select case(lzone%defsolver%typ_solver)
      case(solKDIF)
        call calc_kdif_timestep(lzone%deftime, lzone%defsolver%defkdif%materiau, &
                                pgrid%umesh, pgrid%field, dtloc, ncell)
      case default
        call erreur("incohérence interne (calc_zonetimestep)", "solveur inconnu")
      endselect

    case default
      call erreur("incohérence interne (calc_zonetimestep)", "condition incompatible")
    endselect  

    ! -- DEV -- pas de temps global imposé dans cette version
    dt = min(dt, minval(dtloc))
    deallocate(dtloc)

    ! grille suivante
    pgrid => pgrid%next

  enddo

!--------------------------------------------------------
! méthode LAGRANGIENNE
!--------------------------------------------------------
case(solVORTEX)

  select case(lzone%deftime%stab_meth)
  case(given_dt)   ! -- Pas de temps imposé --
    dt = lzone%deftime%dt
  case default
    call erreur("incohérence interne (calc_zonetimestep)", "condition incompatible")
  endselect  

endselect


endsubroutine calc_zonetimestep

!------------------------------------------------------------------------------!
! Historique des modifications
!
! sept 2003 : création, appel des procédures spécifiques aux solveurs
! mars 2003 : calcul de pas de temps pour méthodes lagrangiennes
! avr  2004 : calcul KDIF sur liste chaînée de grilles
!------------------------------------------------------------------------------!
