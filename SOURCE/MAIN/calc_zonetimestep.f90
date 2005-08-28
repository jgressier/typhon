!------------------------------------------------------------------------------!
! Procedure : calc_zonetimestep           Auteur : J. Gressier
!                                         Date   : Septembre 2003
! Fonction                                Modif  : (cf historique)
!   Calcul du pas de temps local et global par zone selon solveur
!
! Defauts/Limitations/Divers :
!   ATTENTION : a ce moment, les variables primitives ne sont pas calculees
!
!------------------------------------------------------------------------------!

subroutine calc_zonetimestep(lzone, dt)

use TYPHMAKE
use OUTPUT
use VARCOM
use MODWORLD
use MGRID

implicit none

! -- Declaration des entrees --
type(st_zone) :: lzone

! -- Declaration des sorties --
real(krp)      :: dt

! -- Declaration des variables internes --
type(st_grid), pointer               :: pgrid    ! pointeur sur grille
real(krp), dimension(:), allocatable :: dtloc    ! tableau de pas de temps local
real(krp)                            :: cfl
integer                              :: ncell    ! nombre de cellules pour le calcul


! -- Debut de la procedure --


! -- Calcul des pas de temps locaux --

select case(lzone%defsolver%typ_solver)

!--------------------------------------------------------
! methode VOLUMES FINIS
!--------------------------------------------------------
case(solKDIF, solNS)

  pgrid => lzone%grid
  dt    =  1.e20        ! DEV (max de real)

  do while (associated(pgrid))   ! BOUCLE sur les grilles (liste chainee)

    ncell = pgrid%umesh%ncell_int
    allocate(dtloc(ncell))

    select case(lzone%deftime%stab_meth)

    case(given_dt)   ! -- Pas de temps impose --
      dtloc(1:ncell) = lzone%deftime%dt

    case(stab_cond)  ! -- Calcul par condition de stabilite (deftim%stabnb) --
      select case(lzone%defsolver%typ_solver)
      case(solNS)
        cfl = lzone%deftime%stabnb*(1._krp-log10(lzone%info%cur_res/lzone%info%residu_ref))
        cfl = min(cfl, lzone%deftime%stabnb_max)
        call calc_ns_timestep(cfl, lzone%defsolver%defns%properties(1), &
                              pgrid%umesh, pgrid%info%field_loc, dtloc, ncell)
      case(solKDIF)
        call calc_kdif_timestep(lzone%deftime, lzone%defsolver%defkdif%materiau, &
                                pgrid%umesh, pgrid%info%field_loc, dtloc, ncell)
      case default
        call erreur("incoherence interne (calc_zonetimestep)", "solveur inconnu")
      endselect

    case default
      call erreur("incoherence interne (calc_zonetimestep)", "condition incompatible")
    endselect  

    ! -- DEV -- pas de temps global impose dans cette version
    dt = min(dt, minval(dtloc))
    deallocate(dtloc)

    ! grille suivante
    pgrid => pgrid%next

  enddo

!--------------------------------------------------------
! methode LAGRANGIENNE
!--------------------------------------------------------
case(solVORTEX)

  select case(lzone%deftime%stab_meth)
  case(given_dt)   ! -- Pas de temps impose --
    dt = lzone%deftime%dt
  case default
    call erreur("incoherence interne (calc_zonetimestep)", "condition incompatible")
  endselect  

!--------------------------------------------------------
case default
  call erreur("Developpement","solveur inattendu (calc_zonetimestep)")

endselect


endsubroutine calc_zonetimestep

!------------------------------------------------------------------------------!
! Historique des modifications
!
! sept 2003 : creation, appel des procedures specifiques aux solveurs
! mars 2003 : calcul de pas de temps pour methodes lagrangiennes
! avr  2004 : calcul KDIF sur liste chainee de grilles
! july 2004 : NS solver call
! oct  2004 : field chained list
!------------------------------------------------------------------------------!
