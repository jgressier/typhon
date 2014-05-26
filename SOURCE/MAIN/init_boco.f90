!------------------------------------------------------------------------------!
! Procedure : init_boco                   Auteur : J. Gressier/ E. Radenac
!                                         Date   : Novembre 2003
! Fonction                                Modif  : (cf historique)
!   Initialisation des conditions limites
!
!------------------------------------------------------------------------------!

subroutine init_boco(zone)

use TYPHMAKE
use VARCOM
use OUTPUT
use DEFZONE
use DEFFIELD
use MENU_SOLVER

implicit none

! -- INPUTS --
type(st_zone)    :: zone               ! maillage et connectivites

! -- OUTPUTS --

! -- Internal variables --
type(st_grid), pointer :: pgrid
integer                :: ig, ib, if
integer                :: nsca, nvec, nten


! -- BODY --

!-------------------------------------------------------------
! Solver dependent INITIALIZATION

select case(zone%defsolver%typ_solver)

case(solNS)
  pgrid => zone%gridlist%first
  do while (associated(pgrid))
    call init_boco_ns(zone%defsolver, pgrid%umesh)
    pgrid => pgrid%next
  enddo

case(solKDIF)
  if (zone%gridlist%nbgrid /= 1) call erreur("Init BOCO","only one grid allowed in KDIF solver")
  call init_boco_kdif(zone%defsolver, zone%gridlist%first%umesh)

case(solVORTEX)
  pgrid => zone%gridlist%first
  do while (associated(pgrid))
    call init_boco_vort(zone%defsolver, pgrid)
    pgrid => pgrid%next
  enddo

case default
  call erreur("Incoherence interne (init_boco_ust)","type de solveur inconnu")
endselect 

!-------------------------------------------------------------
! HISTORY structures

!! DEV !! must initialize a common structure for all BOCO when mpi computation (in defsolver ?)

pgrid => zone%gridlist%first            ! ----------------------------------------
do while (associated(pgrid))            ! loop on grids in a zone

  nsca = pgrid%info%field_loc%nscal
  nvec = pgrid%info%field_loc%nvect
  nten = 0

  do ib = 1, pgrid%umesh%nboco          ! loop on boco in grid

    ! compute surface
    pgrid%umesh%boco(ib)%area = 0._krp
    do if = 1, pgrid%umesh%boco(ib)%nface
      pgrid%umesh%boco(ib)%area = pgrid%umesh%boco(ib)%area &
                                  + pgrid%umesh%mesh%iface(pgrid%umesh%boco(ib)%iface(if), 1, 1)%surface
    enddo

    ! -- init field (dim=1 quantity ~ average/integral) and scal/vect size of main field --

    call new_genericfield(pgrid%umesh%boco(ib)%avg_quant, 1*zone%defsolver%nsim, nsca, nvec, 0)
    call new_genericfield(pgrid%umesh%boco(ib)%sum_flux,  1*zone%defsolver%nsim, nsca, nvec, 0)
    !call print_info(11, "initializing boco "//"")
  enddo

  pgrid => pgrid%next
enddo



endsubroutine init_boco

!------------------------------------------------------------------------------!
! Changes history
!
! nov  2003: creation de la procedure
! mars 2004: fusion "init_boco_ust" dans "init_boco"
!            ajout du solveur VORTEX
! july 2004: NS solver (call init_boco_ns)
! Apr  2008: init history structures (and boco area) 
!------------------------------------------------------------------------------!

