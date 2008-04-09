!------------------------------------------------------------------------------!
! Procedure : treelevel_explicit                    Auteur : J. Gressier
!                                                Date   : March 2006
! Fonction                                       Modif  : (cf history)
!   Time Integration during one timestep of ONE LEVEL of UST grid TREE structure
!
!------------------------------------------------------------------------------!
subroutine treelevel_explicit(dt, info, defsolver, gridlist, coupling, ncoupling)

use TYPHMAKE
use OUTPUT
use VARCOM
use MGRID
use MENU_SOLVER
use MODINFO
use MENU_ZONECOUPLING

implicit none

! -- Inputs --
real(krp)         :: dt              ! timestep for this level
type(st_infozone) :: info            ! zone information structure
type(mnu_solver)  :: defsolver       ! solver parameters
type(st_gridlist) :: gridlist        ! list of grids
integer           :: ncoupling        ! nombre de couplages de la zone

! -- Outputs --
type(mnu_zonecoupling), dimension(1:ncoupling) &
                 :: coupling ! donnees de couplage
! retour des residus a travers le champ field de la structure zone

! -- Internal variables --
type(st_grid), pointer :: pgrid
integer                :: if

! -- Body --

! -- Preparation du calcul --

pgrid => gridlist%first
do while (associated(pgrid))
  call calc_varprim(defsolver, pgrid%info%field_loc)     ! calcul des var. primitives
  pgrid => pgrid%next
enddo

! -- calcul des conditions aux limites pour tous les domaines --

call conditions_limites(info, defsolver, gridlist)
    
! -- gradients are computed only if necessary (by selected methods)

if (defsolver%defspat%calc_grad) then
  pgrid => gridlist%first
  do while (associated(pgrid))
    call calc_gradient(defsolver, defsolver%defspat, pgrid,                 &
                       pgrid%info%field_loc%etatprim, pgrid%info%field_loc%gradient)
    call calc_gradient_limite(defsolver, pgrid%umesh, pgrid%info%field_loc%gradient)
    pgrid => pgrid%next
  enddo
endif

! -- integration des domaines --

pgrid => gridlist%first
do while (associated(pgrid))

  ! DEV : changer les structures de couplages dans MGRID
  call integration_grid(dt, info%time_model, defsolver, &
                        pgrid, coupling, ncoupling)

  ! Desallocation des eventuelles listes chainees de champ generique utilisees
  if (pgrid%nbocofield .ne. 0) then
    call delete_chainedgfield(pgrid%bocofield)
    pgrid%nbocofield = 0
  endif

  pgrid => pgrid%next

enddo


!-----------------------------
endsubroutine treelevel_explicit

!------------------------------------------------------------------------------!
! Changes history
!
! Mar  2006: created from integzone_tstep_usttree
! Nov  2007: only compute RHS, updating is done by calling routine, changed name
!------------------------------------------------------------------------------!
