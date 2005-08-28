!------------------------------------------------------------------------------!
! Procedure : init_connect_grid                         Authors : J. Gressier
!                                                       Created : March 2004
! Fonction
!   Calcul des connectivites supplementaires (conditions limites)
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine init_connect_grid(defsolver, grid)

use TYPHMAKE
use STRING
use VARCOM
use OUTPUT
use MGRID
use MENU_SOLVER

implicit none

! -- Declaration des entrees --
type(mnu_solver) :: defsolver            ! parametres du solveur

! -- Declaration des entrees/sorties --
type(st_grid)    :: grid                 ! maillage et connectivites

! -- Declaration des sorties --

! -- Declaration des variables internes --
integer :: ib, idef                      ! index de conditions aux limites et index de definition
logical :: same_name

! -- Debut de la procedure --

call print_info(10,"  Grid "//name(grid))

write(str_w,'(a,i8,a,i8,a,i7,a)') "  connectivity :",grid%umesh%ncell," cells included",&
                                  grid%umesh%ncell_int," internal cells &",&
                                  grid%umesh%ncell_lim," ghost cells"
call print_info(10, str_w)
write(str_w,'(a,i8,a,i8,a,i7,a)') "  connectivity :",grid%umesh%nface," faces included",&
                                  grid%umesh%nface_int," internal faces &",&
                                  grid%umesh%nface_lim," boundary faces"
call print_info(10, str_w)

call print_info(8, ". initializing boundary faces to ghost cells")

! -- Definition des connectivites faces limites -> cellules limites

grid%umesh%ncell_lim = 0     ! initialisation du compteur de cellules limites

! Boucle sur les conditions aux limites

do ib = 1, grid%umesh%nboco

  ! recherche d'une definition (boco) par nom de famille

  same_name = .false.
  idef      = 0
  do while ((.not.same_name).and.(idef+1 <= defsolver%nboco))
    idef      = idef + 1
    same_name = samestring(grid%umesh%boco(ib)%family, defsolver%boco(idef)%family)
  enddo
  if (same_name) then
    grid%umesh%boco(ib)%idefboco = idef
  else
    call erreur("Error in matching family names", &
                "impossible to find "//trim(grid%umesh%boco(ib)%family)// &
                " in the parameter definition")
  endif

  ! affectation 

  select case(defsolver%boco(idef)%typ_calc)
  case(bc_calc_ghostcell)
    call init_ustboco_ghostcell(ib, defsolver%boco(idef), grid%umesh)
  case(bc_calc_ghostface)
    call init_ustboco_ghostface(ib, defsolver%boco(idef), grid%umesh)
  case(bc_calc_singpanel)
    call init_ustboco_singpanel(ib, defsolver%boco(idef), grid)
  case(bc_calc_kutta)
    call init_ustboco_kutta(ib, defsolver%boco(idef), grid)
  case default
    call erreur("Internal error (init_connect_grid)","unknown type of boundary conditions")
  endselect 

enddo


endsubroutine init_connect_grid

!------------------------------------------------------------------------------!
! Changes history
!
! Mar 2004 : creation  (copied from init_connect_ust)
!------------------------------------------------------------------------------!
