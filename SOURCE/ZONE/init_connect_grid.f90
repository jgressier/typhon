!------------------------------------------------------------------------------!
! Procedure : init_connect_grid           Auteur : J. Gressier
!                                         Date   : Mars 2004
! Fonction                                Modif  : (cf historique)
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

call print_info(10,"  Grille "//name(grid))

write(str_w,'(a,i8,a,i8,a,i7,a)') "  connectivite :",grid%umesh%ncell," cellules dont",&
                                  grid%umesh%ncell_int," internes et",&
                                  grid%umesh%ncell_lim," limites"
call print_info(10, str_w)
write(str_w,'(a,i8,a,i8,a,i7,a)') "  connectivite :",grid%umesh%nface," faces    dont",&
                                  grid%umesh%nface_int," internes et",&
                                  grid%umesh%nface_lim," limites"
call print_info(10, str_w)

call print_info(8, ". initialisation des connectivites faces limites -> cellules limites")

! -- Definition des connectivites faces limites -> cellules limites

grid%umesh%ncell_lim = 0     ! initialisation du compteur de cellules limites

! Boucle sur les conditions aux limites

do ib = 1, grid%umesh%nboco

  ! recherche d'une definition (boco) par nom de famille

  same_name = .false.
  idef      = 0
  do while ((.not.same_name).and.(idef+1 <= defsolver%nboco))
    idef      = idef + 1
    !print*,'debug:',grid%umesh%boco(ib)%family, defsolver%boco(idef)%family
    same_name = samestring(grid%umesh%boco(ib)%family, defsolver%boco(idef)%family)
  enddo
  if (same_name) then
    grid%umesh%boco(ib)%idefboco = idef
  else
    call erreur("Definition des conditions aux limites", &
                "la definition de la famille "//trim(grid%umesh%boco(ib)%family)// &
                " est introuvable")
  endif

  ! affectation 

  select case(defsolver%boco(idef)%typ_calc)
  case(bc_calc_ghostface)
    call init_ustboco_ghostface(ib, defsolver%boco(idef), grid%umesh)
  case(bc_calc_singpanel)
    call init_ustboco_singpanel(ib, defsolver%boco(idef), grid)
  case(bc_calc_kutta)
    call init_ustboco_kutta(ib, defsolver%boco(idef), grid)
  case default
    call erreur("Incoherence interne (init_connect_grid)","type d'implementation boco inconnu")
  endselect 

enddo

write(str_w,'(a,i8,a,i8,a,i7,a)') "  connectivite :",grid%umesh%ncell," cellules dont",&
                                  grid%umesh%ncell_int," internes et",&
                                  grid%umesh%ncell_lim," limites"
call print_info(10, str_w)
write(str_w,'(a,i8,a,i8,a,i7,a)') "  connectivite :",grid%umesh%nface," faces    dont",&
                                  grid%umesh%nface_int," internes et",&
                                  grid%umesh%nface_lim," limites"
call print_info(10, str_w)

endsubroutine init_connect_grid

!------------------------------------------------------------------------------!
! Historique des modifications
!
! mars 2004 : creation de la procedure (copie de init_connect_grid)
!------------------------------------------------------------------------------!
