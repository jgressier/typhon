!------------------------------------------------------------------------------!
! Procedure : init_connect_ust            Auteur : J. Gressier
!                                         Date   : Mars 2003
! Fonction                                Modif  :
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine init_connect_ust(defsolver, ust_mesh)

use TYPHMAKE
use STRING
use VARCOM
use OUTPUT
use USTMESH
use MENU_SOLVER

implicit none

! -- Declaration des entrées --
type(mnu_solver) :: defsolver            ! paramètres du solveur

! -- Declaration des entrées/sorties --
type(st_ustmesh) :: ust_mesh             ! maillage et connectivités

! -- Declaration des sorties --

! -- Declaration des variables internes --
integer :: ib, idef                      ! index de conditions aux limites et index de définition
logical :: same_name

! -- Debut de la procedure --

write(str_w,'(a,i6,a,i6,a,i6,a)') "  connectivité :",ust_mesh%ncell," cellules dont",&
                                  ust_mesh%ncell_int," internes et",&
                                  ust_mesh%ncell_lim," limites"
call print_info(10, str_w)
write(str_w,'(a,i6,a,i6,a,i6,a)') "  connectivité :",ust_mesh%nface," faces dont",&
                                  ust_mesh%nface_int," internes et",&
                                  ust_mesh%nface_lim," limites"
call print_info(10, str_w)

call print_info(8, ". initialisation des connectivités faces limites -> cellules limites")

! -- Définition des connectivités faces limites -> cellules limites

ust_mesh%ncell_lim = 0     ! initialisation du compteur de cellules limites

! Boucle sur les conditions aux limites

do ib = 1, ust_mesh%nboco

  ! recherche d'une définition (boco) par nom de famille

  same_name = .false.
  idef      = 0
  do while ((.not.same_name).and.(idef+1 <= defsolver%nboco))
    idef      = idef + 1
    same_name = samestring(ust_mesh%boco(ib)%family, defsolver%boco(idef)%family)
  enddo
  if (same_name) then
    ust_mesh%boco(ib)%idefboco = idef
  else
    call erreur("Définition des conditions aux limites", &
                "la définition de la famille "//trim(ust_mesh%boco(ib)%family)// &
                " est introuvable")
  endif

  ! affectation 

  select case(defsolver%boco(idef)%typ_calc)
  case(bc_calc_ghostface)
    call init_ustboco_ghostface(ib, defsolver%boco(idef), ust_mesh)
  case default
    call erreur("Incohérence interne (init_connect_ust)","type d'implémentation boco inconnu")
  endselect 

enddo

write(str_w,'(a,i6,a,i6,a,i6,a)') "  connectivité :",ust_mesh%ncell," cellules dont",&
                                  ust_mesh%ncell_int," internes et",&
                                  ust_mesh%ncell_lim," limites"
call print_info(10, str_w)
write(str_w,'(a,i6,a,i6,a,i6,a)') "  connectivité :",ust_mesh%nface," faces dont",&
                                  ust_mesh%nface_int," internes et",&
                                  ust_mesh%nface_lim," limites"
call print_info(10, str_w)

endsubroutine init_connect_ust

!------------------------------------------------------------------------------!
! Historique des modifications
!
! mars 2003 (v0.0.1b): création de la procédure
!------------------------------------------------------------------------------!
