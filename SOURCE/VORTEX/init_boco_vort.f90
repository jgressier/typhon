!------------------------------------------------------------------------------!
! Procedure : init_boco_vort              Auteur : J. Gressier
!                                         Date   : Mars 2004
! Fonction                                Modif  : (cf historique)
!   Traitement des paramètres du fichier menu principal
!   Initialisation des conditions limites du solveur VORTEX
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine init_boco_vort(defsolver, grid)

use TYPHMAKE
use DEFFIELD
use MENU_VORTEX
use USTMESH
use MENU_SOLVER
use VARCOM

implicit none

! -- Declaration des entrées --
type(st_grid)  :: grid

! -- Declaration des entrées/sorties --
type(mnu_solver)  :: defsolver

! -- Declaration des variables internes --
integer :: iboco, idef

! -- Debut de la procedure --

! On parcourt toutes les conditions limites du domaine

do iboco = 1, grid%umesh%nboco 

  idef = grid%umesh%boco(iboco)%idefboco
  if (defsolver%boco(idef)%typ_calc == bc_calc_singpanel) then 
    call new(grid%bocofield, grid%umesh%boco(iboco)%nface+1, 1, 0, 0)
  endif

enddo

endsubroutine init_boco_vort

!------------------------------------------------------------------------------!
! Historique des modifications
!
! mars 2004 : création de la routine
!------------------------------------------------------------------------------!
