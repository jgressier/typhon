!------------------------------------------------------------------------------!
! Procedure : init_boco_ns                Auteur : J. Gressier
!                                         Date   : July 2004
! Fonction                                Modif  : 
!   Traitement des parametres du fichier menu principal
!   Initialisation des conditions limites
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine init_boco_ns(defsolver, grid)

use TYPHMAKE
use DEFFIELD
use MENU_NS
use USTMESH
use MENU_SOLVER
use VARCOM

implicit none

! -- Declaration des entrees --
type(st_grid)  :: grid

! -- Declaration des entrees/sorties --
type(mnu_solver)  :: defsolver

! -- Declaration des variables internes --
integer :: iboco,i

! -- Debut de la procedure --

! On parcourt toutes les conditions limites du domaine

do iboco = 1, grid%umesh%nboco 


enddo

endsubroutine init_boco_ns

!------------------------------------------------------------------------------!
! Changes history
!
! july 2004 : creation de la routine
!------------------------------------------------------------------------------!


