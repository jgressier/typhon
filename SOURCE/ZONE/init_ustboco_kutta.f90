!------------------------------------------------------------------------------!
! Procedure : init_ustboco_kutta          Auteur : J. Gressier
!                                         Date   : Mars 2004
! Fonction                                Modif  : (cf historique)
!   Affectation des connectivites entre faces limites et cellules limites
!   pour le type "kutta" (point fictif sur la face)
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine init_ustboco_kutta(ib, defboco, grid)

use TYPHMAKE
!use VARCOM
use OUTPUT
use USTMESH
use MENU_BOCO

implicit none

! -- Declaration des entrees --
integer        :: ib                     ! numero de condition aux limites
type(mnu_boco) :: defboco                ! parametres du solveur

! -- Declaration des entrees/sorties --
type(st_grid), target  :: grid                   ! maillage et connectivites

! -- Declaration des sorties --

! -- Declaration des variables internes --
integer :: if                            ! indice boco et de face
integer :: icell, iface                  ! index de cellule et de face

! -- Debut de la procedure --

! lien de la condition limite (defsolver) vers grid

defboco%boco_vortex%pgrid => grid



endsubroutine init_ustboco_kutta

!------------------------------------------------------------------------------!
! Historique des modifications
!
! mars 2003 : creation de la procedure
!------------------------------------------------------------------------------!
