!------------------------------------------------------------------------------!
! Procedure : init_ustboco_singpanel      Auteur : J. Gressier
!                                         Date   : Mars 2004
! Fonction                                Modif  : (cf historique)
!   Affectation des connectivités entre faces limites et cellules limites
!   pour le type "singpanel" (point fictif sur la face) 
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine init_ustboco_singpanel(ib, defboco, grid)

use TYPHMAKE
!use VARCOM
use OUTPUT
use USTMESH
use MENU_BOCO

implicit none

! -- Declaration des entrées --
integer        :: ib                     ! numéro de condition aux limites
type(mnu_boco) :: defboco                ! paramètres du solveur

! -- Declaration des entrées/sorties --
type(st_grid), target :: grid            ! maillage et connectivités

! -- Declaration des sorties --

! -- Declaration des variables internes --
integer :: if                            ! indice boco et de face
integer :: icell, iface                  ! index de cellule et de face

! -- Debut de la procedure --


! allocation du champ

!print*,'debug : init_singpanel : ',grid%umesh%nface,' faces'
!call new(grid%field, 1, 0, grid%umesh%nface+1, 0)
!print*,'debug field scalaires : ',grid%field%nscal,'x',grid%field%ncell

! lien de la condition limite (defsolver) vers grid

defboco%boco_vortex%pgrid => grid


endsubroutine init_ustboco_singpanel

!------------------------------------------------------------------------------!
! Historique des modifications
!
! mars 2004 : création de la procédure
!------------------------------------------------------------------------------!
