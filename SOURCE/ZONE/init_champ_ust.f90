!------------------------------------------------------------------------------!
! Procedure : init_champ_ust              Auteur : J. Gressier
!                                         Date   : Mars 2003
! Fonction                                Modif  : Juin 2003
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine init_champ_ust(defsolver, ust_mesh, champ)

use TYPHMAKE
use VARCOM
use OUTPUT
use USTMESH
use DEFFIELD
use MENU_SOLVER

implicit none

! -- Declaration des entrées --
type(mnu_solver) :: defsolver            ! paramètres du solveur
type(st_ustmesh) :: ust_mesh             ! maillage et connectivités

! -- Declaration des sorties --
type(st_field)   :: champ                ! champ d'état et de gradients

! -- Declaration des variables internes --
integer :: i

! -- Debut de la procedure --

call print_info(8, ". initialisation des champs")

! allocation des champs

call new(champ, 1, 0, ust_mesh%ncell, ust_mesh%nface)
call alloc_prim(champ)

! Boucle sur les définitions de champ

do i = 1, defsolver%ninit

  write(str_w,'(a,i3)') "    initialisation n°",i
  call print_info(10, str_w)

  ! initialisation selon solveur

  select case(defsolver%typ_solver)
  case(solKDIF)
    call init_kdif_ust(defsolver%init(i)%kdif, champ, defsolver%init(i)%unif, ust_mesh%mesh)
  case(solVORTEX)
    call init_vort_ust(defsolver%init(i)%vortex, champ)
  case default
    call erreur("Incohérence interne (init_champ_ust)","type de solveur inconnu")
  endselect 

enddo

select case(defsolver%typ_solver)
case(solKDIF)
  call calc_varcons(defsolver, champ)
endselect

endsubroutine init_champ_ust

!------------------------------------------------------------------------------!
! Historique des modifications
!
! mars 2003 : création de la procédure
! juin 2003 : mise à jour 
! mars 2004 : ajouts spécifiques au solveur VORTEX
!------------------------------------------------------------------------------!
