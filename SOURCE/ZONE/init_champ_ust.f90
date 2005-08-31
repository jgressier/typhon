!------------------------------------------------------------------------------!
! Procedure : init_champ_ust              Auteur : J. Gressier
!                                         Date   : Mars 2003
! Fonction                                Modif  : see history
!   Initialization of fields according to the solver
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
!subroutine init_champ_ust(defsolver, ust_mesh, champ, grid)
subroutine init_champ_ust(defsolver, ust_mesh, grid)

use TYPHMAKE
use VARCOM
use OUTPUT
use USTMESH
use DEFFIELD
use MENU_SOLVER

implicit none

! -- Declaration des entrees --
type(mnu_solver) :: defsolver            ! parametres du solveur
type(st_ustmesh) :: ust_mesh             ! maillage et connectivites
type(st_grid)    :: grid                 ! grille

! -- Declaration des sorties --
!type(st_field), pointer :: champ                ! champ d'etat et de gradients

! -- Declaration des variables internes --
integer :: i
type(st_field), pointer :: champ

! -- Debut de la procedure --

call print_info(8, ". initialisation et allocation des champs")

! allocation des champs

select case(defsolver%typ_solver)
case(solNS)
  champ=>newfield(grid, 2, 1, ust_mesh%ncell, ust_mesh%nface)
case(solKDIF)
  champ=>newfield(grid, 1, 0, ust_mesh%ncell, ust_mesh%nface) 
case(solVORTEX)
  champ=>newfield(grid, 1, 0, ust_mesh%ncell, ust_mesh%nface)
case default
  call erreur("Incoherence interne (init_champ_ust)","type de solveur inconnu")
endselect 

call alloc_prim(champ)

! Boucle sur les definitions de champ

do i = 1, defsolver%ninit

  write(str_w,'(a,i3)') "    initialisation n°",i
  call print_info(10, str_w)

  ! initialisation selon solveur

  select case(defsolver%typ_solver)
  case(solNS)
    call init_ns_ust(defsolver%defns, defsolver%init(i)%ns, champ, ust_mesh%mesh)
  case(solKDIF)
    call init_kdif_ust(defsolver%init(i)%kdif, champ, defsolver%init(i)%unif, ust_mesh%mesh)
  case(solVORTEX)
    call init_vort_ust(defsolver%init(i)%vortex, champ)
  case default
    call erreur("Incoherence interne (init_champ_ust)","type de solveur inconnu")
  endselect 

enddo

select case(defsolver%typ_solver)
case(solNS, solKDIF)
  call calc_varcons(defsolver, champ)
case(solVORTEX)
  ! nothing to do
case default
  call erreur("Incoherence interne (init_champ_ust)","type de solveur inconnu")
endselect

grid%field => champ

endsubroutine init_champ_ust

!------------------------------------------------------------------------------!
! Modification history
!
! mars 2003 : creation de la procedure
! juin 2003 : mise a jour 
! mars 2004 : ajouts specifiques au solveur VORTEX
! july 2004 : initialization of NS fields
! oct  2004 : field chained list
!------------------------------------------------------------------------------!
