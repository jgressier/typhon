!------------------------------------------------------------------------------!
! Procedure : init_gridfield_ust              Auteur : J. Gressier
!
! Fonction 
!   Initialization of fields according to the solver
!
!------------------------------------------------------------------------------!
subroutine init_gridfield_ust(defsolver, umesh, grid)

use TYPHMAKE
use VARCOM
use OUTPUT
use USTMESH
use DEFFIELD
use MENU_SOLVER

implicit none

include 'cgnslib_f.h'

! -- INPUTS --
type(mnu_solver) :: defsolver            ! parametres du solveur
type(st_ustmesh) :: umesh             ! maillage et connectivites

! -- INPUTS/OUTPUTS --
type(st_grid)    :: grid               ! grid and its field

! -- Internal variables --
integer                 :: i, cgnsunit, ier, isca, ivec
type(st_field), pointer :: field

! -- BODY --

call print_info(8, ". initializing and allocating fields")

! allocation des fields

select case(defsolver%typ_solver)
case(solNS)
  field=>newfield(grid, defsolver%nsca, defsolver%nvec, umesh%ncell, umesh%nface)
case(solKDIF)
  field=>newfield(grid, defsolver%nsca, defsolver%nvec, umesh%ncell, umesh%nface) 
case default
  call error_stop("Internal error (init_gridfield_ust): unknown solver type")
endselect 

call alloc_prim(field)

! -- copy defsolver quantity ids to PRIMITIVE field --

do isca = 1, field%etatprim%nscal
  field%etatprim%tabscal(isca)%quantity_id = defsolver%idsca(isca)
enddo

do ivec = 1, field%etatprim%nvect
  field%etatprim%tabvect(ivec)%quantity_id = defsolver%idvec(ivec)
enddo

! Boucle sur les definitions de field

do i = 1, defsolver%ninit

  write(str_w,'(a,i3)') "  initialization #",i
  call print_info(10, str_w)

  ! initialisation selon solveur

  select case(defsolver%init(i)%type)

  case(init_cgns)

     call print_info(5,"  > CGNS file initialization")
     call cg_open_f(trim(defsolver%defmesh%filename), MODE_READ, cgnsunit, ier)
     if (ier /= 0) call erreur("Fatal CGNS IO", "cannot open "//trim(defsolver%defmesh%filename))
     call readcgns_sol(cgnsunit, defsolver%defmesh%icgnsbase, defsolver%defmesh%icgnszone, &
                       umesh, field%etatprim) 
     call cg_close_f(cgnsunit, ier)
     if (ier /= 0) call error_stop("Fatal CGNS IO: cannot close "//trim(defsolver%defmesh%filename))

   case default
     
      select case(defsolver%typ_solver)
      case(solNS)
         call init_ns_ust(defsolver%defns, defsolver%init(i)%ns, field, &
              umesh, defsolver%init(i)%type, defsolver%init(i)%file)
      case(solKDIF)
         call init_kdif_ust(defsolver%init(i)%kdif, field, defsolver%init(i)%unif, &
              umesh, defsolver%init(i)%type, defsolver%init(i)%file)
      case default
         call error_stop("Internal error (init_gridfield_ust): unknown solver type")
      endselect

   endselect

enddo

select case(defsolver%typ_solver)
case(solNS, solKDIF)
  call calc_varcons(defsolver, field)
case default
  call error_stop("Internal error (init_gridfield_ust): unknown solver type")
endselect

! --

grid%field => field

endsubroutine init_gridfield_ust

!------------------------------------------------------------------------------!
! Modification history
!
! mars 2003 : creation de la procedure
! juin 2003 : mise a jour 
! mars 2004 : ajouts specifiques au solveur VORTEX
! july 2004 : initialization of NS fields
! oct  2004 : field chained list
! Fev  2007 : English translation
! May  2009 : init_champ_ust changed to init_gridfield_ust
!------------------------------------------------------------------------------!
