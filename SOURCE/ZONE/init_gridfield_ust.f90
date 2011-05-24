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
use TYPHON_FMT
use TYFMT_SOL

implicit none

include 'cgnslib_f.h'

! -- INPUTS --
type(mnu_solver) :: defsolver            ! parametres du solveur
type(st_ustmesh) :: umesh             ! maillage et connectivites

! -- INPUTS/OUTPUTS --
type(st_grid)    :: grid               ! grid and its field

! -- Internal variables --
integer                 :: i, ier, isca, ivec, ic, iunit
type(st_field), pointer :: field
type(st_deftyphon)      :: deftyphon
type(st_ustmesh)        :: p_umesh        ! 

! -- BODY --

call print_info(8, ". initializing and allocating fields")

! allocation des fields

select case(defsolver%typ_solver)
case(solNS, solKDIF)
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

    call print_info(5,"  > CGNS file initialization: "//trim(defsolver%defmesh%filename))
    !! iunit = getnew_io_unit() ! defined by cg_open_f
    call cg_open_f(trim(defsolver%defmesh%filename), MODE_READ, iunit, ier)
    if (ier /= 0) call erreur("Fatal CGNS IO", "cannot open "//trim(defsolver%defmesh%filename))
    call readcgns_sol(iunit, defsolver%defmesh%icgnsbase, defsolver%defmesh%icgnszone, &
                      umesh, field%etatprim) 
    call cg_close_f(iunit, ier)
    if (ier /= 0) call error_stop("Fatal CGNS IO: cannot close "//trim(defsolver%defmesh%filename))

  case(init_typhon)

    call print_info(5,"  > TYPHON solution initialization: "//trim(defsolver%defmesh%filename))
    iunit = getnew_io_unit() 
    call typhon_openread(iunit, trim(defsolver%defmesh%filename), deftyphon)

    call typhonread_ustmesh(deftyphon, p_umesh) !! DEV: must SKIP reading
    !call delete_ustmesh_subelements(umesh)
    call delete_ustmesh(p_umesh)

    if (deftyphon%nb_sol >= 1) then
      call typhonread_sol(deftyphon, umesh, field%etatprim)
    else
      call error_stop("Initialization: missing solution in TYPHON input file")
    endif

    call close_io_unit(iunit)

  case(init_def) 
     
    call print_info(5,"  > USER defined initialization")
    select case(defsolver%typ_solver)
    case(solNS)
      call init_ns_ust(defsolver%defns, defsolver%defmrf, defsolver%init(i), field, umesh)

    case(solKDIF)
      call init_kdif_ust(defsolver%init(i), field, umesh)

    case default
      call error_stop("Internal error (init_gridfield_ust): unknown solver type")
    endselect

  case default
    call error_stop("Internal error (init_gridfield_ust): unknown initialization option")
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
