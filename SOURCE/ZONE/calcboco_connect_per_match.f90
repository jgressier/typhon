!------------------------------------------------------------------------------!
! Procedure : calcboco_connect_per_match                   Authors : J. Gressier
!    
! Fonction
!   Computation & exchange of connection data for connection boundary conditions
!
!------------------------------------------------------------------------------!
subroutine calcboco_connect_per_match(defsolver, umesh, prim, boco)

use TYPHMAKE
use OUTPUT
use VARCOM
use COMMTAG
use MENU_SOLVER
use USTMESH
use GENFIELD

implicit none

! -- Inputs --
type(mnu_solver)       :: defsolver        ! solver type
type(st_ustmesh)       :: umesh
type(st_ustboco)       :: boco

! -- Inputs/Outputs --
type(st_genericfield)  :: prim             ! primitive variables fields

! -- Internal variables --
integer :: if, ic, ic2, var
integer :: idef                      ! boundary condition definition index
real(krp), allocatable :: bocodata(:) ! array of packed data

! -- BODY --

if (mpi_run) call erreur("Critical error", "Periodic conditions cannot be used in parallel computations")

do if = 1, boco%nface
  ic   = umesh%facecell%fils(boco%iface(if), 2)     ! ghost cell
  ic2  = boco%gridcon%i_param(if)                   ! periodic cell equivalent to ghostcell
  do var = 1, prim%nscal
    prim%tabscal(var)%scal(ic) = prim%tabscal(var)%scal(ic2) 
  enddo
  do var = 1, prim%nvect
    prim%tabvect(var)%vect(ic) = prim%tabvect(var)%vect(ic2)
    call transvec_per(defsolver%defmesh%periodicity(defsolver%connect(boco%gridcon%ilink)%ilink), &
                      prim%tabvect(var)%vect(ic:ic), -boco%gridcon%rlink)
  enddo
enddo


endsubroutine calcboco_connect_per_match

!------------------------------------------------------------------------------!
! Changes history
!
! Mar   2009 : Created
!------------------------------------------------------------------------------!
