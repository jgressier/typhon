!------------------------------------------------------------------------------!
! Procedure : calcboco_connect_per_match                   Authors : J. Gressier
!    
! Fonction
!   Computation & exchange of connection data for connection boundary conditions
!
!------------------------------------------------------------------------------!
subroutine calcboco_connect_per_match(bccon_mode, defsolver, umesh, fsend, frecv, boco)

use TYPHMAKE
use OUTPUT
use VARCOM
use COMMTAG
use MENU_SOLVER
use USTMESH
use GENFIELD

implicit none

! -- Inputs --
integer(kpp)           :: bccon_mode       ! data exchange mode for connection
type(mnu_solver)       :: defsolver        ! solver type
type(st_ustmesh)       :: umesh
type(st_ustboco)       :: boco

! -- Inputs/Outputs --
type(st_genericfield)  :: fsend, frecv     ! field to exchange (primitive or gradient)

! -- Internal variables --
integer(kip) :: nf, dim
integer(kip) :: if, ic, ideb, var
integer :: idef                      ! boundary condition definition index
real(krp), allocatable :: bocodata(:) ! array of packed data
integer(kip), dimension(:), allocatable :: isend, irecv ! index of data in fsent and frecv

! -- BODY --

if (mpi_run) call erreur("Critical error", "Periodic conditions cannot be used in parallel computations")

nf  = boco%nface
allocate(isend(nf))
allocate(irecv(nf))

select case(bccon_mode)
case(bccon_cell_state, bccon_cell_grad)
  isend(1:nf) = umesh%facecell%fils(boco%gridcon%i_param(1:nf), 1)  ! index indirection: internal cell of current BC face
  irecv(1:nf) = umesh%facecell%fils(boco%iface(1:nf), 2)            ! index indirection: ghost    cell of current BC face
case(bccon_face_state)
  isend(1:nf) = boco%gridcon%i_param(1:nf)
  irecv(1:nf) = boco%iface(1:nf)
case(bccon_face_grad)
  call error_stop("Internal error: connection mode not yet implemented")
case default
  call error_stop("Internal error: unknown connection mode")
endselect

do if = 1, boco%nface
  ic   = irecv(if)

  ! -- SCALARS --
  do var = 1, frecv%nscal
    frecv%tabscal(var)%scal(ic) = fsend%tabscal(var)%scal(isend(if)) 
  enddo

  ! -- VECTORS --  
  do var = 1, frecv%nvect
    frecv%tabvect(var)%vect(ic) = fsend%tabvect(var)%vect(isend(if))
    call transvec_per(defsolver%defmesh%periodicity(defsolver%connect(boco%gridcon%ilink)%ilink), &
                      frecv%tabvect(var)%vect(ic:ic), -boco%gridcon%rlink)
  enddo

  ! -- TENSORS --  
  do var = 1, frecv%ntens
    frecv%tabtens(var)%tens(ic) = fsend%tabtens(var)%tens(isend(if))
    call transten_per(defsolver%defmesh%periodicity(defsolver%connect(boco%gridcon%ilink)%ilink), &
                      frecv%tabtens(var)%tens(ic:ic), -boco%gridcon%rlink)
  enddo

enddo

deallocate(isend, irecv)


endsubroutine calcboco_connect_per_match

!------------------------------------------------------------------------------!
! Changes history
!
! Mar   2009 : Created
!------------------------------------------------------------------------------!
