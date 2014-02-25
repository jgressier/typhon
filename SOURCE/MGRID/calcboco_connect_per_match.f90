!------------------------------------------------------------------------------!
! Procedure : calcboco_connect_per_match                   Authors : J. Gressier
!    
! Fonction
!   Computation & exchange of connection data for connection boundary conditions
!
!------------------------------------------------------------------------------!
subroutine calcboco_connect_per_match(imode, defsolver, umesh, boco, bccon)

use TYPHMAKE
use OUTPUT
use VARCOM
use COMMTAG
use MENU_SOLVER
use USTMESH
use GENFIELD
use GRID_CONNECT

implicit none

! -- Inputs --
integer(kpp)           :: imode
type(mnu_solver)       :: defsolver        ! solver type
type(st_ustmesh)       :: umesh
type(st_ustboco)       :: boco

! -- Inputs/Outputs --
type(st_bccon)         :: bccon

! -- Internal variables --
integer(kip) :: nf, dim
integer(kip) :: if, ic, is, ideb, var
integer :: idef                      ! boundary condition definition index
real(krp), allocatable :: bocodata(:) ! array of packed data

! -- BODY --

if (mpi_run) call error_stop("Critical error: periodic conditions cannot be used in parallel computations")

nf  = boco%nface

allocate(bccon%isend(nf))
allocate(bccon%irecv(nf))

select case(bccon%bccon_mode)
case(bccon_cell_state, bccon_cell_grad)
  bccon%isend(1:nf) = umesh%facecell%fils(boco%gridcon%i_param(1:nf), 1)    ! index indirection: internal cell of current BC face
  bccon%irecv(1:nf) = umesh%facecell%fils(boco%iface(1:nf), 2)              ! index indirection: ghost    cell of current BC face
case(bccon_face_state)
  bccon%isend(1:nf) = boco%gridcon%i_param(1:nf)
  bccon%irecv(1:nf) = boco%iface(1:nf)
case(bccon_face_grad)
  call error_stop("Internal error: connection mode not yet implemented")
case default
  call error_stop("Internal error: unknown connection mode")
endselect

select case(imode)
case(igcon_send)

do if = 1, boco%nface
  ic = bccon%irecv(if)
  is = bccon%isend(if)
  
  ! -- SCALARS --
  do var = 1, bccon%frecv%nscal
    bccon%frecv%tabscal(var)%scal(ic) = bccon%fsend%tabscal(var)%scal(is) 
  enddo

  ! -- VECTORS --  
  do var = 1, bccon%frecv%nvect
    bccon%frecv%tabvect(var)%vect(ic) = bccon%fsend%tabvect(var)%vect(is)
    call transvec_per(defsolver%defmesh%periodicity(defsolver%connect(boco%gridcon%ilink)%ilink), &
                      bccon%frecv%tabvect(var)%vect(ic:ic), -boco%gridcon%rlink)
  enddo

  ! -- TENSORS --  
  do var = 1, bccon%frecv%ntens
    bccon%frecv%tabtens(var)%tens(ic) = bccon%fsend%tabtens(var)%tens(is)
    call transten_per(defsolver%defmesh%periodicity(defsolver%connect(boco%gridcon%ilink)%ilink), &
                      bccon%frecv%tabtens(var)%tens(ic:ic), -boco%gridcon%rlink)
  enddo

enddo

case(igcon_recv)
case default
  call error_stop("Internal error: unknown connection mode")
endselect

endsubroutine calcboco_connect_per_match
!------------------------------------------------------------------------------!
! Changes history
!
! Mar   2009 : Created
!------------------------------------------------------------------------------!
