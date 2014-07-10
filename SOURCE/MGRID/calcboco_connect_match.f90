!------------------------------------------------------------------------------!
! Procedure : calcboco_connect_match                   Authors : J. Gressier
!                                                      Created : October 2005
! Fonction
!   Computation & exchange of connection data for connection boundary conditions
!
!------------------------------------------------------------------------------!
subroutine calcboco_connect_match(imode, defsolver, umesh, boco, bccon)

use TYPHMAKE
use OUTPUT
use VARCOM
use COMMTAG
use MENU_SOLVER
use USTMESH
use GENFIELD
use MPICOMM

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
integer(kip) :: if, ic, ideb, var
integer      :: idef                                     ! boundary condition definition index
integer(kmpi) :: mpitag
integer      :: nsim            ! Number of simulations
integer      :: isim 
! -- BODY --
nsim = defsolver%nsim
nf  = boco%nface
dim = (bccon%fsend%nscal + 3*bccon%fsend%nvect + 9*bccon%fsend%ntens)*nsim

allocate(bccon%isend(nf))
allocate(bccon%irecv(nf))

select case(bccon%bccon_mode)
case(bccon_cell_state, bccon_cell_grad)
  bccon%isend(1:nf) = umesh%facecell%fils(boco%iface(1:nf), 1) ! index indirection: internal cell of current BC face
  bccon%irecv(1:nf) = umesh%facecell%fils(boco%iface(1:nf), 2) ! index indirection: ghost cell of current BC face
case(bccon_face_state)
  bccon%isend(1:nf) = boco%iface(1:nf)
  bccon%irecv(1:nf) = boco%iface(1:nf)
case(bccon_face_grad)
  call error_stop("Internal error: connection mode not yet implemented")
case default
  call error_stop("Internal error: unknown connection mode")
endselect 

select case(imode)
case(igcon_send) ! ----- SEND data ---------------------------------------------

boco%gridcon%nsend = nf*dim
allocate(boco%gridcon%rsend(boco%gridcon%nsend))

! -- pack internal variables ( scal1 scal2 vec1%x vec1%y vec1%z ... )--

do isim = 1, nsim 
  do if = 1, boco%nface
    ic   = bccon%isend(if)
    ideb = (if-1)*dim
    do var = 1, bccon%fsend%nscal
      boco%gridcon%rsend(nsim*(var-1)+isim+ideb) = bccon%fsend%tabscal(var)%scal(ic)
    enddo
    ideb = ideb+bccon%fsend%nscal*nsim
    do var = 1, bccon%fsend%nvect
      boco%gridcon%rsend(ideb +(nsim*(var-1)+isim-1)*3+1:ideb+(nsim*(var-1)+isim)*3) = tab(bccon%fsend%tabvect(var)%vect(ic))
    enddo
    ideb = ideb+3*bccon%fsend%nvect*nsim
    do var = 1, bccon%fsend%ntens
      boco%gridcon%rsend(ideb+(nsim*(var-1)+isim-1)*9+1:ideb+(nsim*(var-1)+isim)*9) = reshape(bccon%fsend%tabtens(var)%tens(ic)%mat, (/ 9 /))
    enddo
  enddo
enddo ! end simulation loop
mpitag = mpitag_field !bccon%bccon_mode*100 + myprocid*10 + request_id()

! -- send internal variables --

call sendtogrid(boco%gridcon%grid_id, boco%gridcon%nsend, boco%gridcon%rsend, mpitag)
!print*,'send ',myprocid, boco%gridcon%grid_id, mpitag, ':', boco%gridcon%nsend

! -- request receipt boundary condition data --

!call waitall_mpirequest()

boco%gridcon%nrecv = nf*dim
allocate(boco%gridcon%rrecv(boco%gridcon%nrecv))

call receivefromgrid(boco%gridcon%grid_id, boco%gridcon%nrecv, boco%gridcon%rrecv, mpitag)

!call waitall_mpirequest()

case(igcon_recv)  ! ----- RECEIVE data ---------------------------------------------

! -- unpack boundary condition data - receipt requested between send and receive 
do isim = 1, nsim
  do if = 1, boco%nface
    ic   = bccon%irecv(if)
    ideb = (if-1)*dim
    do var = 1, bccon%frecv%nscal
      bccon%frecv%tabscal(var)%scal(ic) = boco%gridcon%rrecv(ideb+nsim*(var-1)+isim)
    enddo
    ideb = ideb+bccon%frecv%nscal*nsim
    do var = 1, bccon%frecv%nvect
      bccon%frecv%tabvect(var)%vect(ic) = v3d_of(boco%gridcon%rrecv(ideb+(nsim*(var-1)+isim-1)*3+1:ideb+(nsim*(var-1)+isim)*3))
    enddo
    ideb = ideb+3*bccon%frecv%nvect*nsim
    do var = 1, bccon%frecv%ntens
      bccon%frecv%tabtens(var)%tens(ic)%mat = reshape(boco%gridcon%rrecv(ideb+(nsim*(var-1)+isim-1)*9+1:ideb+(nsim*(var-1)+isim)*9), (/ 3, 3 /))
    enddo
  enddo
enddo ! end loop simulation

deallocate(boco%gridcon%rsend)
deallocate(boco%gridcon%rrecv)

case default
  call error_stop("Internal error: unknown connection mode")
endselect

endsubroutine calcboco_connect_match

!------------------------------------------------------------------------------!
! Changes history
!
! Oct  2005 : Created
! Feb  2007 : Comments
! May  2009 : Add tensor to pack/unpack data (necessary for gradient field)
!------------------------------------------------------------------------------!
