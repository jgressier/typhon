!------------------------------------------------------------------------------!
! Procedure : calcboco_connect_match                   Authors : J. Gressier
!                                                      Created : October 2005
! Fonction
!   Computation & exchange of connection data for connection boundary conditions
!
!------------------------------------------------------------------------------!
subroutine calcboco_connect_match(bccon_mode, defsolver, umesh, fsend, frecv, boco)

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
type(st_genericfield)  ::  fsend, frecv    ! field to exchange (primitive or gradient)

! -- Internal variables --

integer(kip) :: nf, dim
integer(kip) :: if, ic, ideb, var
integer      :: idef                                     ! boundary condition definition index
real(krp),                  allocatable :: bocodata(:)   ! array of packed data
integer(kip), dimension(:), allocatable :: isend, irecv  ! index of data in fsent and frecv
integer(kmpi) :: mpitag

! -- BODY --

mpitag = 1000*mpitag_field + bccon_mode

nf  = boco%nface
dim = fsend%nscal + 3*fsend%nvect + 9*fsend%ntens

allocate(bocodata(nf*dim))
allocate(isend(nf))
allocate(irecv(nf))

select case(bccon_mode)
case(bccon_cell_state, bccon_cell_grad)
  isend(1:nf) = umesh%facecell%fils(boco%iface(1:nf), 1)    ! index indirection: internal cell of current BC face
  irecv(1:nf) = umesh%facecell%fils(boco%iface(1:nf), 2)    ! index indirection: ghost    cell of current BC face
case(bccon_face_state)
  isend(1:nf) = boco%iface(1:nf)
  irecv(1:nf) = boco%iface(1:nf)
case(bccon_face_grad)
  call error_stop("Internal error: connection mode not yet implemented")
case default
  call error_stop("Internal error: unknown connection mode")
endselect

! -- pack internal variables ( scal1 scal2 vec1%x vec1%y vec1%z ... )--

do if = 1, boco%nface
  ic   = isend(if)
  ideb = (if-1)*dim
  do var = 1, fsend%nscal
    bocodata(ideb+var) = fsend%tabscal(var)%scal(ic)
  enddo
  ideb = ideb+fsend%nscal
  do var = 1, fsend%nvect
    bocodata(ideb+(var-1)*3+1:ideb+var*3) = tab(fsend%tabvect(var)%vect(ic))
  enddo
  ideb = ideb+3*fsend%nvect
  do var = 1, fsend%ntens
    bocodata(ideb+(var-1)*9+1:ideb+var*9) = reshape(fsend%tabtens(var)%tens(ic)%mat, (/ 9 /))
  enddo
enddo

! -- send internal variables --

call sendtogrid(boco%gridcon%grid_id, dim*nf, bocodata, mpitag)

! -- receive boundary condition data --

call receivefromgrid(boco%gridcon%grid_id, dim*nf, bocodata, mpitag)

! -- unpack boundary condition data --

do if = 1, boco%nface
  ic   = irecv(if)
  ideb = (if-1)*dim
  do var = 1, frecv%nscal
    frecv%tabscal(var)%scal(ic) = bocodata(ideb+var)
  enddo
  ideb = ideb+frecv%nscal
  do var = 1, frecv%nvect
    frecv%tabvect(var)%vect(ic) = v3d_of(bocodata(ideb+(var-1)*3+1:ideb+var*3))
  enddo
  ideb = ideb+3*frecv%nvect
  do var = 1, frecv%ntens
    frecv%tabtens(var)%tens(ic)%mat = reshape(bocodata(ideb+(var-1)*9+1:ideb+var*9), (/ 3, 3 /))
  enddo
enddo

deallocate(bocodata)
deallocate(isend, irecv)

endsubroutine calcboco_connect_match

!------------------------------------------------------------------------------!
! Changes history
!
! Oct  2005 : Created
! Feb  2007 : Comments
! May  2009 : Add tensor to pack/unpack data (necessary for gradient field)
!------------------------------------------------------------------------------!
