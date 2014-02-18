!------------------------------------------------------------------------------!
! Procedure : calcboco_connect_match                   Authors : J. Gressier
!                                                      Created : October 2005
! Fonction
!   Computation & exchange of connection data for connection boundary conditions
!
!------------------------------------------------------------------------------!
subroutine calcboco_connect_match(defsolver, umesh, boco, bccon)

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
type(st_bccon)         :: bccon

! -- Internal variables --

integer(kip) :: nf, dim
integer(kip) :: if, ic, ideb, var
integer      :: idef                                     ! boundary condition definition index
real(krp), allocatable :: bocodata(:)   ! array of packed data
integer(kmpi) :: mpitag

! -- BODY --

mpitag = 1000*mpitag_field + bccon%bccon_mode

nf  = boco%nface
dim = bccon%fsend%nscal + 3*bccon%fsend%nvect + 9*bccon%fsend%ntens

allocate(bocodata(nf*dim))

! -- pack internal variables ( scal1 scal2 vec1%x vec1%y vec1%z ... )--

do if = 1, boco%nface
  ic   = bccon%isend(if)
  ideb = (if-1)*dim
  do var = 1, bccon%fsend%nscal
    bocodata(ideb+var) = bccon%fsend%tabscal(var)%scal(ic)
  enddo
  ideb = ideb+bccon%fsend%nscal
  do var = 1, bccon%fsend%nvect
    bocodata(ideb+(var-1)*3+1:ideb+var*3) = tab(bccon%fsend%tabvect(var)%vect(ic))
  enddo
  ideb = ideb+3*bccon%fsend%nvect
  do var = 1, bccon%fsend%ntens
    bocodata(ideb+(var-1)*9+1:ideb+var*9) = reshape(bccon%fsend%tabtens(var)%tens(ic)%mat, (/ 9 /))
  enddo
enddo

! -- send internal variables --

call sendtogrid(boco%gridcon%grid_id, dim*nf, bocodata, mpitag)

! -- receive boundary condition data --

call receivefromgrid(boco%gridcon%grid_id, dim*nf, bocodata, mpitag)

! -- unpack boundary condition data --

do if = 1, boco%nface
  ic   = bccon%irecv(if)
  ideb = (if-1)*dim
  do var = 1, bccon%frecv%nscal
    bccon%frecv%tabscal(var)%scal(ic) = bocodata(ideb+var)
  enddo
  ideb = ideb+bccon%frecv%nscal
  do var = 1, bccon%frecv%nvect
    bccon%frecv%tabvect(var)%vect(ic) = v3d_of(bocodata(ideb+(var-1)*3+1:ideb+var*3))
  enddo
  ideb = ideb+3*bccon%frecv%nvect
  do var = 1, bccon%frecv%ntens
    bccon%frecv%tabtens(var)%tens(ic)%mat = reshape(bocodata(ideb+(var-1)*9+1:ideb+var*9), (/ 3, 3 /))
  enddo
enddo

deallocate(bocodata)

endsubroutine calcboco_connect_match

!------------------------------------------------------------------------------!
! Changes history
!
! Oct  2005 : Created
! Feb  2007 : Comments
! May  2009 : Add tensor to pack/unpack data (necessary for gradient field)
!------------------------------------------------------------------------------!
