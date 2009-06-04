!------------------------------------------------------------------------------!
! Procedure : calcboco_connect_match                   Authors : J. Gressier
!                                                      Created : October 2005
! Fonction
!   Computation & exchange of connection data for connection boundary conditions
!
!------------------------------------------------------------------------------!
subroutine calcboco_connect_match(bccon_mode, defsolver, umesh, field, boco)

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
type(st_genericfield)  :: field            ! field to exchange (primitive or gradient)

! -- Internal variables --
integer :: if, ic, dim, ideb, var
integer :: idef                      ! boundary condition definition index
real(krp), allocatable :: bocodata(:) ! array of packed data
integer(kmpi) :: mpitag

! -- BODY --

mpitag = 1000*mpitag_field + bccon_mode

dim = field%nscal + 3*field%nvect + 9*field%ntens

allocate(bocodata(boco%nface*dim))

! -- pack internal variables ( scal1 scal2 vec1%x vec1%y vec1%z ... )--

do if = 1, boco%nface
  ic   = umesh%facecell%fils(boco%iface(if), 1)      ! internal cell of limit face
  ideb = (if-1)*dim
  do var = 1, field%nscal
    bocodata(ideb+var) = field%tabscal(var)%scal(ic)
  enddo
  ideb = ideb+field%nscal
  do var = 1, field%nvect
    bocodata(ideb+(var-1)*3+1:ideb+var*3) = tab(field%tabvect(var)%vect(ic))
  enddo
  ideb = ideb+3*field%nvect
  do var = 1, field%ntens
    bocodata(ideb+(var-1)*9+1:ideb+var*9) = reshape(field%tabtens(var)%tens(ic)%mat, (/ 9 /))
  enddo
enddo

! -- send internal variables --

call sendtogrid(boco%gridcon%grid_id, dim*boco%nface, bocodata, mpitag)

! -- receive boundary condition data --

call receivefromgrid(boco%gridcon%grid_id, dim*boco%nface, bocodata, mpitag)

! -- unpack boundary condition data --

do if = 1, boco%nface
  ic   = umesh%facecell%fils(boco%iface(if), 2)     ! ghost cell
  ideb = (if-1)*dim
  do var = 1, field%nscal
    field%tabscal(var)%scal(ic) = bocodata(ideb+var)
  enddo
  ideb = ideb+field%nscal
  do var = 1, field%nvect
    field%tabvect(var)%vect(ic) = v3d_of(bocodata(ideb+(var-1)*3+1:ideb+var*3))
  enddo
  ideb = ideb+3*field%nvect
  do var = 1, field%ntens
    field%tabtens(var)%tens(ic)%mat = reshape(bocodata(ideb+(var-1)*9+1:ideb+var*9), (/ 3, 3 /))
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
