!------------------------------------------------------------------------------!
! MODULE : GENFIELD                             Authors : J. Gressier
!
! Function
!   Library of generic fields operations for all solvers
!
! Defaults/Limitations/Misc :
!
!------------------------------------------------------------------------------!

module GENFIELD

use MESHPREC     ! Precision and string size
use IOCFD
use TENSOR3      !
use BASEFIELD

implicit none

! -- Module global variables ------------------------------------------------

integer(kpp), parameter :: sol_undefined  = 1
integer(kpp), parameter :: sol_cell       = 2
integer(kpp), parameter :: sol_node       = 5
integer(kpp), parameter :: sol_svm2quad   = 20
integer(kpp), parameter :: sol_svm3wang   = 30
integer(kpp), parameter :: sol_svm3kris   = 31
integer(kpp), parameter :: sol_svm3kris2  = 32
integer(kpp), parameter :: sol_svm4wang   = 40
integer(kpp), parameter :: sol_svm4kris   = 41
integer(kpp), parameter :: sol_svm4kris2  = 42

! -- DECLARATIONS -----------------------------------------------------------

!------------------------------------------------------------------------------!
! ST_GENERICFIELD structure definition : generic physical field
!------------------------------------------------------------------------------!

type st_genericfield
  integer      :: nscal, nvect, ntens        ! field dimensions
  integer      :: dim                        ! number of values
  integer      :: ncell, dof                 ! number of cells and degree of freedom per cell
  integer(kpp) :: type                       ! type of solution
  type(st_genericfield),           pointer :: next      ! chained list pointer
  type(st_scafield), dimension(:), pointer :: tabscal   ! scalar fields
  type(st_vecfield), dimension(:), pointer :: tabvect   ! vector fields
  type(st_tenfield), dimension(:), pointer :: tabtens   ! tensor fields
endtype st_genericfield


! -- INTERFACES -------------------------------------------------------------

interface new
  module procedure new_genericfield, new_genericfield_st
endinterface

interface delete
  module procedure delete_genericfield
endinterface

interface insert
  module procedure insert_newgfield
endinterface

interface size_elem
  module procedure size_elem_gfield
endinterface

interface size_tot
  module procedure size_tot_gfield
endinterface

interface packst
  module procedure pack_gfield
endinterface

interface unpackst
  module procedure unpack_gfield
endinterface

interface transfer
  module procedure gfield_transfer, gfield_transfer_t
endinterface

interface scale
  module procedure gfield_scale, gfield_scale_t
endinterface

interface xeqxpy
  module procedure gfield_xeqxpy
endinterface

interface xeqxpay
  module procedure gfield_xeqxpay, gfield_xeqxpay_t
endinterface

interface l1norm
  module procedure gfield_l1norm
endinterface

interface l2sqnorm
  module procedure gfield_l2sqnorm, gfield_l2sqnorm_t
endinterface

interface l2norm
  module procedure gfield_l2norm, gfield_l2norm_t
endinterface

interface l1wnorm
  module procedure gfield_l1wnorm
endinterface

interface l2wnorm
  module procedure gfield_l2wnorm
endinterface

interface dot_prod
  module procedure gfield_dot_prod, gfield_dot_prod_t
endinterface

interface wdot_prod
  module procedure gfield_wdot_prod
endinterface

! -- Functions and Operators ------------------------------------------------


! -- IMPLEMENTATION ---------------------------------------------------------
contains


!------------------------------------------------------------------------------!
! Procedure : ST_GENERICFIELD structure allocation
!------------------------------------------------------------------------------!
subroutine new_genericfield(gfield, dim, n_scal, n_vect, n_tens)
implicit none
type(st_genericfield) :: gfield                  ! generic field
integer               :: dim                     ! cell number
integer               :: n_scal, n_vect, n_tens  ! numbers of fields
integer               :: i

  gfield%dim       = dim
  gfield%nscal     = n_scal
  gfield%nvect     = n_vect
  gfield%ntens     = n_tens
  gfield%type      = sol_undefined

  nullify(gfield%next)

  if (gfield%nscal > 0) then
    allocate(gfield%tabscal(n_scal))          ! scalar field array allocation
    do i = 1, n_scal
      call new(gfield%tabscal(i), gfield%dim)  ! field by field allocation
    enddo
  endif

  if (gfield%nvect > 0) then
    allocate(gfield%tabvect(n_vect))          ! vector field array allocation
    do i = 1, n_vect
      call new(gfield%tabvect(i), gfield%dim)  ! field by field allocation
    enddo
  endif

  if (gfield%ntens > 0) then
    allocate(gfield%tabtens(n_tens))          ! tensor field array allocation
    do i = 1, n_tens
      call new(gfield%tabtens(i), gfield%dim)  ! field by field allocation
    enddo
  endif

endsubroutine new_genericfield


!------------------------------------------------------------------------------!
! Procedure : ST_GENERICFIELD structure allocation from another structure
!------------------------------------------------------------------------------!
subroutine new_genericfield_st(newfield, oldfield)
implicit none
type(st_genericfield) :: newfield, oldfield     ! generic fields (new and original)

  call new(newfield, oldfield%dim, oldfield%nscal, oldfield%nvect, oldfield%ntens)

endsubroutine new_genericfield_st


!------------------------------------------------------------------------------!
! Procedure : ST_GENERICFIELD structure initialisation
!------------------------------------------------------------------------------!
subroutine init_genericfield(gfield, scal, vect)
implicit none
type(st_genericfield) :: gfield     ! generic field
real(krp)             :: scal       ! initialisation scalar
type(v3d)             :: vect       ! initialisation vector
integer               :: i

  do i = 1, gfield%nscal
    gfield%tabscal(i)%scal(:) = scal
  enddo

  do i = 1, gfield%nvect
    gfield%tabvect(i)%vect(:) = vect
  enddo

  do i = 1, gfield%ntens
    gfield%tabtens(i)%tens(:) = t3d(0._krp)
  enddo

endsubroutine init_genericfield


!------------------------------------------------------------------------------!
! Procedure : ST_GENERICFIELD structure deallocation
!------------------------------------------------------------------------------!
subroutine delete_genericfield(gfield)
implicit none
type(st_genericfield) :: gfield
integer               :: i

  if (gfield%nscal > 0) then
    do i = 1, gfield%nscal
      call delete(gfield%tabscal(i))
    enddo
    deallocate(gfield%tabscal)
  endif

  if (gfield%nvect > 0) then
    do i = 1, gfield%nvect
      call delete(gfield%tabvect(i))
    enddo
    deallocate(gfield%tabvect)
  endif

  if (gfield%ntens > 0) then
    do i = 1, gfield%ntens
      call delete(gfield%tabtens(i))
    enddo
    deallocate(gfield%tabtens)
  endif

endsubroutine delete_genericfield


!------------------------------------------------------------------------------!
! Procedure : generic field creation and chained link
!------------------------------------------------------------------------------!
function insert_newgfield(gfield,dim,nscal,nvect,ntens) result(pgfield)
implicit none
type(st_genericfield), pointer :: pgfield
type(st_genericfield), pointer :: gfield
integer                        :: dim,nscal,nvect,ntens

  allocate(pgfield)
  call new(pgfield,dim,nscal,nvect,ntens)
  pgfield%next => gfield

endfunction insert_newgfield


!------------------------------------------------------------------------------!
! Procedure : generic field chained list deallocation
!------------------------------------------------------------------------------!
subroutine delete_chainedgfield(gfield)
implicit none
type(st_genericfield), pointer :: gfield
type(st_genericfield), pointer :: pgfield, dgfield

  pgfield => gfield
  do while(associated(pgfield))
    dgfield => pgfield
    pgfield => pgfield%next
    call delete(dgfield)
  enddo

endsubroutine delete_chainedgfield


!------------------------------------------------------------------------------!
! Procedure : generic field conversion into real array
!------------------------------------------------------------------------------!
subroutine pack_gfield(gfield, tab, n)
implicit none
type(st_genericfield) :: gfield
real(krp)             :: tab(n)
integer(kip)          :: n
integer(kip) :: iv, i, sze, dim, tot, ideb

  sze = size_elem(gfield)
  dim = gfield%dim
  tot = sze*dim

  if (tot /= size(tab)) &
    call cfd_error("non consistent sizes of generic field of real array (pack_gfield)")

  do iv = 1, gfield%nscal
    tab(iv:tot:sze) = gfield%tabscal(iv)%scal(1:dim)
  enddo
  do iv = 1, gfield%nvect
    ideb = gfield%nscal+3*(iv-1)+1
    do i = 1, dim
      tab(ideb  +(i-1)*sze) = gfield%tabvect(iv)%vect(i)%x
      tab(ideb+1+(i-1)*sze) = gfield%tabvect(iv)%vect(i)%y
      tab(ideb+2+(i-1)*sze) = gfield%tabvect(iv)%vect(i)%z
    enddo
  enddo

  if (gfield%ntens /= 0) &
    call cfd_error("[DEV] not yet supposed to pack generic fields with tensor fields (pack_gfield)")

endsubroutine pack_gfield


!------------------------------------------------------------------------------!
! Procedure : real array conversion into generic field
!------------------------------------------------------------------------------!
subroutine unpack_gfield(tab, gfield, n)
implicit none
type(st_genericfield) :: gfield
real(krp)             :: tab(n)
integer(kip)          :: n
integer(kip) :: iv, i, sze, dim, tot, ideb

  sze = size_elem(gfield)
  dim = gfield%dim
  tot = sze*dim

  if (tot /= size(tab)) &
    call cfd_error("non consistent sizes of generic field of real array (unpack_gfield)")

  do iv = 1, gfield%nscal
    gfield%tabscal(iv)%scal(1:dim) = tab(iv:tot:sze)
  enddo
  do iv = 1, gfield%nvect
    ideb = gfield%nscal+3*(iv-1)+1
    do i = 1, dim
      gfield%tabvect(iv)%vect(i)%x = tab(ideb  +(i-1)*sze)
      gfield%tabvect(iv)%vect(i)%y = tab(ideb+1+(i-1)*sze)
      gfield%tabvect(iv)%vect(i)%z = tab(ideb+2+(i-1)*sze)
    enddo
  enddo

  if (gfield%ntens /= 0) &
    call cfd_error("[DEV] not yet supposed to pack generic fields with tensor fields (unpack_gfield)")

endsubroutine unpack_gfield


!------------------------------------------------------------------------------!
! Function : compute size of an element of generic field
!------------------------------------------------------------------------------!
integer(kip) function size_elem_gfield(gfield)
implicit none
type(st_genericfield) :: gfield

  size_elem_gfield = gfield%nscal + 3*gfield%nvect + 9*gfield%ntens

end function size_elem_gfield

!------------------------------------------------------------------------------!
! Function : compute size of the full generic field
!------------------------------------------------------------------------------!
integer(kip) function size_tot_gfield(gfield)
implicit none
type(st_genericfield) :: gfield

  size_tot_gfield = (gfield%nscal + 3*gfield%nvect + 9*gfield%ntens)*gfield%dim

end function size_tot_gfield


!------------------------------------------------------------------------------
! Procédure : generic field transfer : rgfield receives igfield
!------------------------------------------------------------------------------
subroutine gfield_transfer(rgfield, igfield)
implicit none
type(st_genericfield) :: igfield, rgfield
integer               :: i

rgfield%nscal = igfield%nscal
rgfield%nvect = igfield%nvect
rgfield%ntens = igfield%ntens
rgfield%dim = igfield%dim
do i = 1, igfield%nscal
  call transfer(rgfield%tabscal(i),igfield%tabscal(i))
enddo
do i = 1, igfield%nvect
  call transfer(rgfield%tabvect(i),igfield%tabvect(i))
enddo
do i = 1, igfield%ntens
  call transfer(rgfield%tabtens(i),igfield%tabtens(i))
enddo

endsubroutine gfield_transfer


!------------------------------------------------------------------------------
! Procédure : generic field transfer : rgfield receives igfield
!------------------------------------------------------------------------------
subroutine gfield_transfer_t(rgfield, igfield)
implicit none
type(st_genericfield), dimension(:) :: igfield, rgfield
integer               :: i, ig

do ig = 1, size(rgfield)

  rgfield(ig)%nscal = igfield(ig)%nscal
  rgfield(ig)%nvect = igfield(ig)%nvect
  rgfield(ig)%ntens = igfield(ig)%ntens
  rgfield(ig)%dim = igfield(ig)%dim
  do i = 1, igfield(ig)%nscal
    call transfer(rgfield(ig)%tabscal(i),igfield(ig)%tabscal(i))
  enddo
  do i = 1, igfield(ig)%nvect
    call transfer(rgfield(ig)%tabvect(i),igfield(ig)%tabvect(i))
  enddo
  do i = 1, igfield(ig)%ntens
    call transfer(rgfield(ig)%tabtens(i),igfield(ig)%tabtens(i))
  enddo

enddo

endsubroutine gfield_transfer_t


!------------------------------------------------------------------------------
! Computing routine : gfield_scale : X = a*X
!------------------------------------------------------------------------------
subroutine gfield_scale(x, a)
implicit none
type(st_genericfield) :: x
real(krp)             :: a
integer               :: ip

do ip = 1, x%nscal
  call scale(x%tabscal(ip), a)
enddo

do ip = 1, x%nvect
  call scale(x%tabvect(ip), a)
enddo

do ip = 1, x%ntens
  call scale(x%tabtens(ip), a)
enddo

endsubroutine gfield_scale


!------------------------------------------------------------------------------
! Computing routine : gfield_scale_t : X = a*X
!------------------------------------------------------------------------------
subroutine gfield_scale_t(x, a)
implicit none
type(st_genericfield), dimension(:) :: x
real(krp)             :: a
integer               :: ip, ig

do ig = 1, size(x)

  do ip = 1, x(ig)%nscal
    call scale(x(ig)%tabscal(ip), a)
  enddo

  do ip = 1, x(ig)%nvect
    call scale(x(ig)%tabvect(ip), a)
  enddo

  do ip = 1, x(ig)%ntens
    call scale(x(ig)%tabtens(ip), a)
  enddo

enddo

endsubroutine gfield_scale_t


!------------------------------------------------------------------------------
! Computing routine : gfield_xeqxpy : X = X + Y
!------------------------------------------------------------------------------
subroutine gfield_xeqxpy(x, y)
implicit none
type(st_genericfield) :: x, y
integer               :: ip

do ip = 1, x%nscal
  call xeqxpy(x%tabscal(ip), y%tabscal(ip))
enddo

do ip = 1, x%nvect
  call xeqxpy(x%tabvect(ip), y%tabvect(ip))
enddo

do ip = 1, x%ntens
  call xeqxpy(x%tabtens(ip), y%tabtens(ip))
enddo

endsubroutine gfield_xeqxpy


!------------------------------------------------------------------------------
! Computing routine : gfield_xeqxpay : X = X + a*Y
!------------------------------------------------------------------------------
subroutine gfield_xeqxpay(x, a, y)
implicit none
type(st_genericfield) :: x, y
real(krp)             :: a
integer               :: ip

do ip = 1, x%nscal
  call xeqxpay(x%tabscal(ip), a, y%tabscal(ip))
enddo

do ip = 1, x%nvect
  call xeqxpay(x%tabvect(ip), a, y%tabvect(ip))
enddo

do ip = 1, x%ntens
  call xeqxpay(x%tabtens(ip), a, y%tabtens(ip))
enddo

endsubroutine gfield_xeqxpay


!------------------------------------------------------------------------------
! Computing routine : gfield_xeqxpay_t : X = X + a*Y
!------------------------------------------------------------------------------
subroutine gfield_xeqxpay_t(x, a, y)
implicit none
type(st_genericfield), dimension(:) :: x, y
real(krp)             :: a
integer               :: ip, ig

do ig = 1, size(x)

  do ip = 1, x(ig)%nscal
    call xeqxpay(x(ig)%tabscal(ip), a, y(ig)%tabscal(ip))
  enddo

  do ip = 1, x(ig)%nvect
    call xeqxpay(x(ig)%tabvect(ip), a, y(ig)%tabvect(ip))
  enddo

  do ip = 1, x(ig)%ntens
    call xeqxpay(x(ig)%tabtens(ip), a, y(ig)%tabtens(ip))
  enddo

enddo

endsubroutine gfield_xeqxpay_t


!------------------------------------------------------------------------------
! Computing routine : gfield_l1norm : n = sum(abs(X(i)))
!------------------------------------------------------------------------------
real(krp) function gfield_l1norm(x)
implicit none
type(st_genericfield) :: x
integer               :: ip

gfield_l1norm = 0.0_krp

do ip = 1, x%nscal
  gfield_l1norm = gfield_l1norm + l1norm(x%tabscal(ip))
enddo

do ip = 1, x%nvect
  gfield_l1norm = gfield_l1norm + l1norm(x%tabvect(ip))
enddo

do ip = 1, x%ntens
  gfield_l1norm = gfield_l1norm + l1norm(x%tabtens(ip))
enddo

endfunction gfield_l1norm


!------------------------------------------------------------------------------
! Computing routine : gfield_l2sqnorm : n = sqrt(sum(X(i).X(i)))
!------------------------------------------------------------------------------
real(krp) function gfield_l2sqnorm(x)
implicit none
type(st_genericfield) :: x
integer               :: ip

gfield_l2sqnorm = 0.0_krp

do ip = 1, x%nscal
  gfield_l2sqnorm = gfield_l2sqnorm + l2sqnorm(x%tabscal(ip))
enddo

do ip = 1, x%nvect
  gfield_l2sqnorm = gfield_l2sqnorm + l2sqnorm(x%tabvect(ip))
enddo

do ip = 1, x%ntens
  gfield_l2sqnorm = gfield_l2sqnorm + l2sqnorm(x%tabtens(ip))
enddo

endfunction gfield_l2sqnorm


!------------------------------------------------------------------------------
! Computing routine : gfield_l2sqnorm_t : n = sqrt(sum(X(i).X(i)))
!------------------------------------------------------------------------------
real(krp) function gfield_l2sqnorm_t(x)
implicit none
type(st_genericfield), dimension(:) :: x
integer               :: ip, ig

gfield_l2sqnorm_t = 0.0_krp

do ig = 1, size(x)

  do ip = 1, x(ig)%nscal
    gfield_l2sqnorm_t = gfield_l2sqnorm_t + l2sqnorm(x(ig)%tabscal(ip))
  enddo

  do ip = 1, x(ig)%nvect
    gfield_l2sqnorm_t = gfield_l2sqnorm_t + l2sqnorm(x(ig)%tabvect(ip))
  enddo

  do ip = 1, x(ig)%ntens
    gfield_l2sqnorm_t = gfield_l2sqnorm_t + l2sqnorm(x(ig)%tabtens(ip))
  enddo

enddo

endfunction gfield_l2sqnorm_t


!------------------------------------------------------------------------------
! Computing routine : gfield_l2norm : n = sqrt(sum(X(i).X(i)))
!------------------------------------------------------------------------------
real(krp) function gfield_l2norm(x)
implicit none
type(st_genericfield) :: x
integer               :: ip

gfield_l2norm = 0.0_krp

do ip = 1, x%nscal
  gfield_l2norm = gfield_l2norm + l2sqnorm(x%tabscal(ip))
enddo

do ip = 1, x%nvect
  gfield_l2norm = gfield_l2norm + l2sqnorm(x%tabvect(ip))
enddo

do ip = 1, x%ntens
  gfield_l2norm = gfield_l2norm + l2sqnorm(x%tabtens(ip))
enddo

gfield_l2norm = sqrt(gfield_l2norm)

endfunction gfield_l2norm


!------------------------------------------------------------------------------
! Computing routine : gfield_l2norm_t : n = sqrt(sum(X(i).X(i)))
!------------------------------------------------------------------------------
real(krp) function gfield_l2norm_t(x)
implicit none
type(st_genericfield), dimension(:) :: x
integer               :: ip, ig

gfield_l2norm_t = 0.0_krp

do ig = 1, size(x)

  do ip = 1, x(ig)%nscal
    gfield_l2norm_t = gfield_l2norm_t + l2sqnorm(x(ig)%tabscal(ip))
  enddo

  do ip = 1, x(ig)%nvect
    gfield_l2norm_t = gfield_l2norm_t + l2sqnorm(x(ig)%tabvect(ip))
  enddo

  do ip = 1, x(ig)%ntens
    gfield_l2norm_t = gfield_l2norm_t + l2sqnorm(x(ig)%tabtens(ip))
  enddo

enddo

gfield_l2norm_t = sqrt(gfield_l2norm_t)

endfunction gfield_l2norm_t


!------------------------------------------------------------------------------
! Computing routine : gfield_l1wnorm : n = sum(w(i)*abs(X(i)))
!------------------------------------------------------------------------------
real(krp) function gfield_l1wnorm(x, scaw, vecw, tenw)
implicit none
type(st_genericfield) :: x
real(krp), dimension(x%nscal) :: scaw
real(krp), dimension(x%nvect) :: vecw
real(krp), dimension(x%ntens) :: tenw
integer               :: ip

gfield_l1wnorm = 0.0_krp

do ip = 1, x%nscal
  gfield_l1wnorm = gfield_l1wnorm + scaw(ip)*l1norm(x%tabscal(ip))
enddo

do ip = 1, x%nvect
  gfield_l1wnorm = gfield_l1wnorm + vecw(ip)*l1norm(x%tabvect(ip))
enddo

do ip = 1, x%ntens
  gfield_l1wnorm = gfield_l1wnorm + tenw(ip)*l1norm(x%tabtens(ip))
enddo

endfunction gfield_l1wnorm


!------------------------------------------------------------------------------
! Computing routine : gfield_l2wnorm : n = sqrt(sum(w(i)*X(i).X(i)))
!------------------------------------------------------------------------------
real(krp) function gfield_l2wnorm(x, scaw, vecw, tenw)
implicit none
type(st_genericfield) :: x
real(krp), dimension(x%nscal) :: scaw
real(krp), dimension(x%nvect) :: vecw
real(krp), dimension(x%ntens) :: tenw
integer               :: ip

gfield_l2wnorm = 0.0_krp

do ip = 1, x%nscal
  gfield_l2wnorm = gfield_l2wnorm + scaw(ip)*l2norm(x%tabscal(ip))**2
enddo

do ip = 1, x%nvect
  gfield_l2wnorm = gfield_l2wnorm + vecw(ip)*l2norm(x%tabvect(ip))**2
enddo

do ip = 1, x%ntens
  gfield_l2wnorm = gfield_l2wnorm + tenw(ip)*l2norm(x%tabtens(ip))**2
enddo

gfield_l2wnorm = sqrt(gfield_l2wnorm)

endfunction gfield_l2wnorm


!------------------------------------------------------------------------------
! Computing routine : gfield_dot_prod : n = sum(X(i).Y(i))
!------------------------------------------------------------------------------
real(krp) function gfield_dot_prod(x, y)
implicit none
type(st_genericfield) :: x, y
integer               :: ip

gfield_dot_prod = 0.0_krp

do ip = 1, x%nscal
  gfield_dot_prod = gfield_dot_prod + dot_prod(x%tabscal(ip),y%tabscal(ip))
enddo

do ip = 1, x%nvect
  gfield_dot_prod = gfield_dot_prod + dot_prod(x%tabvect(ip),y%tabvect(ip))
enddo

do ip = 1, x%ntens
  gfield_dot_prod = gfield_dot_prod + dot_prod(x%tabtens(ip),y%tabtens(ip))
enddo

endfunction gfield_dot_prod


!------------------------------------------------------------------------------
! Computing routine : gfield_dot_prod_t : n = sum(X(i).Y(i))
!------------------------------------------------------------------------------
real(krp) function gfield_dot_prod_t(x, y)
implicit none
type(st_genericfield), dimension(:) :: x, y
integer               :: ip, ig

gfield_dot_prod_t = 0.0_krp

do ig = 1, size(x)

  do ip = 1, x(ig)%nscal
    gfield_dot_prod_t = gfield_dot_prod_t + dot_prod(x(ig)%tabscal(ip),y(ig)%tabscal(ip))
  enddo

  do ip = 1, x(ig)%nvect
    gfield_dot_prod_t = gfield_dot_prod_t + dot_prod(x(ig)%tabvect(ip),y(ig)%tabvect(ip))
  enddo

  do ip = 1, x(ig)%ntens
    gfield_dot_prod_t = gfield_dot_prod_t + dot_prod(x(ig)%tabtens(ip),y(ig)%tabtens(ip))
  enddo

enddo

endfunction gfield_dot_prod_t


!------------------------------------------------------------------------------
! Computing routine : gfield_wdot_prod : n = sum(w(i)*X(i).Y(i))
!------------------------------------------------------------------------------
real(krp) function gfield_wdot_prod(x, y, scaw, vecw, tenw)
implicit none
type(st_genericfield) :: x, y
real(krp), dimension(x%nscal) :: scaw
real(krp), dimension(x%nvect) :: vecw
real(krp), dimension(x%ntens) :: tenw
integer               :: ip

gfield_wdot_prod = 0.0_krp

do ip = 1, x%nscal
  gfield_wdot_prod = gfield_wdot_prod + scaw(ip)*dot_prod(x%tabscal(ip),y%tabscal(ip))
enddo

do ip = 1, x%nvect
  gfield_wdot_prod = gfield_wdot_prod + vecw(ip)*dot_prod(x%tabvect(ip),y%tabvect(ip))
enddo

do ip = 1, x%ntens
  gfield_wdot_prod = gfield_wdot_prod + tenw(ip)*dot_prod(x%tabtens(ip),y%tabtens(ip))
enddo

endfunction gfield_wdot_prod


endmodule GENFIELD

!------------------------------------------------------------------------------!
! Changes history
!
! Oct 2002 : creation
! Jun 2003 : field structure by type (scalar, vector, tensor)
! Jun 2004 : procedures insert_newgfield and delete_chainedgfield
! Nov 2004 : split DEFFIELD -> DEFFIELD/GENFIELD/BASEFIELD
! Aug 2005 : size of field
!------------------------------------------------------------------------------!
