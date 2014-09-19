!------------------------------------------------------------------------------!
! MODULE : VEC3D
!
! Definition of 3D vector and associated functions
!------------------------------------------------------------------------------!
module VEC3D

use MESHPREC
use VEC2D
use STRING

! -- DECLARATIONS -----------------------------------------------------------

type v3d
  real(krp) :: x, y, z
endtype v3d

! -- Constants    -----------------------------------------------------------

type(v3d), parameter :: v3d_zero = v3d(0._krp, 0._krp, 0._krp)

! -- INTERFACES -------------------------------------------------------------

interface v3d_of
  module procedure v3d_fromtab, v3d_fromv2d, v3d_fromstr
endinterface

interface tab
  module procedure tab_v3d
endinterface

interface abs
  module procedure v3d_norme, v3d_norme_t
endinterface

interface sqrabs
  module procedure v3d_sqrnorme, v3d_sqrnorme_t
endinterface

interface scale
  module procedure v3d_eq_mult, v3d_eq_mult_t, v3d_eq_mult_tt
endinterface

interface shift_add
  module procedure v3d_shift, v3d_shift_t, v3d_shift_tt
endinterface

interface shift_sub
  module procedure v3d_shiftopp, v3d_shiftopp_t, v3d_shiftopp_tt
endinterface

interface rot
  module procedure v3d_rot, v3d_rot_t
endinterface

interface sum
  module procedure v3d_sum
endinterface

interface operator(+)
  module procedure v3d_add, v3d_add_t
endinterface

interface operator(-)
  module procedure v3d_sub, v3d_opp, v3d_sub_t, v3d_opp_t
endinterface

interface operator(*)
  module procedure v3d_mult, v3d_mult_t1, v3d_mult_t2, v3d_mult_tt
endinterface

interface operator(/)
  module procedure v3d_div, v3d_div_t, v3d_div_tt
endinterface

interface operator(.scal.)
  module procedure v3d_scalar_product, v3d_scalar_product_t
endinterface

interface operator(.vect.)
  module procedure v3d_vectorial_product
endinterface

! -- Fonctions et Operateurs ------------------------------------------------


! -- IMPLEMENTATION ---------------------------------------------------------
contains

!------------------------------------------------------------------------------!
! Fonction : transtypage real(1:3) -> v3d
!------------------------------------------------------------------------------!
type(v3d) function v3d_fromtab(tab)
implicit none
real(krp), dimension(3) :: tab

  v3d_fromtab = v3d(tab(1), tab(2), tab(3))

endfunction v3d_fromtab

!------------------------------------------------------------------------------!
! Fonction : transtypage v2d -> v3d
!------------------------------------------------------------------------------!
type(v3d) function v3d_fromv2d(v)
implicit none
type(v2d) :: v

  v3d_fromv2d = v3d(v%x, v%y, 0._krp)

endfunction v3d_fromv2d

!------------------------------------------------------------------------------!
! Fonction : transtypage v3d -> real(1:3)
!------------------------------------------------------------------------------!
function tab_v3d(v)
implicit none
type(v3d), intent(in)   :: v
real(krp), dimension(3) :: tab_v3d

  tab_v3d(1:3) = (/ v%x, v%y, v%z /)

endfunction tab_v3d

!------------------------------------------------------------------------------!
! Fonction : transtypage (avec traitement) string -> v3d
!------------------------------------------------------------------------------!
type(v3d) function v3d_fromstr(str, info)
implicit none
character(len=*), intent(in)  :: str
integer,          intent(out) :: info
character(len=len(str))       :: pstr
integer :: id, if

  info = 0
  id   = scan(str, '(')
  if   = scan(str, ')')
  pstr = chg_char(str(id+1:if-1),',',' ')
  read(pstr,*,iostat=info) v3d_fromstr !%x, v3d_fromstr%z, v3d_fromstr%y

endfunction v3d_fromstr

!------------------------------------------------------------------------------!
! Fonction : vector add
!------------------------------------------------------------------------------!
type(v3d) function v3d_add(v1, v2)
implicit none
type(v3d), intent(in) :: v1, v2

  v3d_add%x = v1%x + v2%x
  v3d_add%y = v1%y + v2%y
  v3d_add%z = v1%z + v2%z

endfunction v3d_add

!------------------------------------------------------------------------------!
! Fonction : vector add (array)
!------------------------------------------------------------------------------!
function v3d_add_t(v1, v2) result(tv)
implicit none
type(v3d), dimension(:), intent(in) :: v1, v2
type(v3d), dimension(size(v1))      :: tv
integer :: i, n

  n = size(v1)
  if (size(v2) /= n) stop
  do i = 1, n
    tv(i)%x = v1(i)%x + v2(i)%x
    tv(i)%y = v1(i)%y + v2(i)%y
    tv(i)%z = v1(i)%z + v2(i)%z
  enddo

endfunction v3d_add_t


!------------------------------------------------------------------------------!
! Assignment : vector v = v + w
!------------------------------------------------------------------------------!
subroutine v3d_shift(v, a)
implicit none
type(v3d), intent(in)    :: a
type(v3d), intent(inout) :: v

  v%x = v%x + a%x
  v%y = v%y + a%y
  v%z = v%z + a%z

end subroutine v3d_shift

!------------------------------------------------------------------------------!
! Assignment : v(:) = v(:) + w
!------------------------------------------------------------------------------!
subroutine v3d_shift_t(v, a)
implicit none
type(v3d), intent(in)    :: a
type(v3d), intent(inout) :: v(:)
integer :: i

  do i = 1, size(v)
    v(i)%x = v(i)%x + a%x
    v(i)%y = v(i)%y + a%y
    v(i)%z = v(i)%z + a%z
  enddo

end subroutine v3d_shift_t

!------------------------------------------------------------------------------!
! Assignment : v(:) = a(:) + v(:)
!------------------------------------------------------------------------------!
subroutine v3d_shift_tt(v, a)
implicit none
type(v3d), intent(in)    :: a(:)
type(v3d), intent(inout) :: v(:)
integer :: i

  do i = 1, size(v)
    v(i)%x = v(i)%x + a(i)%x
    v(i)%y = v(i)%y + a(i)%y
    v(i)%z = v(i)%z + a(i)%z
  enddo

end subroutine v3d_shift_tt

!------------------------------------------------------------------------------!
! Fonction : vector sub
!------------------------------------------------------------------------------!
type(v3d) function v3d_sub(v1, v2)
implicit none
type(v3d), intent(in) :: v1, v2

  v3d_sub%x = v1%x - v2%x
  v3d_sub%y = v1%y - v2%y
  v3d_sub%z = v1%z - v2%z

endfunction v3d_sub

!------------------------------------------------------------------------------!
! Fonction : vector sub (array)
!------------------------------------------------------------------------------!
function v3d_sub_t(v1, v2) result(tv)
implicit none
type(v3d), dimension(:), intent(in) :: v1, v2
type(v3d), dimension(size(v1))      :: tv
integer :: i, n

  n = size(v1)
  if (size(v2) /= n) stop
  do i = 1, n
    tv(i)%x = v1(i)%x - v2(i)%x
    tv(i)%y = v1(i)%y - v2(i)%y
    tv(i)%z = v1(i)%z - v2(i)%z
  enddo

endfunction v3d_sub_t

!------------------------------------------------------------------------------!
! Assignment : vector v = v - w
!------------------------------------------------------------------------------!
subroutine v3d_shiftopp(v, a)
implicit none
type(v3d), intent(in)    :: a
type(v3d), intent(inout) :: v

  v%x = v%x - a%x
  v%y = v%y - a%y
  v%z = v%z - a%z

end subroutine v3d_shiftopp

!------------------------------------------------------------------------------!
! Assignment : v(:) = v(:) - w
!------------------------------------------------------------------------------!
subroutine v3d_shiftopp_t(v, a)
implicit none
type(v3d), intent(in)    :: a
type(v3d), intent(inout) :: v(:)
integer :: i

  do i = 1, size(v)
    v(i)%x = v(i)%x - a%x
    v(i)%y = v(i)%y - a%y
    v(i)%z = v(i)%z - a%z
  enddo

end subroutine v3d_shiftopp_t

!------------------------------------------------------------------------------!
! Assignment : v(:) = a(:) - v(:)
!------------------------------------------------------------------------------!
subroutine v3d_shiftopp_tt(v, a)
implicit none
type(v3d), intent(in)    :: a(:)
type(v3d), intent(inout) :: v(:)
integer :: i

  do i = 1, size(v)
    v(i)%x = v(i)%x - a(i)%x
    v(i)%y = v(i)%y - a(i)%y
    v(i)%z = v(i)%z - a(i)%z
  enddo

end subroutine v3d_shiftopp_tt

!------------------------------------------------------------------------------!
! Fonction : vector opposite
!------------------------------------------------------------------------------!
type(v3d) function v3d_opp(v)
implicit none
type(v3d), intent(in) :: v

  v3d_opp%x = - v%x
  v3d_opp%y = - v%y
  v3d_opp%z = - v%z

endfunction v3d_opp

!------------------------------------------------------------------------------!
! Fonction : vector opposite (array)
!------------------------------------------------------------------------------!
function v3d_opp_t(v) result(tv)
implicit none
type(v3d), dimension(:), intent(in) :: v
type(v3d), dimension(size(v))      :: tv
integer :: i

  do i = 1, size(v)
    tv(i)%x = - v(i)%x
    tv(i)%y = - v(i)%y
    tv(i)%z = - v(i)%z
  enddo

endfunction v3d_opp_t

!------------------------------------------------------------------------------!
! Fonction : vector multiplied par real
!------------------------------------------------------------------------------!
type(v3d) function v3d_mult(x, v)
implicit none
real(krp),   intent(in) :: x
type(v3d), intent(in) :: v

  v3d_mult%x = x * v%x
  v3d_mult%y = x * v%y
  v3d_mult%z = x * v%z

endfunction v3d_mult

!------------------------------------------------------------------------------!
! Assignment : vector v = a . v
!------------------------------------------------------------------------------!
subroutine v3d_eq_mult(v, a)
implicit none
real(krp), intent(in)    :: a
type(v3d), intent(inout) :: v

  v%x = a * v%x
  v%y = a * v%y
  v%z = a * v%z

end subroutine v3d_eq_mult

!------------------------------------------------------------------------------!
! Fonction : vector (array) multiplied par real
!------------------------------------------------------------------------------!
function v3d_mult_t2(x, v) result(tv)
implicit none
real(krp), intent(in) :: x
type(v3d), dimension(:), intent(in) :: v
type(v3d), dimension(size(v))       :: tv
integer :: i

  do i = 1, size(v)
    tv(i)%x = x * v(i)%x
    tv(i)%y = x * v(i)%y
    tv(i)%z = x * v(i)%z
  enddo

endfunction v3d_mult_t2

!------------------------------------------------------------------------------!
! Fonction : vector multiplied par real (array)
!------------------------------------------------------------------------------!
function v3d_mult_t1(x, v) result(tv)
implicit none
real(krp), dimension(:), intent(in) :: x
type(v3d),               intent(in) :: v
type(v3d), dimension(size(x))       :: tv
integer :: i

  do i = 1, size(x)
    tv(i)%x = x(i) * v%x
    tv(i)%y = x(i) * v%y
    tv(i)%z = x(i) * v%z
  enddo

endfunction v3d_mult_t1

!------------------------------------------------------------------------------!
! Assignment : v(:) = a . v(:)
!------------------------------------------------------------------------------!
subroutine v3d_eq_mult_t(v, a)
implicit none
real(krp), intent(in)    :: a
type(v3d), intent(inout) :: v(:)
integer :: i

  do i = 1, size(v)
    v(i)%x = a * v(i)%x
    v(i)%y = a * v(i)%y
    v(i)%z = a * v(i)%z
  enddo

end subroutine v3d_eq_mult_t

!------------------------------------------------------------------------------!
! Fonction : vector multiplied par real (array*array)
!------------------------------------------------------------------------------!
function v3d_mult_tt(x, v) result(tv)
implicit none
real(krp), dimension(:), intent(in) :: x
type(v3d), dimension(:), intent(in) :: v
type(v3d), dimension(size(v))       :: tv
integer :: i

  do i = 1, size(v)
    tv(i)%x = x(i) * v(i)%x
    tv(i)%y = x(i) * v(i)%y
    tv(i)%z = x(i) * v(i)%z
  enddo

endfunction v3d_mult_tt

!------------------------------------------------------------------------------!
! Assignment : v(:) = a(:) . v(:)
!------------------------------------------------------------------------------!
subroutine v3d_eq_mult_tt(v, a)
implicit none
real(krp), intent(in)    :: a(:)
type(v3d), intent(inout) :: v(:)
integer :: i

  do i = 1, size(v)
    v(i)%x = a(i) * v(i)%x
    v(i)%y = a(i) * v(i)%y
    v(i)%z = a(i) * v(i)%z
  enddo

end subroutine v3d_eq_mult_tt


!------------------------------------------------------------------------------!
! Fonction :  vector divided par real
!------------------------------------------------------------------------------!
type(v3d) function v3d_div(v,x)
implicit none
real(krp), intent(in) :: x
type(v3d), intent(in) :: v

  v3d_div%x = v%x / x   ! DEV / a optimiser
  v3d_div%y = v%y / x
  v3d_div%z = v%z / x

endfunction v3d_div

!------------------------------------------------------------------------------!
! Fonction : vector divided par real (array)
!------------------------------------------------------------------------------!
function v3d_div_t(v, x) result(tv)
implicit none
real(krp), intent(in) :: x
type(v3d), dimension(:), intent(in) :: v
type(v3d), dimension(size(v))       :: tv
integer :: i

  do i = 1, size(v)
    tv(i)%x = v(i)%x /x
    tv(i)%y = v(i)%y /x
    tv(i)%z = v(i)%z /x
  enddo

endfunction v3d_div_t

!------------------------------------------------------------------------------!
! Fonction : vector divided par real (array*array)
!------------------------------------------------------------------------------!
function v3d_div_tt(v, x) result(tv)
implicit none
real(krp), dimension(:), intent(in) :: x
type(v3d), dimension(:), intent(in) :: v
type(v3d), dimension(size(v))       :: tv
integer :: i

  do i = 1, size(v)
    tv(i)%x = v(i)%x /x(i)
    tv(i)%y = v(i)%y /x(i)
    tv(i)%z = v(i)%z /x(i)
  enddo

endfunction v3d_div_tt

!------------------------------------------------------------------------------!
! Fonction : vector magnitude
!------------------------------------------------------------------------------!
real(krp) function v3d_norme(v)
implicit none
type(v3d), intent(in) :: v

  v3d_norme = sqrt(v%x*v%x + v%y*v%y + v%z*v%z)

endfunction v3d_norme

!------------------------------------------------------------------------------!
! Fonction : vector magnitude (array)
!------------------------------------------------------------------------------!
function v3d_norme_t(v) result(t)
implicit none
type(v3d), dimension(:), intent(in) :: v
real(krp), dimension(size(v))       :: t
integer :: i

  do i = 1, size(v)
    t(i) = sqrt(v(i)%x*v(i)%x + v(i)%y*v(i)%y + v(i)%z*v(i)%z)
  enddo

endfunction v3d_norme_t

!------------------------------------------------------------------------------!
! Fonction : square of norme magnitude
!------------------------------------------------------------------------------!
real(krp) function v3d_sqrnorme(v)
implicit none
type(v3d), intent(in) :: v

  v3d_sqrnorme = v%x*v%x + v%y*v%y + v%z*v%z

endfunction v3d_sqrnorme

!------------------------------------------------------------------------------!
! Fonction : square of vector magnitude (array)
!------------------------------------------------------------------------------!
function v3d_sqrnorme_t(v) result(t)
implicit none
type(v3d), dimension(:), intent(in) :: v
real(krp), dimension(size(v))       :: t
integer :: i

  do i = 1, size(v)
    t(i) = v(i)%x*v(i)%x + v(i)%y*v(i)%y + v(i)%z*v(i)%z
  enddo

endfunction v3d_sqrnorme_t

!------------------------------------------------------------------------------!
! Fonction : dot product
!------------------------------------------------------------------------------!
real(krp) function v3d_scalar_product(v1, v2)
implicit none
type(v3d), intent(in) :: v1, v2

  v3d_scalar_product = v1%x*v2%x + v1%y*v2%y + v1%z*v2%z

endfunction v3d_scalar_product

!------------------------------------------------------------------------------!
! Fonction : dot product (array)
!------------------------------------------------------------------------------!
function v3d_scalar_product_t(v1, v2) result(tv)
implicit none
type(v3d), dimension(:), intent(in) :: v1, v2
real(krp), dimension(size(v1))      :: tv
integer :: i, n

  n = size(v1)
  if (size(v2) /= n) stop
  do i = 1, n
    tv(i) = v1(i)%x*v2(i)%x + v1(i)%y*v2(i)%y + v1(i)%z*v2(i)%z
  enddo

endfunction v3d_scalar_product_t

!------------------------------------------------------------------------------!
! Fonction : produit vectoriel
!------------------------------------------------------------------------------!
type(v3d) function v3d_vectorial_product(v1, v2)
implicit none
type(v3d), intent(in) :: v1, v2

  v3d_vectorial_product%x = v1%y*v2%z - v1%z*v2%y
  v3d_vectorial_product%y = v1%z*v2%x - v1%x*v2%z
  v3d_vectorial_product%z = v1%x*v2%y - v1%y*v2%x

endfunction v3d_vectorial_product

!------------------------------------------------------------------------------!
! Fonction : sum of v3d vector to a single v3d
!------------------------------------------------------------------------------!
function v3d_sum(v) result(t)
implicit none
type(v3d), dimension(:), intent(in) :: v
type(v3d)                           :: t
integer :: i

  t = v3d( 0._krp, 0._krp, 0._krp )
  do i = 1, size(v)
    t%x = t%x  + v(i)%x
    t%y = t%y  + v(i)%y
    t%z = t%z  + v(i)%z
  enddo

endfunction v3d_sum

!------------------------------------------------------------------------------!
! Assignment : rotation v
!------------------------------------------------------------------------------!
subroutine v3d_rot(v, axis, angle)
implicit none
type(v3d), intent(in)    :: axis
real(krp), intent(in)    :: angle
type(v3d), intent(inout) :: v
real(krp) :: costh, sinth

  costh = cos(angle)
  sinth = sin(angle)
  v = costh*v + sinth*(axis.vect.v) + ((1._krp-costh)*(axis.scal.v))*axis

end subroutine v3d_rot

!------------------------------------------------------------------------------!
! Assignment : sym v  // n
!------------------------------------------------------------------------------!
subroutine v3d_sym(v, n)
implicit none
type(v3d), intent(in)    :: n
type(v3d), intent(inout) :: v
real(krp) :: sym(3,3)
  call calc_symn(sym, n)
  v = v3d_of(matmul(sym, tab(v)))
end subroutine v3d_sym

!------------------------------------------------------------------------------!
! Assignment : sym v  // n
!------------------------------------------------------------------------------!
subroutine v3d_sym2(v, n)
implicit none
type(v3d), intent(in)    :: n
type(v3d), intent(inout) :: v
  v = v - (2._krp*(v.scal.n))*n
end subroutine v3d_sym2

!------------------------------------------------------------------------------!
! computation of rotation matrix
!------------------------------------------------------------------------------!
subroutine calc_rot(mrot, axis, angle)
implicit none
type(v3d), intent(in)  :: axis
real(krp), intent(in)  :: angle
real(krp), intent(out) :: mrot(3,3)
real(krp) :: costh, sinth

  costh = cos(angle)
  sinth = sin(angle)
  mrot(1,1)   = costh
  mrot(2,2)   = costh
  mrot(3,3)   = costh
  mrot(1,2)   = - axis%z*sinth
  mrot(1,3)   = + axis%y*sinth
  mrot(2,1)   = + axis%z*sinth
  mrot(2,3)   = - axis%x*sinth
  mrot(3,1)   = - axis%y*sinth
  mrot(3,2)   = + axis%x*sinth
  mrot(1,1:3) = mrot(1,1:3) + (1._krp-costh)*axis%x*tab(axis)
  mrot(2,1:3) = mrot(2,1:3) + (1._krp-costh)*axis%y*tab(axis)
  mrot(3,1:3) = mrot(3,1:3) + (1._krp-costh)*axis%z*tab(axis)

end subroutine calc_rot

!------------------------------------------------------------------------------!
! computation of symmetry matrix
!------------------------------------------------------------------------------!
subroutine calc_symn(msym, n)
implicit none
type(v3d), intent(in)  :: n
real(krp), intent(out) :: msym(3,3)

  msym(1,1)   = 1._krp - 2._krp*n%x*n%x
  msym(2,2)   = 1._krp - 2._krp*n%y*n%y
  msym(3,3)   = 1._krp - 2._krp*n%z*n%z
  msym(1,2)   = - 2._krp*n%x*n%y
  msym(1,3)   = - 2._krp*n%x*n%z
  msym(2,1)   = msym(1,2)
  msym(2,3)   = - 2._krp*n%y*n%z
  msym(3,1)   = msym(1,3)
  msym(3,2)   = msym(2,3)

end subroutine calc_symn

!------------------------------------------------------------------------------!
! Assignment : rotation v(:)
!------------------------------------------------------------------------------!
subroutine v3d_rot_t(v, axis, angle)
implicit none
type(v3d), intent(in)    :: axis
real(krp), intent(in)    :: angle
type(v3d), intent(inout) :: v(:)
integer   :: i
real(krp) :: mrot(3,3)

  call calc_rot(mrot, axis, angle)

  do i = 1, size(v)
    v(i) = v3d_of(matmul(mrot, tab(v(i))))
  enddo

end subroutine v3d_rot_t



endmodule VEC3D
!------------------------------------------------------------------------------!
! Change history
!
! May 2002 : creation du module
! Jul 2003 : compatibilite des operateurs toute precision
! Mar 2006 : subroutine transformers for vector array shifting and scaling
! Oct 2009 : fork from TYPHON (GEO3D.f90)
!------------------------------------------------------------------------------!

