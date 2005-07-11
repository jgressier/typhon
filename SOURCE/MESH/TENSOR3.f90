!------------------------------------------------------------------------------!
! MODULE : TENSOR3                        Auteur : J. Gressier
!                                         Date   : July 2003
! Fonction                                Modif  :
!   Bibliotheque de procedures et fonctions pour le calcul geometrique 3D
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

module TENSOR3

use TYPHMAKE
use GEO3D

! -- DECLARATIONS -----------------------------------------------------------

type t3d
  real(krp), dimension(3,3) :: mat
endtype

! -- INTERFACES -------------------------------------------------------------

!interface abs
!  module procedure v3d_norme
!endinterface

interface operator(+)
  module procedure t3d_addition, t3d_addition_t
endinterface

interface operator(-)
  module procedure t3d_substraction, t3d_opp, t3d_substraction_t 
endinterface

interface operator(*)
  module procedure t3d_multiply, t3d_multiply_t, t3d_multiply_tt, t3d_multvect, t3d_multvect_tt
endinterface

interface operator(/)
  module procedure t3d_division
endinterface

interface operator(.scal.)
  module procedure t3d_scalar_product, t3d_scalar_product_t
endinterface

interface operator(.tens.)
  module procedure t3d_tensorial_product, t3d_tensorial_product_t
endinterface

! -- Fonctions et Operateurs ------------------------------------------------


! -- IMPLEMENTATION ---------------------------------------------------------
contains

!------------------------------------------------------------------------------!
! Fonction : tensor addition
!------------------------------------------------------------------------------!
type(t3d) function t3d_addition(v1, v2)
implicit none
type(t3d), intent(in) :: v1, v2

  t3d_addition%mat = v1%mat + v2%mat

endfunction t3d_addition

!------------------------------------------------------------------------------!
! Fonction : tensor + tensor (arrays)
!------------------------------------------------------------------------------!
function t3d_addition_t(t1, t2) result(tr)
implicit none
type(t3d), intent(in) :: t1(:), t2(:)
type(t3d), dimension(size(t1)) :: tr
integer :: i

  do i = 1, size(t1)
    tr(i)%mat = t1(i)%mat + t2(i)%mat
  enddo

endfunction t3d_addition_t

!------------------------------------------------------------------------------!
! Fonction : tensor substraction
!------------------------------------------------------------------------------!
type(t3d) function t3d_substraction(v1, v2)
implicit none
type(t3d), intent(in) :: v1, v2

  t3d_substraction%mat = v1%mat - v2%mat 

endfunction t3d_substraction

!------------------------------------------------------------------------------!
! Fonction : tensor - tensor (arrays)
!------------------------------------------------------------------------------!
function t3d_substraction_t(t1, t2) result(tr)
implicit none
type(t3d), intent(in) :: t1(:), t2(:)
type(t3d), dimension(size(t1)) :: tr
integer :: i

  do i = 1, size(t1)
    tr(i)%mat = t1(i)%mat - t2(i)%mat
  enddo

endfunction t3d_substraction_t

!------------------------------------------------------------------------------!
! Fonction : opposite tensor
!------------------------------------------------------------------------------!
type(t3d) function t3d_opp(v)
implicit none
type(t3d), intent(in) :: v

  t3d_opp%mat = - v%mat

endfunction t3d_opp

!------------------------------------------------------------------------------!
! Fonction : transpose tensor
!------------------------------------------------------------------------------!
type(t3d) function t3d_transp(t)
implicit none
type(t3d), intent(in) :: t

  t3d_transp%mat = transpose(t%mat)

endfunction t3d_transp

!------------------------------------------------------------------------------!
! Fonction : scalar * tensor
!------------------------------------------------------------------------------!
type(t3d) function t3d_multiply(x, v)
implicit none
real(krp), intent(in) :: x
type(t3d), intent(in) :: v

  t3d_multiply%mat = x * v%mat

endfunction t3d_multiply

!------------------------------------------------------------------------------!
! Fonction : scalar * tensor (array)
!------------------------------------------------------------------------------!
function t3d_multiply_t(x, t) result(tr)
implicit none
real(krp), intent(in) :: x
type(t3d), intent(in) :: t(:)
type(t3d), dimension(size(t)) :: tr
integer :: i

  do i = 1, size(t)
    tr(i)%mat = x * t(i)%mat
  enddo

endfunction t3d_multiply_t

!------------------------------------------------------------------------------!
! Fonction : scalar (array) * tensor (array)
!------------------------------------------------------------------------------!
function t3d_multiply_tt(x, v) result(tr)
implicit none
real(krp), intent(in) :: x(:)
type(t3d), intent(in) :: v(:)
type(t3d), dimension(size(v)) :: tr
integer :: i

  do i = 1, size(v)
    tr(i)%mat = x(i) * v(i)%mat
  enddo

endfunction t3d_multiply_tt

!------------------------------------------------------------------------------!
! Fonction : vector * tensor
!------------------------------------------------------------------------------!
function t3d_multvect(v, t) result(tr)
implicit none
type(v3d), intent(in) :: v
type(t3d), intent(in) :: t
type(t3d) :: tr

  tr%mat(1,:) = v%x * t%mat(1,:)
  tr%mat(2,:) = v%y * t%mat(2,:)
  tr%mat(3,:) = v%z * t%mat(3,:)

endfunction t3d_multvect

!------------------------------------------------------------------------------!
! Fonction : scalar (array) * tensor (array)
!------------------------------------------------------------------------------!
function t3d_multvect_tt(v, t) result(tr)
implicit none
type(v3d), intent(in) :: v(:)
type(t3d), intent(in) :: t(:)
type(t3d), dimension(size(v)) :: tr
integer :: i

  do i = 1, size(v)
    tr(i)%mat(1,:) = v(i)%x * t(i)%mat(1,:)
    tr(i)%mat(2,:) = v(i)%y * t(i)%mat(2,:)
    tr(i)%mat(3,:) = v(i)%z * t(i)%mat(3,:)
  enddo

endfunction t3d_multvect_tt

!------------------------------------------------------------------------------!
! Fonction : calcul de division de vecteur par reel
!------------------------------------------------------------------------------!
type(t3d) function t3d_division(v,x)
implicit none
real(krp), intent(in) :: x
type(t3d),   intent(in) :: v

  t3d_division%mat = v%mat / x

endfunction t3d_division

!------------------------------------------------------------------------------!
! Fonction : norme de vecteur
!------------------------------------------------------------------------------!
!real(krp) function v3d_norme(v)
!implicit none
!type(v3d), intent(in) :: v

!  v3d_norme = sqrt(v%x*v%x + v%y*v%y + v%z*v%z)

!endfunction v3d_norme

!------------------------------------------------------------------------------!
! Fonction : calcul de produit scalaire
!------------------------------------------------------------------------------!
type(v3d) function t3d_scalar_product(t, v)
implicit none
type(t3d), intent(in) :: t
type(v3d), intent(in) :: v

  t3d_scalar_product = v3d_of(matmul(t%mat, tab(v)))

endfunction t3d_scalar_product

!------------------------------------------------------------------------------!
! Fonction : calcul de produit scalaire
!------------------------------------------------------------------------------!
function t3d_scalar_product_t(t, v) result(vt)
implicit none
type(t3d), intent(in) :: t(:)
type(v3d), intent(in) :: v(:)
type(v3d), dimension(size(t)) :: vt
integer :: i

  do i = 1, size(t)
    vt(i) = v3d_of(matmul(t(i)%mat, tab(v(i))))
  enddo

endfunction t3d_scalar_product_t

!------------------------------------------------------------------------------!
! Fonction : tensorial product
!------------------------------------------------------------------------------!
function t3d_tensorial_product(v1, v2) result(tr)
implicit none
type(v3d), intent(in) :: v1, v2
type(t3d)             :: tr

  tr%mat(1,:) = v1%x * tab(v2)
  tr%mat(2,:) = v1%y * tab(v2)
  tr%mat(3,:) = v1%z * tab(v2)

endfunction t3d_tensorial_product

!------------------------------------------------------------------------------!
! Fonction : tensorial product
!------------------------------------------------------------------------------!
function t3d_tensorial_product_t(v1, v2) result(tr)
implicit none
type(v3d), intent(in) :: v1(:), v2(:)
type(t3d), dimension(size(v1)) :: tr
integer :: i
 
  do i = 1, size(v1)
    tr(i)%mat(1,:) = v1(i)%x * tab(v2(i))
    tr(i)%mat(2,:) = v1(i)%y * tab(v2(i))
    tr(i)%mat(3,:) = v1(i)%z * tab(v2(i))
  enddo

endfunction t3d_tensorial_product_t

!------------------------------------------------------------------------------!
! Fonction : trace
!------------------------------------------------------------------------------!
real(krp) function t3d_trace(v)
implicit none
type(t3d), intent(in) :: v

  t3d_trace = v%mat(1,1) + v%mat(2,2) + v%mat(3,3)

endfunction t3d_trace

endmodule TENSOR3
!------------------------------------------------------------------------------!
! Changes history
!
! july 2003 : created, structure definition
! nov  2004 : basic operators
! jun 2005  : trace operator
!------------------------------------------------------------------------------!

