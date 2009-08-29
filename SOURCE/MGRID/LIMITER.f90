!------------------------------------------------------------------------------!
! MODULE : LIMITER                        Auteur : J. Gressier
!                                         Date   : Nov 2004
! Fonction                                Modif  : cf history
!   Computation of limiters
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

module LIMITER

use TYPHMAKE   ! Definition de la precision
use GEO3D      ! Compilation conditionnelle 

! -- DECLARATIONS -----------------------------------------------------------



! -- INTERFACES -------------------------------------------------------------

interface minmod
  module procedure minmod, minmod_t, minmod_v, minmod_vt
endinterface

interface albada
  module procedure albada, albada_t, albada_v, albada_vt
endinterface

interface vleer
  module procedure vleer, vleer_t, vleer_v, vleer_vt
endinterface

interface superbee
  module procedure superbee, superbee_t, superbee_v, superbee_vt
endinterface

interface kim3
  module procedure kim3, kim3_t, kim3_v, kim3_vt
endinterface

interface lim03
  module procedure lim03, lim03_t, lim03_v, lim03_vt
endinterface

interface monotonic0
  module procedure monotonic0, monotonic0_v
endinterface

interface monotonic1
  module procedure monotonic1, monotonic1_v
endinterface

interface monotonic2
  module procedure monotonic2, monotonic2_v
endinterface


! -- Fonctions et Operateurs ------------------------------------------------


! -- IMPLEMENTATION ---------------------------------------------------------
contains

!-----------------------------------------------------------------------
!     Fonctions  limiteur            
!-----------------------------------------------------------------------
!     Calcul des gradients limites    
!         dx1  : Gradient de reference 
!         dx2  : Gradient de comparaison
!-----------------------------------------------------------------------
function minmod(x1, x2)
implicit none
real(krp) :: x1, x2, ad1, ad2, s, minmod

  ad1 = abs(x1)
  ad2 = abs(x2)
  s   = .5*( sign(1._krp,x1) + sign(1._krp,x2) )
  minmod = s*min(ad1, ad2)

end function minmod

!-----------------------------------------------------------------------
function minmod_t(x1, x2)
implicit none
real(krp), dimension(:)        :: x1, x2
real(krp), dimension(size(x1)) :: minmod_t

  minmod_t = 0._krp  
  where ((x1 > 0._krp).and.(x2 > 0._krp)) minmod_t = min(x1, x2)
  where ((x1 < 0._krp).and.(x2 < 0._krp)) minmod_t = max(x1, x2)

end function minmod_t

!-----------------------------------------------------------------------
function minmod_v(v1, v2)
implicit none
type(v3d) :: v1, v2, minmod_v

  minmod_v%x = minmod(v1%x, v2%x)
  minmod_v%y = minmod(v1%y, v2%y)
  minmod_v%z = minmod(v1%z, v2%z)

end function minmod_v

!-----------------------------------------------------------------------
function minmod_vt(v1, v2)
implicit none
type(v3d), dimension(:) :: v1, v2
type(v3d), dimension(size(v1)) :: minmod_vt
integer :: i

  do i = 1, size(v1)
    minmod_vt(i)%x = minmod(v1(i)%x, v2(i)%x)
    minmod_vt(i)%y = minmod(v1(i)%y, v2(i)%y)
    minmod_vt(i)%z = minmod(v1(i)%z, v2(i)%z)
  enddo

end function minmod_vt

!-----------------------------------------------------------------------
function albada(x1, x2)
implicit none
real(krp) :: x1, x2, s, albada

  s = x1*x2
  if (s.le.0._krp) then
    albada = 0._krp
  else
    albada = s*(x1+x2)/(x1*x1+x2*x2)
  endif

end function albada

!-----------------------------------------------------------------------
function albada_t(x1, x2)
implicit none
real(krp), dimension(:)        :: x1, x2
real(krp), dimension(size(x1)) :: albada_t

  albada_t = 0._krp
  where ((x1 > 0._krp).and.(x2 > 0._krp)) albada_t = x1*x2*(x1+x2)/(x1*x1+x2*x2)
  where ((x1 < 0._krp).and.(x2 < 0._krp)) albada_t = x1*x2*(x1+x2)/(x1*x1+x2*x2)

end function albada_t

!-----------------------------------------------------------------------
function albada_v(v1, v2)
implicit none
type(v3d) :: v1, v2, albada_v

  albada_v%x = albada(v1%x, v2%x)
  albada_v%y = albada(v1%y, v2%y)
  albada_v%z = albada(v1%z, v2%z)

end function albada_v

!-----------------------------------------------------------------------
function albada_vt(v1, v2)
implicit none
type(v3d), dimension(:) :: v1, v2
type(v3d), dimension(size(v1)) :: albada_vt
integer :: i

  do i = 1, size(v1)
    albada_vt(i)%x = albada(v1(i)%x, v2(i)%x)
    albada_vt(i)%y = albada(v1(i)%y, v2(i)%y)
    albada_vt(i)%z = albada(v1(i)%z, v2(i)%z)
  enddo

end function albada_vt


!-----------------------------------------------------------------------
function vleer(x1, x2)
implicit none
real(krp) :: x1, x2, ad1, ad2, s, vleer

  s = x1*x2
  if (s.le.0._krp) then
    vleer = 0._krp
  else
    vleer = 2._krp*s/(x1+x2)
  endif

end function vleer

!-----------------------------------------------------------------------
function vleer_t(x1, x2)
implicit none
real(krp), dimension(:)        :: x1, x2
real(krp), dimension(size(x1)) :: vleer_t

  vleer_t = 0._krp
  where ((x1 > 0._krp).and.(x2 > 0._krp)) vleer_t = 2._krp*x1*x2/(x1+x2)
  where ((x1 < 0._krp).and.(x2 < 0._krp)) vleer_t = 2._krp*x1*x2/(x1+x2)

end function vleer_t

!-----------------------------------------------------------------------
function vleer_v(v1, v2)
implicit none
type(v3d) :: v1, v2, vleer_v

  vleer_v%x = vleer(v1%x, v2%x)
  vleer_v%y = vleer(v1%y, v2%y)
  vleer_v%z = vleer(v1%z, v2%z)

end function vleer_v

!-----------------------------------------------------------------------
function vleer_vt(v1, v2)
implicit none
type(v3d), dimension(:) :: v1, v2
type(v3d), dimension(size(v1)) :: vleer_vt
integer :: i

  do i = 1, size(v1)
    vleer_vt(i)%x = vleer(v1(i)%x, v2(i)%x)
    vleer_vt(i)%y = vleer(v1(i)%y, v2(i)%y)
    vleer_vt(i)%z = vleer(v1(i)%z, v2(i)%z)
  enddo

end function vleer_vt

!-----------------------------------------------------------------------
function superbee(x1, x2)
implicit none
integer   :: ilim
real(krp) :: x1, x2, ad1, ad2, s, superbee

  superbee = 0._krp
  if ((x1 > 0._krp).and.(x2 > 0._krp)) superbee = max(min(2._krp*x1, x2), min(2._krp*x2, x1))
  if ((x1 < 0._krp).and.(x2 < 0._krp)) superbee = min(max(2._krp*x1, x2), max(2._krp*x2, x1))

end function superbee

!-----------------------------------------------------------------------
function superbee_t(x1, x2)
implicit none
real(krp), dimension(:)        :: x1, x2
real(krp), dimension(size(x1)) :: superbee_t

  superbee_t = 0._krp
  where ((x1 > 0._krp).and.(x2 > 0._krp)) superbee_t = max(min(2._krp*x1, x2), min(2._krp*x2, x1))
  where ((x1 < 0._krp).and.(x2 < 0._krp)) superbee_t = min(max(2._krp*x1, x2), max(2._krp*x2, x1))

end function superbee_t

!-----------------------------------------------------------------------
function superbee_v(v1, v2)
implicit none
type(v3d) :: v1, v2, superbee_v

  superbee_v%x = superbee(v1%x, v2%x)
  superbee_v%y = superbee(v1%y, v2%y)
  superbee_v%z = superbee(v1%z, v2%z)

end function superbee_v

!-----------------------------------------------------------------------
function superbee_vt(v1, v2)
implicit none
type(v3d), dimension(:) :: v1, v2
type(v3d), dimension(size(v1)) :: superbee_vt
integer :: i

  do i = 1, size(v1)
    superbee_vt(i)%x = superbee(v1(i)%x, v2(i)%x)
    superbee_vt(i)%y = superbee(v1(i)%y, v2(i)%y)
    superbee_vt(i)%z = superbee(v1(i)%z, v2(i)%z)
  enddo

end function superbee_vt

!-----------------------------------------------------------------------
function kim3(x1, x2)
implicit none
integer   :: ilim
real(krp) :: x1, x2, ad1, ad2, s, kim3

  kim3 = 0._krp
  if ((x1 > 0._krp).and.(x2 > 0._krp)) then
    kim3 = (2._krp*x1 + x2) / 3._krp
    kim3 = min(min(2._krp*x1, 2._krp*x2), kim3)
  endif
  if ((x1 < 0._krp).and.(x2 < 0._krp)) then
    kim3 = (2._krp*x1 + x2) / 3._krp
    kim3 = max(max(2._krp*x1, 2._krp*x2), kim3)
  endif

end function kim3

!-----------------------------------------------------------------------
function kim3_t(x1, x2)
implicit none
real(krp), dimension(:)        :: x1, x2
real(krp), dimension(size(x1)) :: kim3_t

  kim3_t = 0._krp
  where ((x1 > 0._krp).and.(x2 > 0._krp)) 
    kim3_t = (2._krp*x1 + x2) / 3._krp
    kim3_t = min(min(2._krp*x1, 2._krp*x2), kim3_t)
  endwhere
  where ((x1 < 0._krp).and.(x2 < 0._krp)) 
    kim3_t = (2._krp*x1 + x2) / 3._krp
    kim3_t = max(max(2._krp*x1, 2._krp*x2), kim3_t)
  endwhere

end function kim3_t

!-----------------------------------------------------------------------
function kim3_v(v1, v2)
implicit none
type(v3d) :: v1, v2, kim3_v

  kim3_v%x = kim3(v1%x, v2%x)
  kim3_v%y = kim3(v1%y, v2%y)
  kim3_v%z = kim3(v1%z, v2%z)

end function kim3_v

!-----------------------------------------------------------------------
function kim3_vt(v1, v2)
implicit none
type(v3d), dimension(:) :: v1, v2
type(v3d), dimension(size(v1)) :: kim3_vt
integer :: i

  do i = 1, size(v1)
    kim3_vt(i)%x = kim3(v1(i)%x, v2(i)%x)
    kim3_vt(i)%y = kim3(v1(i)%y, v2(i)%y)
    kim3_vt(i)%z = kim3(v1(i)%z, v2(i)%z)
  enddo

end function kim3_vt

!-----------------------------------------------------------------------
function lim03(x1, x2)
implicit none
real(krp) :: x1, x2, lim03, eta

  eta = (x1*x1 + x2*x2) / 0.01_krp / 0.01_krp

  lim03 = 0._krp
  if (eta < 1._krp) then
    lim03 = (2._krp*x2 + x1) / 3._krp
  elseif ((x1 > 0._krp).and.(x2 > 0._krp)) then
    lim03 = (2._krp*x2 + x1) / 3._krp
    lim03 = min(1.6_krp*x2, 2._krp*x1, lim03)
  elseif ((x1 < 0._krp).and.(x2 < 0._krp)) then
    lim03 = (2._krp*x2 + x1) / 3._krp
    lim03 = max(1.6_krp*x2, 2._krp*x1, lim03)
  elseif ((x1 < 0._krp).and.(x2 > 0._krp)) then
    lim03 = (2._krp*x2 + x1) / 3._krp
    lim03 = max(0._krp, min(-0.5_krp*x1, lim03))
  elseif ((x1 > 0._krp).and.(x2 < 0._krp)) then
    lim03 = (2._krp*x2 + x1) / 3._krp
    lim03 = min(0._krp, max(-0.5_krp*x1, lim03))
  endif

end function lim03

!-----------------------------------------------------------------------
function lim03_t(x1, x2)
implicit none
real(krp), dimension(:)        :: x1, x2
real(krp), dimension(size(x1)) :: lim03_t, eta

  eta = (x1*x1 + x2*x2) / 0.01_krp / 0.01_krp

  lim03_t = 0._krp
  where (eta < 1._krp)
    lim03_t = (2._krp*x2 + x1) / 3._krp
  elsewhere ((x1 > 0._krp).and.(x2 > 0._krp))
    lim03_t = (2._krp*x2 + x1) / 3._krp
    lim03_t = min(1.6_krp*x2, 2._krp*x1, lim03_t)
  elsewhere ((x1 < 0._krp).and.(x2 < 0._krp))
    lim03_t = (2._krp*x2 + x1) / 3._krp
    lim03_t = max(1.6_krp*x2, 2._krp*x1, lim03_t)
  elsewhere ((x1 < 0._krp).and.(x2 > 0._krp))
    lim03_t = (2._krp*x2 + x1) / 3._krp
    lim03_t = max(0._krp, min(-0.5_krp*x1, lim03_t))
  elsewhere ((x1 > 0._krp).and.(x2 < 0._krp))
    lim03_t = (2._krp*x2 + x1) / 3._krp
    lim03_t = min(0._krp, max(-0.5_krp*x1, lim03_t))
  endwhere

end function lim03_t

!-----------------------------------------------------------------------
function lim03_v(v1, v2)
implicit none
type(v3d) :: v1, v2, lim03_v

  lim03_v%x = lim03(v1%x, v2%x)
  lim03_v%y = lim03(v1%y, v2%y)
  lim03_v%z = lim03(v1%z, v2%z)

end function lim03_v

!-----------------------------------------------------------------------
function lim03_vt(v1, v2)
implicit none
type(v3d), dimension(:) :: v1, v2
type(v3d), dimension(size(v1)) :: lim03_vt
integer :: i

  do i = 1, size(v1)
    lim03_vt(i)%x = lim03(v1(i)%x, v2(i)%x)
    lim03_vt(i)%y = lim03(v1(i)%y, v2(i)%y)
    lim03_vt(i)%z = lim03(v1(i)%z, v2(i)%z)
  enddo

end function lim03_vt

!-----------------------------------------------------------------------
! MONOTONIC LIMITERS of x1, x2 between a and b
!     (a)      x1 | x2       (b)
! (0): x1 and x2 are always in [a, b] range
! (1): (0) and a, x1, x2, b variation is monotonic ; otherwise x1 and x2 set to average
! (2): (1) and |x1-a|, |x2-b| < .5*|b-a|
! Remarks: 
! - monotonic0 is close to superbee
! - monotonic2 is close to minmod
!-----------------------------------------------------------------------
subroutine monotonic0(x1, x2, a, b)
implicit none
real(krp), intent(inout) :: x1, x2   ! value to check
real(krp), intent(in)    :: a, b     ! range 
real(krp)                :: mid

  mid = 0.5_krp*(x1+x2)
  if (a < b) then
    x1 = max(a, min(x1, b))
    x2 = min(b, max(x2, a))
  else
    x1 = min(a, max(x1, b))
    x2 = max(b, min(x2, a))
  endif

endsubroutine monotonic0

subroutine monotonic1(x1, x2, a, b)
implicit none
real(krp), intent(inout) :: x1, x2   ! value to check
real(krp), intent(in)    :: a, b     ! range 
real(krp)                :: midx

  midx = 0.5_krp*(x1+x2)
  if (a < b) then
    x1 = max(a, min(x1, midx, b))
    x2 = min(b, max(x2, midx, a))
  else
    x1 = min(a, max(x1, midx, b))
    x2 = max(b, min(x2, midx, a))
  endif

endsubroutine monotonic1

!-----------------------------------------------------------------------
subroutine monotonic2(x1, x2, a, b)
implicit none
real(krp), intent(inout) :: x1, x2   ! value to check
real(krp), intent(in)    :: a, b     ! range 
real(krp)                :: midx, mid

  mid  = 0.5_krp*( a+ b)
  if (a < b) then
    x1 = max(a, min(x1, mid))
    x2 = min(b, max(x2, mid))
  else
    x1 = min(a, max(x1, mid))
    x2 = max(b, min(x2, mid))
  endif

endsubroutine monotonic2

!-----------------------------------------------------------------------
subroutine monotonic0_v(x1, x2, a, b)
implicit none
type(v3d), intent(inout) :: x1, x2   ! value to check
type(v3d), intent(in)    :: a, b     ! range 

  call monotonic0(x1%x, x2%x, a%x, b%x)
  call monotonic0(x1%y, x2%y, a%y, b%y)
  call monotonic0(x1%z, x2%z, a%z, b%z)

end subroutine monotonic0_v


!-----------------------------------------------------------------------
subroutine monotonic1_v(x1, x2, a, b)
implicit none
type(v3d), intent(inout) :: x1, x2   ! value to check
type(v3d), intent(in)    :: a, b     ! range 

  call monotonic1(x1%x, x2%x, a%x, b%x)
  call monotonic1(x1%y, x2%y, a%y, b%y)
  call monotonic1(x1%z, x2%z, a%z, b%z)

end subroutine monotonic1_v

!-----------------------------------------------------------------------
subroutine monotonic2_v(x1, x2, a, b)
implicit none
type(v3d), intent(inout) :: x1, x2   ! value to check
type(v3d), intent(in)    :: a, b     ! range 

  call monotonic2(x1%x, x2%x, a%x, b%x)
  call monotonic2(x1%y, x2%y, a%y, b%y)
  call monotonic2(x1%z, x2%z, a%z, b%z)

end subroutine monotonic2_v



endmodule LIMITER

!------------------------------------------------------------------------------!
! Modification history
!
! nov  2004 : created, limiters
! nov  2004 : limiters for vectors & arrays
! dec  2005 : new kim3 limiter (Kim & Kim, JCP 208, 2005)
!------------------------------------------------------------------------------!
