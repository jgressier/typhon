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




endmodule LIMITER

!------------------------------------------------------------------------------!
! Modification history
!
! nov  2004 : created, limiters
! nov  2004 : limiters for vectors & arrays
!------------------------------------------------------------------------------!
