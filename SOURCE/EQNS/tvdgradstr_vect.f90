!------------------------------------------------------------------------------!
! Procedure : tvdgradstr_vect                        Authors : J. Gressier, G. Grondin
!                                                    Created : January 2007
! Fonction
!   TVD MUSCL interpolation (structured like method) for vectors
!
!------------------------------------------------------------------------------!
subroutine tvdgradstr_vect(tvdlimiter, nf, dvect, tgrad, uLR, LRvec, CF)

use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_NUM
use GEO3D
use TENSOR3
use LIMITER

implicit none

! -- INPUTS --
character  :: tvdlimiter
integer    :: nf                        ! number of faces
type(t3d)  :: tgrad(1:nf)               ! cell gradient
type(v3d)  :: uLR(1:nf)                 ! cell to cell normalized vector
type(v3d)  :: LRvec(1:nf)               ! vector difference (cell to cell)
type(v3d)  :: CF(1:nf)                  ! cell to face vector

! -- OUTPUTS --
type(v3d)  :: dvect(1:nf)                ! tvd gradient

! -- Working/Internal variables --
type(v3d)  :: vcellgrad(1:nf)

! -- BODY --

vcellgrad(:) = tgrad(:).scal.uLR(:)

select case(tvdlimiter)
case(lim_none,lim_minmax)
  vcellgrad(:) = LRvec(:) - vcellgrad(:)
case(lim_minmod)
  vcellgrad(:) = minmod(2._krp*vcellgrad(:)-LRvec(:), LRvec(:)) - vcellgrad(:)
case(lim_albada)
  vcellgrad(:) = albada(2._krp*vcellgrad(:)-LRvec(:), LRvec(:)) - vcellgrad(:)
case(lim_vleer)
  vcellgrad(:) = vleer(2._krp*vcellgrad(:)-LRvec(:), LRvec(:)) - vcellgrad(:)
case(lim_sbee)
  vcellgrad(:) = superbee(2._krp*vcellgrad(:)-LRvec(:), LRvec(:)) - vcellgrad(:)
case(lim_kim3)
  vcellgrad(:) = kim3(LRvec(:), 2._krp*vcellgrad(:)-LRvec(:)) - vcellgrad(:)  ! param order !
case default
  call erreur("high resolution","unknown limiter")
endselect

tgrad(:) = tgrad(:) + (vcellgrad(:).tens.uLR(:))
dvect(:) = tgrad(:).scal.CF(:)

endsubroutine tvdgradstr_vect

!------------------------------------------------------------------------------!
! Changes history
!
! Jan  2007 : created from hres_ns_muscl
!------------------------------------------------------------------------------!
