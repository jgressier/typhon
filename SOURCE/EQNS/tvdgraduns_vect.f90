!------------------------------------------------------------------------------!
! Procedure : tvdgraduns_vect                        Authors : G. Grondin, J. Gressier, G. Grondin
!                                                    Created : March 2008
! Fonction
!   TVD MUSCL interpolation (unstructured like method) for vectors
!
!------------------------------------------------------------------------------!
subroutine tvdgraduns_vect(tvdlimiter, nf, dvect, tgrad, uLR, LRvec, CF)

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
  vcellgrad(:) = minmod(vcellgrad(:), LRvec(:)) - vcellgrad(:)
case(lim_albada)
  vcellgrad(:) = albada(vcellgrad(:), LRvec(:)) - vcellgrad(:)
case(lim_vleer)
  vcellgrad(:) = vleer(vcellgrad(:), LRvec(:)) - vcellgrad(:)
case(lim_sbee)
  vcellgrad(:) = superbee(vcellgrad(:), LRvec(:)) - vcellgrad(:)
case(lim_kim3)
  vcellgrad(:) = kim3(LRvec(:), vcellgrad(:)) - vcellgrad(:)  ! param order !
case(lim_lim03)
  vcellgrad(:) = lim03(vcellgrad(:), LRvec(:)) - vcellgrad(:)  ! param order !
case default
  call erreur("high resolution","unknown limiter")
endselect

tgrad(:) = tgrad(:) + (vcellgrad(:).tens.uLR(:))
dvect(:) = tgrad(:).scal.CF(:)

endsubroutine tvdgraduns_vect

!------------------------------------------------------------------------------!
! Changes history
!
! Mar  2008 : created from hres_ns_muscluns
!------------------------------------------------------------------------------!
