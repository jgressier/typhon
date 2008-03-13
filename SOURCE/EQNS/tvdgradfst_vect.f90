!------------------------------------------------------------------------------!
! Procedure : tvdgradfst_vect                        Authors : J. Gressier, G. Grondin
!                                                    Created : March 2008
! Fonction
!   TVD MUSCL interpolation (fast method) for vectors
!
!------------------------------------------------------------------------------!
subroutine tvdgradfst_vect(tvdlimiter, nf, dvect, tgrad, uLR, LRvec, kCF)

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
real(krp)  :: kCF(1:nf)                  ! cell to face vector

! -- OUTPUTS --
type(v3d)  :: dvect(1:nf)                ! tvd gradient

! -- Working/Internal variables --
type(v3d)  :: vcellgrad(1:nf)

! -- BODY --

select case(tvdlimiter)
case(lim_none)
case default
  vcellgrad(:) = tgrad(:).scal.uLR(:)
endselect

select case(tvdlimiter)
case(lim_none)
  vcellgrad(:) = LRvec(:)
case(lim_minmod)
  vcellgrad(:) = minmod(2._krp*vcellgrad(:)-LRvec(:), LRvec(:))
case(lim_albada)
  vcellgrad(:) = albada(2._krp*vcellgrad(:)-LRvec(:), LRvec(:))
case(lim_vleer)
  vcellgrad(:) = vleer(2._krp*vcellgrad(:)-LRvec(:), LRvec(:))
case(lim_sbee)
  vcellgrad(:) = superbee(2._krp*vcellgrad(:)-LRvec(:), LRvec(:))
case(lim_kim3)
  vcellgrad(:) = kim3(LRvec(:), 2._krp*vcellgrad(:)-LRvec(:))  ! param order !
case default
  call erreur("high resolution","unknown limiter")
endselect

dvect(:) = kCF(:)*vcellgrad(:)

endsubroutine tvdgradfst_vect

!------------------------------------------------------------------------------!
! Changes history
!
! Mar  2008 : created from hres_ns_musclfast
!------------------------------------------------------------------------------!
