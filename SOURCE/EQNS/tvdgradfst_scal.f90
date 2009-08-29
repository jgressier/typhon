!------------------------------------------------------------------------------!
! Procedure : tvdgradfst_scal                        Authors : J. Gressier, G. Grondin
!                                                    Created : March 2008
! Fonction
!   TVD MUSCL interpolation (fast method) for scalars
!
!------------------------------------------------------------------------------!
subroutine tvdgradfst_scal(tvdlimiter, nf, dscal, vgrad, uLR, LRsca, kCF)

use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_NUM
use GEO3D
use LIMITER

implicit none

! -- INPUTS --
character  :: tvdlimiter
integer    :: nf                        ! number of faces
type(v3d)  :: vgrad(1:nf)               ! cell gradient
type(v3d)  :: uLR(1:nf)                 ! cell to cell normalized vector
real(krp)  :: kCF(1:nf)                  ! cell (C) to face (F) vector
real(krp)  :: LRsca(1:nf)               ! scalar difference (cell to cell)

! -- OUTPUTS --
real(krp)  :: dscal(1:nf)                ! tvd CF difference

! -- Working/Internal variables --
real(krp)  :: scellgrad(1:nf)

! -- BODY --

select case(tvdlimiter)
case(lim_none)
case default
  scellgrad(:) = vgrad(:).scal.uLR(:)
endselect

select case(tvdlimiter)
case(lim_none)
  scellgrad(:) = LRsca(:)
case(lim_minmod)
  scellgrad(:) = minmod(2._krp*scellgrad(:)-LRsca(:), LRsca(:))
case(lim_albada)
  scellgrad(:) = albada(2._krp*scellgrad(:)-LRsca(:), LRsca(:))
case(lim_vleer)
  scellgrad(:) = vleer(2._krp*scellgrad(:)-LRsca(:), LRsca(:))
case(lim_sbee)
  scellgrad(:) = superbee(2._krp*scellgrad(:)-LRsca(:), LRsca(:))
case(lim_kim3)
  scellgrad(:) = kim3(LRsca(:), 2._krp*scellgrad(:)-LRsca(:))  ! param order !
case(lim_lim03)
  scellgrad(:) = lim03(2._krp*scellgrad(:)-LRsca(:), LRsca(:)) ! param order !
case default
  call erreur("high resolution","unknown limiter")
endselect

dscal(:) = scellgrad(:)*kCF(:)

endsubroutine tvdgradfst_scal

!------------------------------------------------------------------------------!
! Changes history
!
! Mar  2008 : created from hres_ns_musclfast
!------------------------------------------------------------------------------!
