!------------------------------------------------------------------------------!
! Procedure : tvdgradstr_scal                        Authors : J. Gressier, G. Grondin
!                                                    Created : January 2007
! Fonction
!   TVD MUSCL interpolation (structured like method) for scalars
!
!------------------------------------------------------------------------------!
subroutine tvdgradstr_scal(tvdlimiter, nf, dscal, vgrad, uLR, LRsca, CF)

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
type(v3d)  :: CF(1:nf)                  ! cell (C) to face (F) vector
real(krp)  :: LRsca(1:nf)               ! scalar difference (cell to cell)

! -- OUTPUTS --
real(krp)  :: dscal(1:nf)                ! tvd CF difference

! -- Working/Internal variables --
real(krp)  :: scellgrad(1:nf)

! -- BODY --

scellgrad(:) = vgrad(:).scal.uLR(:)

select case(tvdlimiter)
case(lim_none,lim_minmax)
  scellgrad(:) = LRsca(:) - scellgrad(:)
case(lim_minmod)
  scellgrad(:) = minmod(2._krp*scellgrad(:)-LRsca(:), LRsca(:)) - scellgrad(:)
case(lim_albada)
  scellgrad(:) = albada(2._krp*scellgrad(:)-LRsca(:), LRsca(:)) - scellgrad(:)
case(lim_vleer)
  scellgrad(:) = vleer(2._krp*scellgrad(:)-LRsca(:), LRsca(:)) - scellgrad(:)
case(lim_sbee)
  scellgrad(:) = superbee(2._krp*scellgrad(:)-LRsca(:), LRsca(:)) - scellgrad(:)
case(lim_kim3)
  scellgrad(:) = kim3(LRsca(:), 2._krp*scellgrad(:)-LRsca(:)) - scellgrad(:)  ! param order !
case(lim_lim03)
  scellgrad(:) = lim03(2._krp*scellgrad(:)-LRsca(:), LRsca(:)) - scellgrad(:)  ! param order !
case default
  call erreur("high resolution","unknown limiter")
endselect

vgrad(:) = vgrad(:) + scellgrad(:)*uLR(:)
dscal(:) = vgrad(:).scal.CF(:)

endsubroutine tvdgradstr_scal

!------------------------------------------------------------------------------!
! Changes history
!
! Jan  2007 : created from hres_ns_muscl
!------------------------------------------------------------------------------!
