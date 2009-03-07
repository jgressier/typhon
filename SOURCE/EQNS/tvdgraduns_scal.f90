!------------------------------------------------------------------------------!
! Procedure : tvdgraduns_scal                        Authors : J. Gressier, G. Grondin
!                                                    Created : March 2008
! Fonction
!   TVD MUSCL interpolation (unstructured like method) for scalars
!
!------------------------------------------------------------------------------!
subroutine tvdgraduns_scal(tvdlimiter, nf, dscal, vgrad, uLR, LRsca, CF)

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
  scellgrad(:) = minmod(scellgrad(:), LRsca(:)) - scellgrad(:)
case(lim_albada)
  scellgrad(:) = albada(scellgrad(:), LRsca(:)) - scellgrad(:)
case(lim_vleer)
  scellgrad(:) = vleer(scellgrad(:), LRsca(:)) - scellgrad(:)
case(lim_sbee)
  scellgrad(:) = superbee(scellgrad(:), LRsca(:)) - scellgrad(:)
case(lim_kim3) !!! 3rd order is not proved for this combination of slopes !!!
  scellgrad(:) = kim3(LRsca(:), scellgrad(:)) - scellgrad(:)  ! param order !
case default
  call erreur("high resolution","unknown limiter")
endselect

vgrad(:) = vgrad(:) + scellgrad(:)*uLR(:)
dscal(:) = vgrad(:).scal.CF(:)

endsubroutine tvdgraduns_scal

!------------------------------------------------------------------------------!
! Changes history
!
! Mar  2008 : created from hres_ns_muscluns
!------------------------------------------------------------------------------!
