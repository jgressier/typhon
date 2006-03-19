!------------------------------------------------------------------------------!
! Procedure : hres_ns_musclfast                      Authors : J. Gressier
!                                                    Created : October 2005
! Fonction
!   MUSCL interpolation of primitive quantities
!
!
!------------------------------------------------------------------------------!
subroutine hres_ns_musclfast(defspat, nf, ideb, umesh, fprim, fgrad, cell_l, cell_r)

use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_NUM
use USTMESH
use GENFIELD
use EQNS
use GEO3D
use LIMITER

implicit none

! -- INPUTS --
type(mnu_spat)        :: defspat          ! parametres d'integration spatiale
integer               :: nf, ideb         ! face number and first index
type(st_ustmesh)      :: umesh            ! unstructured mesh definition
type(st_genericfield) :: fprim, fgrad     ! primitive variables & gradients fields

! -- OUTPUTS --
type(st_nsetat)       :: cell_l, cell_r   ! champs des valeurs primitives

! -- Internal Variables --
integer                   :: i, if, isca, ivec
integer                   :: icl, icr
type(t3d), allocatable    :: tgradL(:), tgradR(:)
type(v3d), allocatable    :: vgradL(:), vgradR(:)
type(v3d), allocatable    :: uLR(:), LRvec(:)
real(krp), allocatable    :: dLR(:), LRsca(:)
type(v3d), allocatable    :: vcellgrad(:)
real(krp), allocatable    :: kLF(:), kRF(:), scellgrad(:)
type(st_genericfield)     :: gprimL, gprimR
real(krp)                 :: g, ig1, sl, sr, iks, kl, kr, ku
real(krp)                 :: am, al, ar, vm, vnl, vnr, rel, rer

! -- BODY --

allocate(uLR(nf))
allocate(kLF(nf))
allocate(kRF(nf))
allocate(dLR(nf))

! -- prepare geometrical data --

do i = 1, nf
  if  = ideb+i-1
  icl = umesh%facecell%fils(if,1)
  icr = umesh%facecell%fils(if,2)
  uLR(i) = umesh%mesh%centre(icr, 1, 1) - umesh%mesh%centre(icl, 1, 1)
  kLF(i) =  abs(umesh%mesh%iface(if,1,1)%centre  - umesh%mesh%centre(icl, 1, 1))
  kRF(i) = -abs(umesh%mesh%iface(if,1,1)%centre  - umesh%mesh%centre(icr, 1, 1))
  dLR(i) = abs(uLR(i))
  dLR(i) = kLF(i)-kRF(i)
  uLR(i) = uLR(i)/dLR(i)
enddo

call new(gprimL, nf, fprim%nscal, fprim%nvect, 0)
call new(gprimR, nf, fprim%nscal, fprim%nvect, 0)

!------------------------------------------------------------------------------
! for each side of a face
! 1. computation of cell gradient along LR
! 2. computation of "face" gradient between L & R centers
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! SCALAR computations
!------------------------------------------------------------------------------
allocate(    LRsca(nf))
allocate(   vgradL(nf))
allocate(   vgradR(nf))
allocate(scellgrad(nf))

do isca = 1, fprim%nscal

  do i = 1, nf      ! indirection loop
    if  = ideb+i-1
    icl = umesh%facecell%fils(if,1)
    icr = umesh%facecell%fils(if,2)
    LRsca(i)  = (fprim%tabscal(isca)%scal(icr) - fprim%tabscal(isca)%scal(icl))/dLR(i)
    vgradL(i) = fgrad%tabvect(isca)%vect(icl)
    vgradR(i) = fgrad%tabvect(isca)%vect(icr)
  enddo

  select case(defspat%muscl%limiter)
  case(lim_none)
    ! -- left --
    gprimL%tabscal(isca)%scal(:) = LRsca(:)*kLF(:)
    ! -- right --
    gprimR%tabscal(isca)%scal(:) = LRsca(:)*kRF(:)
  case(lim_minmod)
    ! -- left --
    scellgrad(:) = vgradL(:).scal.uLR(:)
    scellgrad(:) = minmod(2._krp*scellgrad(:)-LRsca(:), LRsca(:))
    gprimL%tabscal(isca)%scal(:) = scellgrad(:)*kLF(:)
    ! -- right --
    scellgrad(:) = vgradR(:).scal.uLR(:)
    scellgrad(:) = minmod(2._krp*scellgrad(:)-LRsca(:), LRsca(:))
    gprimR%tabscal(isca)%scal(:) = scellgrad(:)*kRF(:)
  case(lim_albada)
    ! -- left --
    scellgrad(:) = vgradL(:).scal.uLR(:)
    scellgrad(:) = albada(2._krp*scellgrad(:)-LRsca(:), LRsca(:))
    gprimL%tabscal(isca)%scal(:) = scellgrad(:)*kLF(:)
    ! -- right --
    scellgrad(:) = vgradR(:).scal.uLR(:)
    scellgrad(:) = albada(2._krp*scellgrad(:)-LRsca(:), LRsca(:))
    gprimR%tabscal(isca)%scal(:) = scellgrad(:)*kRF(:)
  case(lim_vleer)
    ! -- left --
    scellgrad(:) = vgradL(:).scal.uLR(:)
    scellgrad(:) = vleer(2._krp*scellgrad(:)-LRsca(:), LRsca(:))
    gprimL%tabscal(isca)%scal(:) = scellgrad(:)*kLF(:)
    ! -- right --
    scellgrad(:) = vgradR(:).scal.uLR(:)
    scellgrad(:) = vleer(2._krp*scellgrad(:)-LRsca(:), LRsca(:))
    gprimR%tabscal(isca)%scal(:) = scellgrad(:)*kRF(:)
  case(lim_sbee)
    ! -- left --
    scellgrad(:) = vgradL(:).scal.uLR(:)
    scellgrad(:) = superbee(2._krp*scellgrad(:)-LRsca(:), LRsca(:))
    gprimL%tabscal(isca)%scal(:) = scellgrad(:)*kLF(:)
    ! -- right --
    scellgrad(:) = vgradR(:).scal.uLR(:)
    scellgrad(:) = superbee(2._krp*scellgrad(:)-LRsca(:), LRsca(:))
    gprimR%tabscal(isca)%scal(:) = scellgrad(:)*kRF(:)
  case(lim_kim3)
    ! -- left --
    scellgrad(:) = vgradL(:).scal.uLR(:)
    scellgrad(:) = kim3(LRsca(:), 2._krp*scellgrad(:)-LRsca(:))  ! param order !
    gprimL%tabscal(isca)%scal(:) = scellgrad(:)*kLF(:)
    ! -- right --
    scellgrad(:) = vgradR(:).scal.uLR(:)
    scellgrad(:) = kim3(LRsca(:), 2._krp*scellgrad(:)-LRsca(:))  ! param order !
    gprimR%tabscal(isca)%scal(:) = scellgrad(:)*kRF(:)
  case default
    call erreur("high resolution","unknown limiter")
  endselect
  
enddo ! scalar loop

deallocate(LRsca, vgradL, vgradR, scellgrad)

!------------------------------------------------------------------------------
! VECTOR computations
!------------------------------------------------------------------------------
allocate(    LRvec(nf))
allocate(   tgradL(nf))
allocate(   tgradR(nf))
allocate(vcellgrad(nf))

do ivec = 1, fprim%nvect

  do i = 1, nf      ! indirection loop
    if  = ideb+i-1
    icl = umesh%facecell%fils(if,1)
    icr = umesh%facecell%fils(if,2)
    LRvec(i)  = (fprim%tabvect(ivec)%vect(icr) - fprim%tabvect(ivec)%vect(icl))/dLR(i)
    tgradL(i) = fgrad%tabtens(ivec)%tens(icl)
    tgradR(i) = fgrad%tabtens(ivec)%tens(icr)
  enddo

  select case(defspat%muscl%limiter)
  case(lim_none)
    ! -- left --
    gprimL%tabvect(ivec)%vect(:) = kLF(:)*LRvec(:)
    ! -- right --
    gprimR%tabvect(ivec)%vect(:) = kRF(:)*LRvec(:)
  case(lim_minmod)
    ! -- left --
    vcellgrad(:) = tgradL(:).scal.uLR(:)
    vcellgrad(:) = minmod(2._krp*vcellgrad(:)-LRvec(:), LRvec(:))
    gprimL%tabvect(ivec)%vect(:) = kLF(:)*vcellgrad(:)
    ! -- right --
    vcellgrad(:) = tgradR(:).scal.uLR(:)
    vcellgrad(:) = minmod(2._krp*vcellgrad(:)-LRvec(:), LRvec(:))
    gprimR%tabvect(ivec)%vect(:) = kRF(:)*vcellgrad(:)
  case(lim_albada)
    ! -- left --
    vcellgrad(:) = tgradL(:).scal.uLR(:)
    vcellgrad(:) = albada(2._krp*vcellgrad(:)-LRvec(:), LRvec(:))
    gprimL%tabvect(ivec)%vect(:) = kLF(:)*vcellgrad(:)
    ! -- right --
    vcellgrad(:) = tgradR(:).scal.uLR(:)
    vcellgrad(:) = albada(2._krp*vcellgrad(:)-LRvec(:), LRvec(:))
    gprimR%tabvect(ivec)%vect(:) = kRF(:)*vcellgrad(:)
  case(lim_vleer)
    ! -- left --
    vcellgrad(:) = tgradL(:).scal.uLR(:)
    vcellgrad(:) = vleer(2._krp*vcellgrad(:)-LRvec(:), LRvec(:))
    gprimL%tabvect(ivec)%vect(:) = kLF(:)*vcellgrad(:)
    ! -- right --
    vcellgrad(:) = tgradR(:).scal.uLR(:)
    vcellgrad(:) = vleer(2._krp*vcellgrad(:)-LRvec(:), LRvec(:))
    gprimR%tabvect(ivec)%vect(:) = kRF(:)*vcellgrad(:)
  case(lim_sbee)
    ! -- left --
    vcellgrad(:) = tgradL(:).scal.uLR(:)
    vcellgrad(:) = superbee(2._krp*vcellgrad(:)-LRvec(:), LRvec(:))
    gprimL%tabvect(ivec)%vect(:) = kLF(:)*vcellgrad(:)
    ! -- right --
    vcellgrad(:) = tgradR(:).scal.uLR(:)
    vcellgrad(:) = superbee(2._krp*vcellgrad(:)-LRvec(:), LRvec(:))
    gprimR%tabvect(ivec)%vect(:) = kRF(:)*vcellgrad(:)
  case(lim_kim3)
    ! -- left --
    vcellgrad(:) = tgradL(:).scal.uLR(:)
    vcellgrad(:) = kim3(LRvec(:), 2._krp*vcellgrad(:)-LRvec(:))  ! param order !
    gprimL%tabvect(ivec)%vect(:) = kLF(:)*vcellgrad(:)
    ! -- right --
    vcellgrad(:) = tgradR(:).scal.uLR(:)
    vcellgrad(:) = kim3(LRvec(:), 2._krp*vcellgrad(:)-LRvec(:))  ! param order !
    gprimR%tabvect(ivec)%vect(:) = kRF(:)*vcellgrad(:)
  case default
    call erreur("high resolution","unknown limiter")
  endselect
  
enddo ! vector loop

deallocate(LRvec, tgradL, tgradR, vcellgrad)

!------------------------------------------
!------------------------------------------
do i = 1, nf
  if  = ideb+i-1
  icl = umesh%facecell%fils(if,1)
  icr = umesh%facecell%fils(if,2)
  cell_l%density(i)  = fprim%tabscal(1)%scal(icl) + gprimL%tabscal(1)%scal(i)
  cell_r%density(i)  = fprim%tabscal(1)%scal(icr) + gprimR%tabscal(1)%scal(i)
  cell_l%pressure(i) = fprim%tabscal(2)%scal(icl) + gprimL%tabscal(2)%scal(i)
  cell_r%pressure(i) = fprim%tabscal(2)%scal(icr) + gprimR%tabscal(2)%scal(i)
  cell_l%velocity(i) = fprim%tabvect(1)%vect(icl) + gprimL%tabvect(1)%vect(i)
  cell_r%velocity(i) = fprim%tabvect(1)%vect(icr) + gprimR%tabvect(1)%vect(i)
enddo

!------------------------------------------
deallocate(uLR, dLR, kLF, kRF)

call delete(gprimL)
call delete(gprimR)

endsubroutine hres_ns_musclfast

!------------------------------------------------------------------------------!
! Changes history
!
! Oct  2005 : created, from hres_ns_muscl
!------------------------------------------------------------------------------!
