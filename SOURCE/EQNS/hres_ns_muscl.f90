!------------------------------------------------------------------------------!
! Procedure : hres_ns_muscl               Auteur : J. Gressier
!                                         Date   : Nov 2004
! Fonction                                Modif  : (cf historique)
!   MUSCL interpolation of states
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine hres_ns_muscl(defspat, nf, ideb, umesh, fprim, fgrad, cell_l, cell_r)

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

! -- Declaration des entrees --
type(mnu_spat)        :: defspat          ! parametres d'integration spatiale
integer               :: nf, ideb         ! face number and first index
type(st_ustmesh)      :: umesh            ! unstructured mesh definition
type(st_genericfield) :: fprim, fgrad     ! primitive variables & gradients fields

! -- Declaration des sorties --
type(st_nsetat), dimension(1:nf) &
                      :: cell_l, cell_r   ! champs des valeurs primitives

! -- Declaration des variables internes --
integer                   :: i, if, isca, ivec
integer                   :: icl, icr
type(t3d), allocatable    :: tgradL(:), tgradR(:)
type(v3d), allocatable    :: vgradL(:), vgradR(:)
type(v3d), allocatable    :: uLR(:), LF(:), RF(:), LRvec(:)
real(krp), allocatable    :: dLR(:), LRsca(:)
type(v3d), allocatable    :: vcellgrad(:)
real(krp), allocatable    :: scellgrad(:)
type(st_genericfield)     :: gprimL, gprimR
real(krp)                 :: g, ig1, sl, sr, iks, kl, kr, ku
real(krp)                 :: am, al, ar, vm, vnl, vnr, rel, rer


! -- Debut de la procedure --

allocate(  uLR(nf))
allocate(   LF(nf))
allocate(   RF(nf))
allocate(  dLR(nf))

! -- prepare geometrical data --

do i = 1, nf
  if  = ideb+i-1
  icl = umesh%facecell%fils(if,1)
  icr = umesh%facecell%fils(if,2)
  uLR(i) = umesh%mesh%centre(icr, 1, 1) - umesh%mesh%centre(icl, 1, 1)
  LF(i)  = umesh%mesh%iface(if,1,1)%centre  - umesh%mesh%centre(icl, 1, 1)
  RF(i)  = umesh%mesh%iface(if,1,1)%centre  - umesh%mesh%centre(icr, 1, 1)
  dLR(i) = abs(uLR(i))
  uLR(i) = uLR(i)/dLR(i)
enddo

call new(gprimL, nf, fprim%nscal, fprim%nvect, 0)
call new(gprimR, nf, fprim%nscal, fprim%nvect, 0)

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
    scellgrad(:) = vgradL(:).scal.uLR(:)
    scellgrad(:) = LRsca(:) - scellgrad(:)
    vgradL   (:) = vgradL(:) + scellgrad(:)*uLR(:)
    gprimL%tabscal(isca)%scal(:) = vgradL(:).scal.LF(:)
    ! -- right --
    scellgrad(:) = vgradR(:).scal.uLR(:)
    scellgrad(:) = LRsca(:) - scellgrad(:)
    vgradR   (:) = vgradR(:) + scellgrad(:)*uLR(:)
    gprimR%tabscal(isca)%scal(:) = vgradR(:).scal.RF(:)
  case(lim_minmod)
    ! -- left --
    scellgrad(:) = vgradL(:).scal.uLR(:)
    scellgrad(:) = minmod(2._krp*scellgrad(:)-LRsca(:), LRsca(:)) - scellgrad(:)
    vgradL   (:) = vgradL(:) + scellgrad(:)*uLR(:)
    gprimL%tabscal(isca)%scal(:) = vgradL(:).scal.LF(:)
    ! -- right --
    scellgrad(:) = vgradR(:).scal.uLR(:)
    scellgrad(:) = minmod(2._krp*scellgrad(:)-LRsca(:), LRsca(:)) - scellgrad(:)
    vgradR   (:) = vgradR(:) + scellgrad(:)*uLR(:)
    gprimR%tabscal(isca)%scal(:) = vgradR(:).scal.RF(:)
  case(lim_albada)
    ! -- left --
    scellgrad(:) = vgradL(:).scal.uLR(:)
    scellgrad(:) = albada(scellgrad(:), LRsca(:)) - scellgrad(:)
    vgradL   (:) = vgradL(:) + scellgrad(:)*uLR(:)
    gprimL%tabscal(isca)%scal(:) = vgradL(:).scal.LF(:)
    ! -- right --
    scellgrad(:) = vgradR(:).scal.uLR(:)
    scellgrad(:) = albada(scellgrad(:), LRsca(:)) - scellgrad(:)
    vgradR   (:) = vgradR(:) + scellgrad(:)*uLR(:)
    gprimR%tabscal(isca)%scal(:) = vgradR(:).scal.RF(:)
  case(lim_vleer)
    ! -- left --
    scellgrad(:) = vgradL(:).scal.uLR(:)
    scellgrad(:) = vleer(scellgrad(:), LRsca(:)) - scellgrad(:)
    vgradL   (:) = vgradL(:) + scellgrad(:)*uLR(:)
    gprimL%tabscal(isca)%scal(:) = vgradL(:).scal.LF(:)
    ! -- right --
    scellgrad(:) = vgradR(:).scal.uLR(:)
    scellgrad(:) = vleer(scellgrad(:), LRsca(:)) - scellgrad(:)
    vgradR   (:) = vgradR(:) + scellgrad(:)*uLR(:)
    gprimR%tabscal(isca)%scal(:) = vgradR(:).scal.RF(:)
  case(lim_sbee)
    ! -- left --
    scellgrad(:) = vgradL(:).scal.uLR(:)
    scellgrad(:) = superbee(scellgrad(:), LRsca(:)) - scellgrad(:)
    vgradL   (:) = vgradL(:) + scellgrad(:)*uLR(:)
    gprimL%tabscal(isca)%scal(:) = vgradL(:).scal.LF(:)
    ! -- right --
    scellgrad(:) = vgradR(:).scal.uLR(:)
    scellgrad(:) = superbee(scellgrad(:), LRsca(:)) - scellgrad(:)
    vgradR   (:) = vgradR(:) + scellgrad(:)*uLR(:)
    gprimR%tabscal(isca)%scal(:) = vgradR(:).scal.RF(:)
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
    vcellgrad(:) = tgradL(:).scal.uLR(:)
    vcellgrad(:) = LRvec(:) - vcellgrad(:) 
    tgradL   (:) = tgradL(:) + (vcellgrad(:).tens.uLR(:))
    gprimL%tabvect(ivec)%vect(:) = tgradL(:).scal.LF(:)
    ! -- right --
    vcellgrad(:) = tgradR(:).scal.uLR(:)
    vcellgrad(:) = LRvec(:) - vcellgrad(:) 
    tgradR   (:) = tgradR(:) + (vcellgrad(:).tens.uLR(:))
    gprimR%tabvect(ivec)%vect(:) = tgradR(:).scal.RF(:)
  case(lim_minmod)
    ! -- left --
    vcellgrad(:) = tgradL(:).scal.uLR(:)
    vcellgrad(:) = minmod(vcellgrad(:), LRvec(:)) - vcellgrad(:) 
    tgradL   (:) = tgradL(:) + (vcellgrad(:).tens.uLR(:))
    gprimL%tabvect(ivec)%vect(:) = tgradL(:).scal.LF(:)
    ! -- right --
    vcellgrad(:) = tgradR(:).scal.uLR(:)
    vcellgrad(:) = minmod(vcellgrad(:), LRvec(:)) - vcellgrad(:) 
    tgradR   (:) = tgradR(:) + (vcellgrad(:).tens.uLR(:))
    gprimR%tabvect(ivec)%vect(:) = tgradR(:).scal.RF(:)
  case(lim_albada)
    ! -- left --
    vcellgrad(:) = tgradL(:).scal.uLR(:)
    vcellgrad(:) = albada(vcellgrad(:), LRvec(:)) - vcellgrad(:) 
    tgradL   (:) = tgradL(:) + (vcellgrad(:).tens.uLR(:))
    gprimL%tabvect(ivec)%vect(:) = tgradL(:).scal.LF(:)
    ! -- right --
    vcellgrad(:) = tgradR(:).scal.uLR(:)
    vcellgrad(:) = albada(vcellgrad(:), LRvec(:)) - vcellgrad(:) 
    tgradR   (:) = tgradR(:) + (vcellgrad(:).tens.uLR(:))
    gprimR%tabvect(ivec)%vect(:) = tgradR(:).scal.RF(:)
  case(lim_vleer)
    ! -- left --
    vcellgrad(:) = tgradL(:).scal.uLR(:)
    vcellgrad(:) = vleer(vcellgrad(:), LRvec(:)) - vcellgrad(:) 
    tgradL   (:) = tgradL(:) + (vcellgrad(:).tens.uLR(:))
    gprimL%tabvect(ivec)%vect(:) = tgradL(:).scal.LF(:)
    ! -- right --
    vcellgrad(:) = tgradR(:).scal.uLR(:)
    vcellgrad(:) = vleer(vcellgrad(:), LRvec(:)) - vcellgrad(:) 
    tgradR   (:) = tgradR(:) + (vcellgrad(:).tens.uLR(:))
    gprimR%tabvect(ivec)%vect(:) = tgradR(:).scal.RF(:)
  case(lim_sbee)
    ! -- left --
    vcellgrad(:) = tgradL(:).scal.uLR(:)
    vcellgrad(:) = superbee(vcellgrad(:), LRvec(:)) - vcellgrad(:) 
    tgradL   (:) = tgradL(:) + (vcellgrad(:).tens.uLR(:))
    gprimL%tabvect(ivec)%vect(:) = tgradL(:).scal.LF(:)
    ! -- right --
    vcellgrad(:) = tgradR(:).scal.uLR(:)
    vcellgrad(:) = superbee(vcellgrad(:), LRvec(:)) - vcellgrad(:) 
    tgradR   (:) = tgradR(:) + (vcellgrad(:).tens.uLR(:))
    gprimR%tabvect(ivec)%vect(:) = tgradR(:).scal.RF(:)
  case default
    call erreur("high resolution","unknown limiter")
  endselect
  
enddo ! vector loop

deallocate(LRvec, tgradL, tgradR, vcellgrad)

!------------------------------------------
!------------------------------------------
do i = 1, nf
  !print*, 'dens', gprimL%tabscal(1)%scal(i), gprimR%tabscal(1)%scal(i)
  !print*, 'pres', gprimL%tabscal(2)%scal(i), gprimR%tabscal(2)%scal(i)
  if  = ideb+i-1
  icl = umesh%facecell%fils(if,1)
  icr = umesh%facecell%fils(if,2)
  cell_l(i)%density  = fprim%tabscal(1)%scal(icl) + gprimL%tabscal(1)%scal(i)
  cell_r(i)%density  = fprim%tabscal(1)%scal(icr) + gprimR%tabscal(1)%scal(i)
  cell_l(i)%pressure = fprim%tabscal(2)%scal(icl) + gprimL%tabscal(2)%scal(i)
  cell_r(i)%pressure = fprim%tabscal(2)%scal(icr) + gprimR%tabscal(2)%scal(i)
  cell_l(i)%velocity = fprim%tabvect(1)%vect(icl) + gprimL%tabvect(1)%vect(i)
  cell_r(i)%velocity = fprim%tabvect(1)%vect(icr) + gprimR%tabvect(1)%vect(i)
enddo

!------------------------------------------
deallocate(uLR, dLR, LF, RF)

call delete(gprimL)
call delete(gprimR)

endsubroutine hres_ns_muscl

!------------------------------------------------------------------------------!
! Changes history
!
! nov  2004 : created, MUSCL interpolation
!------------------------------------------------------------------------------!
