!------------------------------------------------------------------------------!
! Procedure : hres_ns_muscl                          Authors : J. Gressier
!                                                    Created : July 2004
!> @brief MUSCL interpolation of primitive quantities
!
!------------------------------------------------------------------------------!
subroutine hres_ns_muscl(defspat, nf, ideb, umesh, fprim, fgrad, cell_l, cell_r, ic0)

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
integer(kip)          :: ic0              ! cell field offset

! -- OUTPUTS --
type(st_genericfield) :: cell_l, cell_r   ! champs des valeurs primitives

! -- Internal variables --
integer                   :: i, if, ic, isca, ivec
integer                   :: icl, icr
type(t3d), allocatable    :: tgradL(:), tgradR(:)
type(v3d), allocatable    :: vgradL(:), vgradR(:)
type(v3d), allocatable    :: uLR(:), LRvec(:)
type(v3d), allocatable    :: LF(:), RF(:)
real(krp), allocatable    :: dLR(:), LRsca(:)
type(st_genericfield)     :: gprimL, gprimR

! -- BODY --

allocate(uLR(nf))
allocate( LF(nf))
allocate( RF(nf))
allocate(dLR(nf))

! -- prepare geometrical data --

do i = 1, nf
  if  = ideb+i-1
  icl = umesh%facecell%fils(if,1)
  icr = umesh%facecell%fils(if,2)
  uLR(i) = umesh%mesh%centre(icr, 1, 1) - umesh%mesh%centre(icl, 1, 1)
  LF(i)  = umesh%mesh%face_center(if,1)  - umesh%mesh%centre(icl, 1, 1)
  RF(i)  = umesh%mesh%face_center(if,1)  - umesh%mesh%centre(icr, 1, 1)
  dLR(i) = abs(uLR(i))
  uLR(i) = uLR(i)/dLR(i)
enddo

call new_genfield(gprimL, nf, fprim%nscal, fprim%nvect, 0)
call new_genfield(gprimR, nf, fprim%nscal, fprim%nvect, 0)

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

do isca = 1, fprim%nscal

  do i = 1, nf      ! indirection loop
    if  = ideb+i-1
    icl = umesh%facecell%fils(if,1)
    icr = umesh%facecell%fils(if,2)
    LRsca(i)  = (fprim%tabscal(isca)%scal(icr) - fprim%tabscal(isca)%scal(icl))/dLR(i)
    vgradL(i) = fgrad%tabvect(isca)%vect(icl)
    vgradR(i) = fgrad%tabvect(isca)%vect(icr)
  enddo

  ! -- left --
  call tvdgradstr_scal(defspat%muscl%limiter, nf, gprimL%tabscal(isca)%scal, &
                       vgradL, uLR, LRsca, LF)
  ! -- right --
  call tvdgradstr_scal(defspat%muscl%limiter, nf, gprimR%tabscal(isca)%scal, &
                       vgradR, uLR, LRsca, RF)

enddo ! scalar loop

deallocate(LRsca, vgradL, vgradR)

!------------------------------------------------------------------------------
! VECTOR computations
!------------------------------------------------------------------------------
allocate(    LRvec(nf))
allocate(   tgradL(nf))
allocate(   tgradR(nf))

do ivec = 1, fprim%nvect

  do i = 1, nf      ! indirection loop
    if  = ideb+i-1
    icl = umesh%facecell%fils(if,1)
    icr = umesh%facecell%fils(if,2)
    LRvec(i)  = (fprim%tabvect(ivec)%vect(icr) - fprim%tabvect(ivec)%vect(icl))/dLR(i)
    tgradL(i) = fgrad%tabtens(ivec)%tens(icl)
    tgradR(i) = fgrad%tabtens(ivec)%tens(icr)
  enddo

  ! -- left --
  call tvdgradstr_vect(defspat%muscl%limiter, nf, gprimL%tabvect(ivec)%vect, &
                       tgradL, uLR, LRvec, LF)
  ! -- right --
  call tvdgradstr_vect(defspat%muscl%limiter, nf, gprimR%tabvect(ivec)%vect, &
                       tgradR, uLR, LRvec, RF)

enddo ! vector loop

deallocate(LRvec, tgradL, tgradR)

!------------------------------------------
! Primitive variable reconstruction
!------------------------------------------
do i = 1, nf
  if  = ideb+i-1
  ic  = ic0 +i-1
  icl = umesh%facecell%fils(if,1)
  icr = umesh%facecell%fils(if,2)
  cell_l%tabscal(1)%scal(ic) = fprim%tabscal(1)%scal(icl) + gprimL%tabscal(1)%scal(i)
  cell_r%tabscal(1)%scal(ic) = fprim%tabscal(1)%scal(icr) + gprimR%tabscal(1)%scal(i)
  cell_l%tabscal(2)%scal(ic) = fprim%tabscal(2)%scal(icl) + gprimL%tabscal(2)%scal(i)
  cell_r%tabscal(2)%scal(ic) = fprim%tabscal(2)%scal(icr) + gprimR%tabscal(2)%scal(i)
  cell_l%tabvect(1)%vect(ic) = fprim%tabvect(1)%vect(icl) + gprimL%tabvect(1)%vect(i)
  cell_r%tabvect(1)%vect(ic) = fprim%tabvect(1)%vect(icr) + gprimR%tabvect(1)%vect(i)
enddo

!------------------------------------------
deallocate(uLR, dLR, LF, RF)

call delete(gprimL)
call delete(gprimR)

endsubroutine hres_ns_muscl

!------------------------------------------------------------------------------!
! Changes history
!
! nov  2004 : created, MUSCL interpolation (from hres_ns_muscl)
!------------------------------------------------------------------------------!
