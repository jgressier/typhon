!------------------------------------------------------------------------------!
! Procedure : hres_ns_svm
!                                
! Fonction
!   SVM interpolation of primitive quantities
!
!------------------------------------------------------------------------------!
subroutine hres_ns_svm(defspat, nf, ideb, umesh, fprim, cell_l, cell_r)

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
type(st_genericfield) :: fprim            ! primitive variables fields

! -- OUTPUTS --
type(st_genericfield) :: cell_l, cell_r   ! champs des valeurs primitives

! -- Internal variables --
integer                   :: i, if, isca, ivec, ig
integer                   :: isv, icv1, icv2, ncv
integer                   :: isvface
real(krp)                 :: weights(1:defspat%svm%cv_split)

! -- BODY --

!!!!!! CANNOT HANDLE MULTIPLE GAUSS POINTS !!!!!!!!

ncv = defspat%svm%cv_split
!print*,'SVM packet',ideb,nf, ncv
!------------------------------------------------------------------------------
! SCALAR interpolation
!------------------------------------------------------------------------------
do isca = 1, fprim%nscal

  do i = 1, nf      ! indirection loop on faces (index in packet)

    if  = ideb+i-1                    ! (face index)

    ! -- left side --

    isv  = (umesh%facecell%fils(if,1)-1) / ncv + 1
    icv1 = (isv-1)*ncv + 1
    icv2 = isv*ncv
    do ig = 1, defspat%svm%nb_facepoints     ! loop on gauss points
      !print*,'interp',defspat%svm%interp_weights(umesh%face_Ltag%fils(if, ig), 1:ncv)
      weights(1:ncv) = defspat%svm%interp_weights(umesh%face_Ltag%fils(if, ig), 1:ncv)
      !print*,'  loop',umesh%facecell%fils(if,1),isv,icv1,icv2
      cell_l%tabscal(isca)%scal(i) = sum(weights(1:ncv) * fprim%tabscal(isca)%scal(icv1:icv2))
      !print*,'scal',isca,umesh%facecell%fils(if,1),cell_l%tabscal(isca)%scal(i)
      !print*,weights(1:ncv), fprim%tabscal(isca)%scal(icv1:icv2)
    enddo

    ! -- right side --

    isv  = (umesh%facecell%fils(if,2)-1) / ncv + 1
    icv1 = (isv-1)*ncv + 1
    icv2 = isv*ncv

    if (umesh%face_Rtag%fils(if, 1) /= 0) then
       do ig = 1, defspat%svm%nb_facepoints     ! loop on gauss points
         weights(1:ncv) = defspat%svm%interp_weights(umesh%face_Rtag%fils(if, ig), 1:ncv)
         cell_r%tabscal(isca)%scal(i) = sum(weights(1:ncv) * fprim%tabscal(isca)%scal(icv1:icv2))
       enddo
    else
      cell_r%tabscal(isca)%scal(i) = fprim%tabscal(isca)%scal(umesh%facecell%fils(if,2))
    endif
  enddo

enddo

!------------------------------------------------------------------------------
! VECTOR interpolation
!------------------------------------------------------------------------------
do ivec = 1, fprim%nvect

  do i = 1, nf      ! indirection loop on faces (index in packet)

    if  = ideb+i-1                    ! (face index)

    !print*,if, umesh%facecell%fils(if,:), umesh%face_Ltag%fils(if, 1), umesh%face_Rtag%fils(if, 1)
    !print*,if, umesh%facevtex%fils(if,1:2)

    ! -- left side --

    isv  = (umesh%facecell%fils(if,1)-1) / ncv + 1
    icv1 = (isv-1)*ncv + 1
    icv2 = isv*ncv

    do ig = 1, defspat%svm%nb_facepoints     ! loop on gauss points
      weights(1:ncv) = defspat%svm%interp_weights(umesh%face_Ltag%fils(if, ig), 1:ncv)
      cell_l%tabvect(ivec)%vect(i) = sum(weights(1:ncv) * fprim%tabvect(ivec)%vect(icv1:icv2))
    enddo

    ! -- right side --

    isv  = (umesh%facecell%fils(if,2)-1) / ncv + 1
    icv1 = (isv-1)*ncv + 1
    icv2 = isv*ncv

    if (umesh%face_Rtag%fils(if, 1) /= 0) then
       do ig = 1, defspat%svm%nb_facepoints     ! loop on gauss points
         weights(1:ncv) = defspat%svm%interp_weights(umesh%face_Rtag%fils(if, ig), 1:ncv)
         cell_r%tabvect(ivec)%vect(i) = sum(weights(1:ncv) * fprim%tabvect(ivec)%vect(icv1:icv2))
       enddo
    else
      cell_r%tabvect(ivec)%vect(i) = fprim%tabvect(ivec)%vect(umesh%facecell%fils(if,2))
    endif

  enddo

enddo


endsubroutine hres_ns_svm

!------------------------------------------------------------------------------!
! Changes history
! Mar  2008 : created, generic SVM interpolation (now only applied with svm2quad
!------------------------------------------------------------------------------!
