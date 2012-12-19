!------------------------------------------------------------------------------!
! Procedure : hres_ns_gradsvm
!                                
! Fonction
!   High order Gradient SVM interpolation 
!
!------------------------------------------------------------------------------!
subroutine hres_ns_gradsvm(defspat, nf, ideb, umesh, fprim, gradL, gradR, ic0)

use TYPHMAKE
use OUTPUT
use VARCOM
use MENU_NUM
use USTMESH
use GENFIELD
use EQNS
use GEO3D
use LIMITER
use MENU_SOLVER
use MESHBASE
use DEFFIELD
use TENSOR3
use MATRIX_ARRAY

implicit none

! -- INPUTS --
type(mnu_spat)        :: defspat          ! parametres d'integration spatiale
integer               :: nf, ideb         ! face number and first index
type(st_ustmesh)      :: umesh            ! unstructured mesh definition
type(st_genericfield) :: fprim            ! primitive variables fields
integer(kip)          :: ic0              ! cell field offset


! -- OUTPUTS --
type(st_genericfield) :: gradL, gradR     ! left & right gradients

! -- Internal variables --
integer                   :: i, if, ic, isca, ivec
integer                   :: isv, icv1, icv2, ncv,icv
integer                   :: isvface
real(krp)                 :: weights1(1:defspat%svm%cv_split,1:2),weights2(1:defspat%svm%cv_split,1:2)
real(krp)                 :: metric(1:4)      

! -- BODY --
!------------------------------------------------------------------------------
! SVM Gradient SCALAR interpolation
!------------------------------------------------------------------------------
ncv = defspat%svm%cv_split
do isca = 1, fprim%nscal

  do i = 1, nf      ! indirection loop on faces (index in packet)

    if = ideb-1+i                    ! (face index)
    ic = ic0 -1+i

    ! -- left side --

    isv  = (umesh%facecell%fils(if,1)-1) / ncv + 1
    icv1 = (isv-1)*ncv + 1
    icv2 = isv*ncv
     weights1(1:ncv,1:2)=defspat%svm%grad_interp_weights(2*umesh%face_Ltag%fils(if, 1)-1, 1:ncv,1:2) !1st gauss point
     weights2(1:ncv,1:2)=defspat%svm%grad_interp_weights(2*umesh%face_Ltag%fils(if, 1)  , 1:ncv,1:2) !2nd gauss point
     metric(1:4) = umesh%mesh%metricsvm(isv:isv+3  ,1,1)

      gradL%tabvect(isca)%vect(i)%x = .5_krp * (&
        sum( weights1(1:ncv,1) * fprim%tabscal(isca)%scal(icv1:icv2))* metric(1)&
      + sum( weights1(1:ncv,2) * fprim%tabscal(isca)%scal(icv1:icv2))* metric(2)&
      + sum( weights2(1:ncv,1) * fprim%tabscal(isca)%scal(icv1:icv2))* metric(1)&
      + sum( weights2(1:ncv,2) * fprim%tabscal(isca)%scal(icv1:icv2))* metric(2))

      gradL%tabvect(isca)%vect(i)%y = .5_krp * (&
        sum( weights1(1:ncv,1)  * fprim%tabscal(isca)%scal(icv1:icv2))* metric(3)&
      + sum( weights1(1:ncv,2)  * fprim%tabscal(isca)%scal(icv1:icv2))* metric(4)&
      + sum( weights2(1:ncv,1)  * fprim%tabscal(isca)%scal(icv1:icv2))* metric(3)&
      + sum( weights2(1:ncv,2)  * fprim%tabscal(isca)%scal(icv1:icv2))* metric(4))

    ! -- right side --
    isv  = (umesh%facecell%fils(if,2)-1) / ncv + 1
    icv1 = (isv-1)*ncv + 1
    icv2 = isv*ncv

    if (umesh%face_Rtag%fils(if, 1) /= 0) then
     weights1(1:ncv,1:2)=defspat%svm%grad_interp_weights(2*umesh%face_Rtag%fils(if, 1)-1, 1:ncv,1:2) !1st gauss point
     weights2(1:ncv,1:2)=defspat%svm%grad_interp_weights(2*umesh%face_Rtag%fils(if, 1)  , 1:ncv,1:2) !2nd gauss point
     metric(1:4) = umesh%mesh%metricsvm(isv:isv+3  ,1,1)
         gradR%tabvect(isca)%vect(i)%x = .5_krp *(&
        sum( weights1(1:ncv,1)  * fprim%tabscal(isca)%scal(icv1:icv2))* metric(1)&
      + sum( weights1(1:ncv,2)  * fprim%tabscal(isca)%scal(icv1:icv2))* metric(2)&
      + sum( weights2(1:ncv,1)  * fprim%tabscal(isca)%scal(icv1:icv2))* metric(1)&
      + sum( weights2(1:ncv,2)  * fprim%tabscal(isca)%scal(icv1:icv2))* metric(2))

         gradR%tabvect(isca)%vect(i)%y = .5_krp * (&
        sum( weights1(1:ncv,1)  * fprim%tabscal(isca)%scal(icv1:icv2))* metric(3)&
      + sum( weights1(1:ncv,2)  * fprim%tabscal(isca)%scal(icv1:icv2))* metric(4)&
      + sum( weights2(1:ncv,1)  * fprim%tabscal(isca)%scal(icv1:icv2))* metric(3)&
      + sum( weights2(1:ncv,2)  * fprim%tabscal(isca)%scal(icv1:icv2))* metric(4))

      else  !! Cellules limites : Attention copie du gradient (à revoir)
         gradR%tabvect(isca)%vect(i)=gradL%tabvect(isca)%vect(i)
    endif
  enddo

enddo
!------------------------------------------------------------------------------
! SVM Gradient VECTOR interpolation
!------------------------------------------------------------------------------
do ivec = 1, fprim%nvect

  do i = 1, nf      ! indirection loop on faces (index in packet)

    if = ideb-1+i                    ! (face index)
    ic = ic0 -1+i

    ! -- left side --
    isv  = (umesh%facecell%fils(if,1)-1) / ncv + 1
    icv1 = (isv-1)*ncv + 1
    icv2 = isv*ncv
     weights1(1:ncv,1:2)=defspat%svm%grad_interp_weights(2*umesh%face_Ltag%fils(if, 1)-1, 1:ncv,1:2) !1st gauss point
     weights2(1:ncv,1:2)=defspat%svm%grad_interp_weights(2*umesh%face_Ltag%fils(if, 1)  , 1:ncv,1:2) !2nd gauss point
     metric(1:4) = umesh%mesh%metricsvm(isv:isv+3  ,1,1)


      do icv= icv1, icv2 ! boucle sur les CV du SV

      gradL%tabtens(ivec)%tens(i)%mat(1,1) = gradL%tabtens(ivec)%tens(i)%mat(1,1) +.5_krp * (&
        (weights1(icv-icv1+1,1) * fprim%tabvect(ivec)%vect(icv)%x)* metric(1)&
      + (weights1(icv-icv1+1,2) * fprim%tabvect(ivec)%vect(icv)%x)* metric(2)&
      + (weights2(icv-icv1+1,1) * fprim%tabvect(ivec)%vect(icv)%x)* metric(1)&
      + (weights2(icv-icv1+1,2) * fprim%tabvect(ivec)%vect(icv)%x)* metric(2)) 

      gradL%tabtens(ivec)%tens(i)%mat(1,2) = gradL%tabtens(ivec)%tens(i)%mat(1,2) +.5_krp * (&
        (weights1(icv-icv1+1,1) * fprim%tabvect(ivec)%vect(icv)%x)* metric(3)&
      + (weights1(icv-icv1+1,2) * fprim%tabvect(ivec)%vect(icv)%x)* metric(4)&
      + (weights2(icv-icv1+1,1) * fprim%tabvect(ivec)%vect(icv)%x)* metric(3)&
      + (weights2(icv-icv1+1,2) * fprim%tabvect(ivec)%vect(icv)%x)* metric(4))
     
      gradL%tabtens(ivec)%tens(i)%mat(2,1) = gradL%tabtens(ivec)%tens(i)%mat(2,1) +.5_krp * (&  
        (weights1(icv-icv1+1,1) * fprim%tabvect(ivec)%vect(icv)%y)* metric(1)&
      + (weights1(icv-icv1+1,2) * fprim%tabvect(ivec)%vect(icv)%y)* metric(2)&
      + (weights2(icv-icv1+1,1) * fprim%tabvect(ivec)%vect(icv)%y)* metric(1)&
      + (weights2(icv-icv1+1,2) * fprim%tabvect(ivec)%vect(icv)%y)* metric(2)) 


      gradL%tabtens(ivec)%tens(i)%mat(2,2) = gradL%tabtens(ivec)%tens(i)%mat(2,2) +.5_krp * (&
        (weights1(icv-icv1+1,1) * fprim%tabvect(ivec)%vect(icv)%y)* metric(3)&
      + (weights1(icv-icv1+1,2) * fprim%tabvect(ivec)%vect(icv)%y)* metric(4)&
      + (weights2(icv-icv1+1,1) * fprim%tabvect(ivec)%vect(icv)%y)* metric(3)&
      + (weights2(icv-icv1+1,2) * fprim%tabvect(ivec)%vect(icv)%y)* metric(4))
     
      enddo

    ! -- right side --
    isv  = (umesh%facecell%fils(if,2)-1) / ncv + 1
    icv1 = (isv-1)*ncv + 1
    icv2 = isv*ncv

    if (umesh%face_Rtag%fils(if, 1) /= 0) then
     weights1(1:ncv,1:2)=defspat%svm%grad_interp_weights(2*umesh%face_Rtag%fils(if, 1)-1, 1:ncv,1:2) !1st gauss point
     weights2(1:ncv,1:2)=defspat%svm%grad_interp_weights(2*umesh%face_Rtag%fils(if, 1)  , 1:ncv,1:2) !2nd gauss point
     metric(1:4) = umesh%mesh%metricsvm(isv:isv+3  ,1,1)

      do icv= icv1, icv2 ! boucle sur les CV du SV
      gradR%tabtens(ivec)%tens(i)%mat(1,1) = gradR%tabtens(ivec)%tens(i)%mat(1,1) +.5_krp * (&
        (weights1(icv-icv1+1,1) * fprim%tabvect(ivec)%vect(icv)%x)* metric(1)&
      + (weights1(icv-icv1+1,2) * fprim%tabvect(ivec)%vect(icv)%x)* metric(2)&
      + (weights2(icv-icv1+1,1) * fprim%tabvect(ivec)%vect(icv)%x)* metric(1)&
      + (weights2(icv-icv1+1,2) * fprim%tabvect(ivec)%vect(icv)%x)* metric(2)) 
 
      gradR%tabtens(ivec)%tens(i)%mat(1,2) = gradR%tabtens(ivec)%tens(i)%mat(1,2) +.5_krp * (&
        (weights1(icv-icv1+1,1) * fprim%tabvect(ivec)%vect(icv)%x)* metric(3)&
      + (weights1(icv-icv1+1,2) * fprim%tabvect(ivec)%vect(icv)%x)* metric(4)&
      + (weights2(icv-icv1+1,1) * fprim%tabvect(ivec)%vect(icv)%x)* metric(3)&
      + (weights2(icv-icv1+1,2) * fprim%tabvect(ivec)%vect(icv)%x)* metric(4))

      gradR%tabtens(ivec)%tens(i)%mat(2,1) = gradR%tabtens(ivec)%tens(i)%mat(2,1) +.5_krp * (&  
        (weights1(icv-icv1+1,1) * fprim%tabvect(ivec)%vect(icv)%y)* metric(1)&
      + (weights1(icv-icv1+1,2) * fprim%tabvect(ivec)%vect(icv)%y)* metric(2)&
      + (weights2(icv-icv1+1,1) * fprim%tabvect(ivec)%vect(icv)%y)* metric(1)&
      + (weights2(icv-icv1+1,2) * fprim%tabvect(ivec)%vect(icv)%y)* metric(2)) 

      gradR%tabtens(ivec)%tens(i)%mat(2,2) = gradR%tabtens(ivec)%tens(i)%mat(2,2) +.5_krp * (&
        (weights1(icv-icv1+1,1) * fprim%tabvect(ivec)%vect(icv)%y)* metric(3)&
      + (weights1(icv-icv1+1,2) * fprim%tabvect(ivec)%vect(icv)%y)* metric(4)&
      + (weights2(icv-icv1+1,1) * fprim%tabvect(ivec)%vect(icv)%y)* metric(3)&
      + (weights2(icv-icv1+1,2) * fprim%tabvect(ivec)%vect(icv)%y)* metric(4))

      enddo
      else  !! Cellules limites : Attention copie du gradient (à revoir)
         gradR%tabtens(ivec)%tens(i)=gradL%tabtens(ivec)%tens(i)
    endif

  enddo
enddo
!------------------------------------------------------------------------------

endsubroutine hres_ns_gradsvm
!------------------------------------------------------------------------------!
! Changes history
! Mar  2008 : created, generic SVM interpolation (now only applied with svm2quad
!------------------------------------------------------------------------------!
