!------------------------------------------------------------------------------!
! Procedure : calc_gradface_svm
!                                
! Fonction
!   High order Gradient SVM interpolation 
!
!------------------------------------------------------------------------------!
subroutine calc_gradface_svm(defspat, umesh, fprim, gradL, gradR)

use TYPHMAKE
use OUTPUT
use PACKET
use MENU_NUM
use USTMESH
use GENFIELD
use MESHBASE
use DEFFIELD

implicit none

! -- INPUTS --
type(mnu_spat)        :: defspat          ! parametres d'integration spatiale
type(st_ustmesh)      :: umesh            ! unstructured mesh definition
type(st_genericfield) :: fprim            ! primitive variables fields

! -- OUTPUTS --
type(st_genericfield) :: gradL, gradR     ! left & right gradients

! -- Internal variables --
integer                                 :: i, if, isca, ivec
integer                                 :: isv, icv1, icv2, ncv,icv
integer                                 :: isvface
integer                                 :: ib, buf, nblock
integer, pointer                        :: ista(:), iend(:)  ! starting and ending index
real(krp), dimension(defspat%svm%ncv,2) :: weights1, weights2
real(krp)                               :: metric(1:4)      

! -- BODY --

call new_buf_index(umesh%nface, face_buffer, nblock, ista, iend)

!$OMP PARALLEL DO private(ib, buf, weights1, weights2, metric) shared (fprim, ista, iend)

do ib = 1, nblock

  buf = iend(ib)-ista(ib)+1

!------------------------------------------------------------------------------
! SVM Gradient SCALAR interpolation
!------------------------------------------------------------------------------
ncv = defspat%svm%ncv

do isca = 1, fprim%nscal
!! PERFO : test switch of both isca / iface loops
!! PERFO : factorize indirection of weights into buf arrays
  do i = 1, buf      ! indirection loop on faces (index in packet)

    if = ista(ib)-1+i                    ! (face index)

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

  do i = 1, buf      ! indirection loop on faces (index in packet)

    if = ista(ib)-1+i                    ! (face index)

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

      do icv = icv1, icv2 ! boucle sur les CV du SV
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

  !----------------------------------------------------------------------
  ! end of nblock
enddo
!$OMP END PARALLEL DO

deallocate(ista, iend)


endsubroutine calc_gradface_svm
!------------------------------------------------------------------------------!
! Changes history
! Feb  2013: gradient interpolation
!------------------------------------------------------------------------------!
