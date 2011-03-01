!------------------------------------------------------------------------------!
! Procedure : postlimit_monotonic  
!                                
! Fonction
!   Ensure monotonic variations of face values according to cell values
!
!------------------------------------------------------------------------------!
subroutine postlimit_monotonic(defspat, umesh, fprim, cell_l, cell_r)

use TYPHMAKE
use OUTPUT
use PACKET
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
type(st_genericfield) :: cell_l, cell_r   ! champs des valeurs primitives

! -- Internal variables --
integer    :: i, if, isca, ivec
integer    :: icl(face_buffer),  icr(face_buffer)
integer    :: buf                     ! buffer size 
integer    :: ib, nblock              ! block index and number of blocks
integer, pointer :: ista(:), iend(:)  ! starting and ending index

! -- BODY --

call new_buf_index(umesh%nface, face_buffer, nblock, ista, iend)

!$OMP PARALLEL DO private(ib, icl, icr, buf, i) shared (cell_L, cell_R, fprim, umesh, ista, iend)

do ib = 1, nblock

  buf = iend(ib)-ista(ib)+1

  icl(1:buf)  = umesh%facecell%fils(ista(ib):iend(ib), 1)
  icr(1:buf)  = umesh%facecell%fils(ista(ib):iend(ib), 2)

!------------------------------------------------------------------------------
! SCALAR computations
!------------------------------------------------------------------------------

do isca = 1, fprim%nscal

  select case(defspat%postlimiter)
  case(postlim_monotonic0)
    do i = 1, buf
      call monotonic0(cell_L%tabscal(isca)%scal(ista(ib)-1+i), cell_R%tabscal(isca)%scal(ista(ib)-1+i), &
                   fprim%tabscal(isca)%scal(icl(i)), fprim%tabscal(isca)%scal(icr(i)))

    enddo
  case(postlim_monotonic1)
    do i = 1, buf
      call monotonic1(cell_L%tabscal(isca)%scal(ista(ib)-1+i), cell_R%tabscal(isca)%scal(ista(ib)-1+i), &
                   fprim%tabscal(isca)%scal(icl(i)), fprim%tabscal(isca)%scal(icr(i)))

    enddo
  case(postlim_monotonic2)
    do i = 1, buf
      call monotonic2(cell_L%tabscal(isca)%scal(ista(ib)-1+i), cell_R%tabscal(isca)%scal(ista(ib)-1+i), &
                   fprim%tabscal(isca)%scal(icl(i)), fprim%tabscal(isca)%scal(icr(i)))

    enddo
  case default
    call erreur("flux computation","unknown POST-LIMITATION method")
  endselect

enddo ! scalar loop

!------------------------------------------------------------------------------
! VECTOR computations
!------------------------------------------------------------------------------

do ivec = 1, fprim%nvect

  select case(defspat%postlimiter)
  case(postlim_monotonic0)
    do i = 1, buf
      call monotonic0(cell_L%tabvect(ivec)%vect(ista(ib)-1+i), cell_R%tabvect(ivec)%vect(ista(ib)-1+i), &
                   fprim%tabvect(ivec)%vect(icl(i)), fprim%tabvect(ivec)%vect(icr(i)))

    enddo
  case(postlim_monotonic1)
    do i = 1, buf
      call monotonic1(cell_L%tabvect(ivec)%vect(ista(ib)-1+i), cell_R%tabvect(ivec)%vect(ista(ib)-1+i), &
                   fprim%tabvect(ivec)%vect(icl(i)), fprim%tabvect(ivec)%vect(icr(i)))

    enddo
  case(postlim_monotonic2)
    do i = 1, buf
      call monotonic2(cell_L%tabvect(ivec)%vect(ista(ib)-1+i), cell_R%tabvect(ivec)%vect(ista(ib)-1+i), &
                   fprim%tabvect(ivec)%vect(icl(i)), fprim%tabvect(ivec)%vect(icr(i)))

    enddo
  case default
    call erreur("flux computation","unknown POST-LIMITATION method")
  endselect

enddo ! vector loop

  !----------------------------------------------------------------------
  ! end of nblock
enddo
!$OMP END PARALLEL DO

endsubroutine postlimit_monotonic

!------------------------------------------------------------------------------!
! Changes history
!
! nov  2007 : created, ensure monotone variation at face
!------------------------------------------------------------------------------!
