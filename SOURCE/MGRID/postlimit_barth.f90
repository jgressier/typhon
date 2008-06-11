!------------------------------------------------------------------------------!
! Procedure : postlimit_barth  
!                                
! Fonction
!   Ensure monotonic variations of face values according to cell values
!
!------------------------------------------------------------------------------!
subroutine postlimit_barth(defspat, umesh, fprim, cell_l, cell_r)

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
type(st_genericfield) :: cell_l, cell_r   ! champs des valeurs primitives

! -- Internal variables --
real(krp), allocatable  :: ratio(:)   ! cell  ratio
real(krp)               :: lratio     ! local ratio
real(krp)               :: LRsca(cell_buffer)
type(v3d)               :: LRvec(cell_buffer)
real(krp), parameter    :: klim = .5_krp
integer    :: i, if, isca, ivec
integer    :: icl(cell_buffer),  icr(cell_buffer)
integer    :: buf, dimbuf, dimbuf1 ! buffer size (current, regular and first)
integer    :: ib, nblock           ! block index and number of blocks
integer    :: ista, iend           ! starting and ending index

! -- BODY --

allocate(ratio(umesh%ncell))

call calc_buffer(umesh%nface, cell_buffer, nblock, dimbuf, dimbuf1)

!------------------------------------------------------------------------------
! SCALAR computations
!------------------------------------------------------------------------------

do isca = 1, fprim%nscal

  ratio(1:umesh%ncell) = 1._krp

  ista = 1
  buf  = dimbuf1

  !------------------------------------------------------------------------------
  ! COMPUTING CONSTRAINTS

  do ib = 1, nblock

    iend = ista+buf-1

    icl(1:buf)  = umesh%facecell%fils(ista:iend, 1)
    icr(1:buf)  = umesh%facecell%fils(ista:iend, 2)

    select case(defspat%postlimiter)
    case(postlim_barth)

      LRsca(1:buf) = fprim%tabscal(isca)%scal(icr(1:buf))-fprim%tabscal(isca)%scal(icl(1:buf))

      do i = 1, buf

        if = ista-1+i

        lratio = LRsca(i)/(cell_L%tabscal(isca)%scal(if)-fprim%tabscal(isca)%scal(icl(i)))
        lratio = max(klim*lratio, 0._krp)
        ratio(icl(i)) = min(ratio(icl(i)), lratio)

        lratio = -LRsca(i)/(cell_R%tabscal(isca)%scal(if)-fprim%tabscal(isca)%scal(icr(i)))
        lratio = max(klim*lratio, 0._krp)
        ratio(icr(i)) = min(ratio(icr(i)), lratio)

      enddo
    case default
      call erreur("flux computation","unknown POST-LIMITATION method")
    endselect

    !----------------
    ! end of block

    ista = ista + buf
    buf  = dimbuf         ! tous les nblocks suivants sont de taille dimbuf

  enddo

  !------------------------------------------------------------------------------
  ! ACTUAL LIMITATION

  ista = 1
  buf  = dimbuf1

  do ib = 1, nblock

    iend = ista+buf-1

    icl(1:buf)  = umesh%facecell%fils(ista:iend, 1)
    icr(1:buf)  = umesh%facecell%fils(ista:iend, 2)

    cell_L%tabscal(isca)%scal(ista:iend) =  fprim%tabscal(isca)%scal(icl(1:buf)) &
         + ratio(icl(1:buf))*(cell_L%tabscal(isca)%scal(ista:iend) - fprim%tabscal(isca)%scal(icl(1:buf)) )
    cell_R%tabscal(isca)%scal(ista:iend) =  fprim%tabscal(isca)%scal(icr(1:buf)) &
         + ratio(icr(1:buf))*(cell_R%tabscal(isca)%scal(ista:iend) - fprim%tabscal(isca)%scal(icr(1:buf)) )

    !---------------
    ! end of block

    ista = ista + buf
    buf  = dimbuf         ! tous les nblocks suivants sont de taille dimbuf

  enddo

enddo ! scalar loop

!------------------------------------------------------------------------------
! VECTOR computations
!------------------------------------------------------------------------------

do ivec = 1, fprim%nvect

  ratio(1:umesh%ncell) = 1._krp

  ista = 1
  buf  = dimbuf1

  !------------------------------------------------------------------------------
  ! COMPUTING CONSTRAINTS

  do ib = 1, nblock

    iend = ista+buf-1

    icl(1:buf)  = umesh%facecell%fils(ista:iend, 1)
    icr(1:buf)  = umesh%facecell%fils(ista:iend, 2)

    select case(defspat%postlimiter)
    case(postlim_barth)

      do i = 1, buf
        LRvec(i) = fprim%tabvect(ivec)%vect(icr(i))-fprim%tabvect(ivec)%vect(icl(i))
      enddo

      do i = 1, buf

        if = ista-1+i

        lratio = LRvec(i)%x/(cell_L%tabvect(ivec)%vect(if)%x   -fprim%tabvect(ivec)%vect(icl(i))%x) 
        lratio = max(klim*lratio, 0._krp)
        ratio(icl(i)) = min(ratio(icl(i)), lratio)

        lratio = LRvec(i)%y/(cell_L%tabvect(ivec)%vect(if)%y   -fprim%tabvect(ivec)%vect(icl(i))%y) 
        lratio = max(klim*lratio, 0._krp)
        ratio(icl(i)) = min(ratio(icl(i)), lratio)

        lratio = LRvec(i)%z/(cell_L%tabvect(ivec)%vect(if)%z   -fprim%tabvect(ivec)%vect(icl(i))%z) 
        lratio = max(klim*lratio, 0._krp)
        ratio(icl(i)) = min(ratio(icl(i)), lratio)

        lratio = -LRvec(i)%x/(cell_R%tabvect(ivec)%vect(if)%x   -fprim%tabvect(ivec)%vect(icr(i))%x) 
        lratio = max(klim*lratio, 0._krp)
        ratio(icr(i)) = min(ratio(icr(i)), lratio)

        lratio = -LRvec(i)%y/(cell_R%tabvect(ivec)%vect(if)%y   -fprim%tabvect(ivec)%vect(icr(i))%y) 
        lratio = max(klim*lratio, 0._krp)
        ratio(icr(i)) = min(ratio(icr(i)), lratio)

        lratio = -LRvec(i)%z/(cell_R%tabvect(ivec)%vect(if)%z   -fprim%tabvect(ivec)%vect(icr(i))%z) 
        lratio = max(klim*lratio, 0._krp)
        ratio(icr(i)) = min(ratio(icr(i)), lratio)

      enddo
    case default
      call erreur("flux computation","unknown POST-LIMITATION method")
    endselect


    !----------------------------------------------------------------------
    ! end of block

    ista = ista + buf
    buf  = dimbuf         ! tous les nblocks suivants sont de taille dimbuf

  enddo

  !------------------------------------------------------------------------------
  ! ACTUAL LIMITATION
  !------------------------------------------------------------------------------

  ista = 1
  buf  = dimbuf1

  do ib = 1, nblock

    iend = ista+buf-1

    icl(1:buf)  = umesh%facecell%fils(ista:iend, 1)
    icr(1:buf)  = umesh%facecell%fils(ista:iend, 2)

    cell_L%tabvect(ivec)%vect(ista:iend) =  fprim%tabvect(ivec)%vect(icl(1:buf)) &
         + ratio(icl(1:buf))*(cell_L%tabvect(ivec)%vect(ista:iend) - fprim%tabvect(ivec)%vect(icl(1:buf)) )
    cell_R%tabvect(ivec)%vect(ista:iend) =  fprim%tabvect(ivec)%vect(icr(1:buf)) &
         + ratio(icr(1:buf))*(cell_R%tabvect(ivec)%vect(ista:iend) - fprim%tabvect(ivec)%vect(icr(1:buf)) )


    !----------------------------------------------------------------------
    ! end of block

    ista = ista + buf
    buf  = dimbuf         ! tous les nblocks suivants sont de taille dimbuf

  enddo

enddo ! vector loop


endsubroutine postlimit_barth

!------------------------------------------------------------------------------!
! Changes history
!
! Jun  2008 : created, ensure monotone variation at face, cell based limiting
!------------------------------------------------------------------------------!
