!------------------------------------------------------------------------------!
! Procedure : postlimit_monotonic  
!                                
! Fonction
!   Ensure monotonic variations of face values according to cell values
!
!------------------------------------------------------------------------------!
subroutine postlimit_monotonic(defspat, nf, ideb, umesh, fprim, cell_l, cell_r)

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
integer                   :: i, if, isca, ivec
integer                   :: icl(nf),  icr(nf)
real(krp)                 :: smin(nf), smax(nf)

! -- BODY --

icl(1:nf)  = umesh%facecell%fils(ideb:ideb-1+nf, 1)
icr(1:nf)  = umesh%facecell%fils(ideb:ideb-1+nf, 2)

!------------------------------------------------------------------------------
! SCALAR computations
!------------------------------------------------------------------------------

do isca = 1, fprim%nscal

  select case(defspat%postlimiter)
  case(postlim_monotonic0)
    do i = 1, nf
      call monotonic0(cell_L%tabscal(isca)%scal(ideb-1+i), cell_R%tabscal(isca)%scal(ideb-1+i), &
                   fprim%tabscal(isca)%scal(icl(i)), fprim%tabscal(isca)%scal(icr(i)))

    enddo
  case(postlim_monotonic1)
    do i = 1, nf
      call monotonic1(cell_L%tabscal(isca)%scal(ideb-1+i), cell_R%tabscal(isca)%scal(ideb-1+i), &
                   fprim%tabscal(isca)%scal(icl(i)), fprim%tabscal(isca)%scal(icr(i)))

    enddo
  case(postlim_monotonic2)
    do i = 1, nf
      call monotonic2(cell_L%tabscal(isca)%scal(ideb-1+i), cell_R%tabscal(isca)%scal(ideb-1+i), &
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
    do i = 1, nf
      call monotonic0(cell_L%tabvect(ivec)%vect(ideb-1+i), cell_R%tabvect(ivec)%vect(ideb-1+i), &
                   fprim%tabvect(ivec)%vect(icl(i)), fprim%tabvect(ivec)%vect(icr(i)))

    enddo
  case(postlim_monotonic1)
    do i = 1, nf
      call monotonic1(cell_L%tabvect(ivec)%vect(ideb-1+i), cell_R%tabvect(ivec)%vect(ideb-1+i), &
                   fprim%tabvect(ivec)%vect(icl(i)), fprim%tabvect(ivec)%vect(icr(i)))

    enddo
  case(postlim_monotonic2)
    do i = 1, nf
      call monotonic2(cell_L%tabvect(ivec)%vect(ideb-1+i), cell_R%tabvect(ivec)%vect(ideb-1+i), &
                   fprim%tabvect(ivec)%vect(icl(i)), fprim%tabvect(ivec)%vect(icr(i)))

    enddo
  case default
    call erreur("flux computation","unknown POST-LIMITATION method")
  endselect

enddo ! vector loop



endsubroutine postlimit_monotonic

!------------------------------------------------------------------------------!
! Changes history
!
! nov  2007 : created, ensure monotone variation at face
!------------------------------------------------------------------------------!
