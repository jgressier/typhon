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


!------------------------------------------------------------------------------
! SCALAR computations
!------------------------------------------------------------------------------

icl(1:nf)  = umesh%facecell%fils(ideb:ideb+nf, 1)
icr(1:nf)  = umesh%facecell%fils(ideb:ideb+nf, 2)

do isca = 1, fprim%nscal

  smin(1:nf) = min( fprim%tabscal(isca)%scal(icl(1:nf)), fprim%tabscal(isca)%scal(icr(1:nf)) )
  smax(1:nf) = max( fprim%tabscal(isca)%scal(icl(1:nf)), fprim%tabscal(isca)%scal(icr(1:nf)) )

  if (isca==2) then
  do i = 1, nf
    if ((icl(i)==40).or.(icr(i)==40)) print'(2i4,4f8.1)',icl(i),icr(i),smin(i),smax(i),cell_L%tabscal(isca)%scal(i),cell_R%tabscal(isca)%scal(i)
  enddo ; endif

  where (cell_L%tabscal(isca)%scal > smax) cell_L%tabscal(isca)%scal = smax
  where (cell_R%tabscal(isca)%scal > smax) cell_R%tabscal(isca)%scal = smax
  where (cell_L%tabscal(isca)%scal < smin) cell_L%tabscal(isca)%scal = smin
  where (cell_R%tabscal(isca)%scal < smin) cell_R%tabscal(isca)%scal = smin

enddo ! scalar loop


!------------------------------------------------------------------------------
! VECTOR computations
!------------------------------------------------------------------------------

do ivec = 1, fprim%nvect

  do i = 1, nf

    smin(i) = min( fprim%tabvect(ivec)%vect(icl(i))%x, fprim%tabvect(ivec)%vect(icr(i))%x )
    smax(i) = max( fprim%tabvect(ivec)%vect(icl(i))%x, fprim%tabvect(ivec)%vect(icr(i))%x )

    if (cell_L%tabvect(ivec)%vect(i)%x > smax(i)) cell_L%tabvect(ivec)%vect(i)%x = smax(i)
    if (cell_R%tabvect(ivec)%vect(i)%x > smax(i)) cell_R%tabvect(ivec)%vect(i)%x = smax(i)
    if (cell_L%tabvect(ivec)%vect(i)%x < smin(i)) cell_L%tabvect(ivec)%vect(i)%x = smin(i)
    if (cell_R%tabvect(ivec)%vect(i)%x < smin(i)) cell_R%tabvect(ivec)%vect(i)%x = smin(i)

    smin(i) = min( fprim%tabvect(ivec)%vect(icl(i))%y, fprim%tabvect(ivec)%vect(icr(i))%y )
    smax(i) = max( fprim%tabvect(ivec)%vect(icl(i))%y, fprim%tabvect(ivec)%vect(icr(i))%y )

    if (cell_L%tabvect(ivec)%vect(i)%y > smax(i)) cell_L%tabvect(ivec)%vect(i)%y = smax(i)
    if (cell_R%tabvect(ivec)%vect(i)%y > smax(i)) cell_R%tabvect(ivec)%vect(i)%y = smax(i)
    if (cell_L%tabvect(ivec)%vect(i)%y < smin(i)) cell_L%tabvect(ivec)%vect(i)%y = smin(i)
    if (cell_R%tabvect(ivec)%vect(i)%y < smin(i)) cell_R%tabvect(ivec)%vect(i)%y = smin(i)

    smin(i) = min( fprim%tabvect(ivec)%vect(icl(i))%z, fprim%tabvect(ivec)%vect(icr(i))%z )
    smax(i) = max( fprim%tabvect(ivec)%vect(icl(i))%z, fprim%tabvect(ivec)%vect(icr(i))%z )

    if (cell_L%tabvect(ivec)%vect(i)%z > smax(i)) cell_L%tabvect(ivec)%vect(i)%z = smax(i)
    if (cell_R%tabvect(ivec)%vect(i)%z > smax(i)) cell_R%tabvect(ivec)%vect(i)%z = smax(i)
    if (cell_L%tabvect(ivec)%vect(i)%z < smin(i)) cell_L%tabvect(ivec)%vect(i)%z = smin(i)
    if (cell_R%tabvect(ivec)%vect(i)%z < smin(i)) cell_R%tabvect(ivec)%vect(i)%z = smin(i)

  enddo

enddo ! vector loop



endsubroutine postlimit_monotonic

!------------------------------------------------------------------------------!
! Changes history
!
! nov  2007 : created, ensure monotone variation at face
!------------------------------------------------------------------------------!
