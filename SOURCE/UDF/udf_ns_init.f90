!------------------------------------------------------------------------------!
! Procedure : udf_ns_init                           Authors : J. Gressier
!                                                   Created : Oct 2005
! Fonction
!   UDF fonction for NS/Euler Initialization (primitive variables)
!
!------------------------------------------------------------------------------!
subroutine udf_ns_init(defns, npt, point, prim)

use TYPHMAKE
use MESHBASE
use MENU_NS
use GENFIELD

implicit none

! -- INPUTS --
type(mnu_ns)   :: defns
integer        :: npt                  ! nb of points
type(v3d)      :: point(1:npt)         ! geometrical positions

! -- OUTPUTS --
type(st_genericfield) :: prim

! -- Internal variables --
integer :: ic

! -- BODY --

do ic = 1, npt
  if (point(ic)%x <= 0.5_krp) then
    prim%tabscal(1)%scal(ic) = 120._krp
    prim%tabscal(2)%scal(ic) = 120._krp/1.4_krp
    prim%tabvect(1)%vect(ic) = v3d(0._krp, 0._krp, 0._krp)
  else
    prim%tabscal(1)%scal(ic) = 1.2_krp
    prim%tabscal(2)%scal(ic) = 1.2_krp/1.4_krp
    prim%tabvect(1)%vect(ic) = v3d(0._krp, 0._krp, 0._krp)
  endif
enddo

endsubroutine udf_ns_init

!------------------------------------------------------------------------------!
! Change history
!
! Oct  2005 : creation
!------------------------------------------------------------------------------!
