!------------------------------------------------------------------------------!
! Procedure : inverse_get_tmes
!
! Fonction
!
!------------------------------------------------------------------------------!
subroutine inverse_get_tmes(ifut, definv, zone, tmes)
 
use TYPHMAKE
use OUTPUT
use VARCOM
use DEFZONE
use MENU_INVERSE

implicit none

! -- INPUTS --
integer       :: ifut
type(mnu_inv) :: definv
type(st_zone) :: zone

! -- INPUTS/OUTPUTS --

! -- OUTPUTS --
real(krp) :: tmes(definv%nmes, definv%ncyc_futur)  ! computed tmes(1:nmes, 1:nfut)

! -- Internal variables --
integer :: ib, nmes

! -- BODY --

ib   = definv%ib_tmes
nmes = definv%nmes

tmes(1:nmes, ifut) = zone%gridlist%first%info%field_loc%etatprim%tabscal(1)%scal( &
                       zone%gridlist%first%umesh%facecell%fils(                        &
                         zone%gridlist%first%umesh%boco(ib)%iface(1:nmes), 2))


!-------------------------------------
endsubroutine inverse_get_tmes

!------------------------------------------------------------------------------!
! Changes history
!
! Apr 2008: created
!------------------------------------------------------------------------------!
