!------------------------------------------------------------------------------!
! Procedure : inverse_get_tmes
!
! Fonction
!
!------------------------------------------------------------------------------!
subroutine inverse_get_tmes(winfo, ifut, definv, zone, tmes)
 
use TYPHMAKE
use OUTPUT
use VARCOM
use DEFZONE
use MENU_INVERSE

implicit none

! -- INPUTS --
type(st_info) :: winfo     ! world info
integer       :: ifut
type(mnu_inv) :: definv
type(st_zone) :: zone

! -- INPUTS/OUTPUTS --

! -- OUTPUTS --
real(krp) :: tmes(definv%nmes, definv%ncyc_futur)  ! computed tmes(1:nmes, 1:nfut)

! -- Internal variables --
integer                :: ib, nmes
type(st_grid), pointer :: pgrid

! -- BODY --

ib   = definv%ib_tmes
nmes = definv%nmes

! ensure computation of primitive data and BOCO (temperature for KDIF)

pgrid => zone%gridlist%first
do while (associated(pgrid))
  call calc_varprim(zone%defsolver, pgrid%info%field_loc)     ! calcul des var. primitives
  pgrid => pgrid%next
enddo

! -- recompute BOCO data (needed for BOCO outputs AND Tecplot output) --

pgrid => zone%gridlist%first
do while (associated(pgrid))
  call calcboco_ust(winfo%curtps, zone%defsolver, zone%defsolver%defspat, pgrid)
  pgrid => pgrid%next
enddo

! -- measure --

tmes(1:nmes, ifut) = zone%gridlist%first%info%field_loc%etatprim%tabscal(1)%scal( &
                       zone%gridlist%first%umesh%facecell%fils(                        &
                         zone%gridlist%first%umesh%boco(ib)%iface(1:nmes), 2))


!-------------------------------------
endsubroutine inverse_get_tmes

!------------------------------------------------------------------------------!
! Changes history
!
! Apr 2008: created
! Sep 2009: ensure primitive data computation
!------------------------------------------------------------------------------!
