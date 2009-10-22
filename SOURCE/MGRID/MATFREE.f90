!------------------------------------------------------------------------------!
! MODULE : MATFREE
!
! Fonction   
!   Library for matrix-free matrix products on field structures
!
!------------------------------------------------------------------------------!

module MATFREE

use DEFFIELD
use GENFIELD
use MODINFO
use MENU_SOLVER
use MENU_NUM
use MGRID
use MENU_ZONECOUPLING

implicit none

! -- Module global variables ------------------------------------------------


! -- DECLARATIONS -----------------------------------------------------------

!------------------------------------------------------------------------------!
! structure ST_FIELDINFO : container for field information
!------------------------------------------------------------------------------!

type st_fieldinfo
  real(krp)         :: dt               ! 
  type(st_infozone) :: info             ! zone information structure
  type(mnu_solver)  :: defsolver        ! solver parameters
  type(st_gridlist) :: gridlist         ! list of grids
  integer           :: ncoupling        ! number of couplings of zone
  type(mnu_zonecoupling), dimension(:), pointer &
                    :: coupling         ! coupling data
endtype st_fieldinfo


! -- INTERFACES -------------------------------------------------------------


! -- Functions and Operators ------------------------------------------------


! -- IMPLEMENTATION ---------------------------------------------------------
contains


!------------------------------------------------------------------------------!
! calcrhs_free : unpacks field_info and calls calc_rhs
!------------------------------------------------------------------------------!
subroutine calcrhs_free(field_info)
implicit none
! -- parameters --
type(st_fieldinfo) :: field_info
! -- internal --

! -- body --

call calc_rhs(field_info%dt, field_info%info, &
              field_info%defsolver, &
              field_info%gridlist, &
              field_info%coupling, field_info%ncoupling)

endsubroutine calcrhs_free


!------------------------------------------------------------------------------!
! yeqdfdqx_free : y = dFdQ.x
!------------------------------------------------------------------------------!
subroutine yeqdfdqx_free(y, x, uref, rref, field_info)
implicit none
! -- parameters --
type(st_genericfield), dimension(:), intent(out) :: y
type(st_genericfield), dimension(:), intent(in)  :: x, uref, rref
type(st_fieldinfo) , intent(in) :: field_info
! -- internal
type(st_grid), pointer :: pgrid
integer(kip)           :: ngrid, igrid
real(krp) :: coef

! -- body --
ngrid  = field_info%gridlist%nbgrid

coef = 1._krp ! compute(coef)

! -- etatcons = uref + coef * x --
pgrid => field_info%gridlist%first
do igrid = 1, ngrid
  call transfer(pgrid%field%etatcons, uref(igrid))
  call xeqxpay(pgrid%field%etatcons, coef, x(igrid))
  pgrid => pgrid%next
enddo

! -- residu = rhs(etatcons) --
call calcrhs_free(field_info)

! -- y = ( residu -rref ) / coef --
pgrid => field_info%gridlist%first
do igrid = 1, ngrid
  call transfer(y(igrid), pgrid%field%residu)
  call xeqxpay(y(igrid), -1.0_krp, rref(igrid))
  call scale(y(igrid), -1.0_krp/coef)
  pgrid => pgrid%next
enddo

endsubroutine yeqdfdqx_free


!------------------------------------------------------------------------------!
! yeqax_free : y = A.x
!------------------------------------------------------------------------------!
subroutine yeqax_free(y, x, uref, rref, field_info)
implicit none
! -- parameters --
type(st_genericfield), dimension(:), intent(out) :: y
type(st_genericfield), dimension(:), intent(in)  :: x, uref, rref
type(st_fieldinfo) , intent(in) :: field_info
! -- internal
type(st_grid), pointer :: pgrid
integer(kip)           :: ngrid, igrid
integer(kip)           :: ic, i

! -- body --
ngrid  = field_info%gridlist%nbgrid

call yeqdfdqx_free_opt(y, x, uref, rref, field_info)

pgrid => field_info%gridlist%first
do igrid = 1, ngrid
  do i = 1,pgrid%field%etatcons%nscal
    do ic = 1, pgrid%umesh%ncell_int
      y(igrid)%tabscal(i)%scal(ic) = x(igrid)%tabscal(i)%scal(ic) &
                                   + y(igrid)%tabscal(i)%scal(ic)
!        + ( pgrid%dtloc(ic) &
!        / pgrid%umesh%mesh%volume(ic,1,1) ) * y(igrid)%tabscal(i)%scal(ic)
!          y(igrid)%tabscal(1)%scal(ic) &
!        + x(igrid)%tabscal(1)%scal(ic) * pgrid%umesh%mesh%volume(ic,1,1) &
!                                       / pgrid%dtloc(ic)
    enddo
  enddo
  do i = 1,pgrid%field%etatcons%nvect
    do ic = 1, pgrid%umesh%ncell_int
      y(igrid)%tabvect(i)%vect(ic) = x(igrid)%tabvect(i)%vect(ic) &
                                   + y(igrid)%tabvect(i)%vect(ic)
!        + ( pgrid%dtloc(ic) &
!        / pgrid%umesh%mesh%volume(ic,1,1) ) * y(igrid)%tabvect(i)%vect(ic)
!          y(igrid)%tabvect(1)%vect(ic) &
!        + x(igrid)%tabvect(1)%vect(ic) * pgrid%umesh%mesh%volume(ic,1,1) &
!                                       / pgrid%dtloc(ic)
    enddo
  enddo
  pgrid => pgrid%next
enddo

endsubroutine yeqax_free


!------------------------------------------------------------------------------!
! yeqmaxpz_free : y = - A.x + z
!------------------------------------------------------------------------------!
subroutine yeqmaxpz_free(y, x, z, uref, rref, field_info)
implicit none
! -- parameters --
type(st_genericfield), dimension(:), intent(out) :: y
type(st_genericfield), dimension(:), intent(in)  :: x, z, uref, rref
type(st_fieldinfo) , intent(in) :: field_info
! -- internal

! -- body --

call yeqax_free(y, x, uref, rref, field_info)
call scale(y, -1.0_krp)
call xeqxpay(y, 1.0_krp, z)

endsubroutine yeqmaxpz_free


endmodule MATFREE


!------------------------------------------------------------------------------!
! Changes history
!
! Jun 2009 : creation
!------------------------------------------------------------------------------!
