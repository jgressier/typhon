!------------------------------------------------------------------------------!
! Procedure : distrib_field               Auteur : J. Gressier
!                                         Date   : February 2005
! Fonction                                Modif  : (cf history)
!   Distribute cell field according to face->cell connectivity
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine distrib_field(field, facecell, ideb, ifin, fieldL, fieldR, istart)

use TYPHMAKE
use OUTPUT
use VARCOM
use USTMESH
use GENFIELD

implicit none

! -- INPUTS --
type(st_genericfield) :: field      ! cell field
type(st_connect)      :: facecell   ! face to cell connectivity
integer               :: ideb, ifin ! connectivity index for distribution
integer               :: istart     ! starting index for fieldL and fieldR

! -- OUTPUTS --
type(st_genericfield) :: fieldL, fieldR        ! left and right fields

! -- Internal Variables --
integer :: if, k, icl, icr, iv

! -- BODY --

do if = ideb, ifin
  k   = istart-ideb + if
  icl = facecell%fils(if,1)
  icr = facecell%fils(if,2)
  do iv = 1, field%nscal
    fieldL%tabscal(iv)%scal(k) = field%tabscal(iv)%scal(icl)
    fieldR%tabscal(iv)%scal(k) = field%tabscal(iv)%scal(icr)
  enddo
  do iv = 1, field%nvect
    fieldL%tabvect(iv)%vect(k) = field%tabvect(iv)%vect(icl)
    fieldR%tabvect(iv)%vect(k) = field%tabvect(iv)%vect(icr)
  enddo
  do iv = 1, field%ntens
    fieldL%tabtens(iv)%tens(k) = field%tabtens(iv)%tens(icl)
    fieldR%tabtens(iv)%tens(k) = field%tabtens(iv)%tens(icr)
  enddo
enddo

!-----------------------------
endsubroutine distrib_field

!------------------------------------------------------------------------------!
! Changes history
!
! feb  2005 : creation
!------------------------------------------------------------------------------!
