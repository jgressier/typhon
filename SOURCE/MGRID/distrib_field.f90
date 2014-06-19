!------------------------------------------------------------------------------!
! Procedure : distrib_field               Auteur : J. Gressier
!                                         Date   : February 2005
! Fonction                                Modif  : (cf history)
!   Distribute cell field according to face->cell connectivity
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine distrib_field(field, facecell, ideb, ifin, fieldL, fieldR, istart, nsim)

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
integer		      :: nsim       ! Number of simulations
! -- OUTPUTS --
type(st_genericfield) :: fieldL, fieldR        ! left and right fields

! -- Internal Variables --
integer :: if, k, icl, icr, iv, isim

! -- BODY --
do isim = 1, nsim ! loop on simulations 
  do if = ideb, ifin
    k   = istart-ideb + if
    icl = facecell%fils(if,1)
    icr = facecell%fils(if,2)
    do iv = 1, field%nscal
      fieldL%tabscal(iv)%scal(nsim*(k-1)+isim) = field%tabscal(iv)%scal(nsim*(icl-1)+isim)
      fieldR%tabscal(iv)%scal(nsim*(k-1)+isim) = field%tabscal(iv)%scal(nsim*(icr-1)+isim)
    enddo
    do iv = 1, field%nvect
      fieldL%tabvect(iv)%vect(nsim*(k-1)+isim) = field%tabvect(iv)%vect(nsim*(icl-1)+isim)
      fieldR%tabvect(iv)%vect(nsim*(k-1)+isim) = field%tabvect(iv)%vect(nsim*(icr-1)+isim)
    enddo
    do iv = 1, field%ntens
      fieldL%tabtens(iv)%tens(nsim*(k-1)+isim) = field%tabtens(iv)%tens(nsim*(icl-1)+isim)
      fieldR%tabtens(iv)%tens(nsim*(k-1)+isim) = field%tabtens(iv)%tens(nsim*(icr-1)+isim)
    enddo
  enddo
enddo

!-----------------------------
endsubroutine distrib_field

!------------------------------------------------------------------------------!
! Changes history
!
! feb  2005 : creation
!------------------------------------------------------------------------------!
