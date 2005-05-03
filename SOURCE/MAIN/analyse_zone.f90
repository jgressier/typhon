!------------------------------------------------------------------------------!
! Procedure : analyse                             Authors : J. Gressier
!                                                 Created : May 2005
! Fonction                                        Modif   : (cf history)
!   Analyse project and report information
!
!------------------------------------------------------------------------------!
subroutine analyse_zone(lzone)

use TYPHMAKE
use STRING
use OUTPUT
use VARCOM
use MODWORLD

implicit none

! -- inputs --
type(st_zone) :: lzone

! -- outputs --

! -- Internal variables --
type(st_grid), pointer :: pgrid
integer(kip)           :: ib, iub, if, ifa

! -- Debut de la procedure --

call print_info(6, "* ZONE: "//trim(lzone%name))

!--------------------------------------------------------
! ANALYSE ZONE
!--------------------------------------------------------
! solver

do ib = 1, lzone%defsolver%nboco
  call print_info(9, "export positions in "//trim(lzone%defsolver%boco(ib)%family)//".fam")
  open(unit=600+ib, file=trim(lzone%defsolver%boco(ib)%family)//".fam", form = "formatted")
enddo
  
!--------------------------------------------------------
pgrid => lzone%grid
 
do while(associated(pgrid))
 
  do ib = 1, pgrid%umesh%nboco 
    iub = pgrid%umesh%boco(ib)%idefboco      ! index of boco definition in defsolver
    do if = 1, pgrid%umesh%boco(ib)%nface
      ifa = pgrid%umesh%boco(ib)%iface(if)   ! absolute index of boco face in face array
      write(600+iub, '(i6,3e20.12)') if, tab(pgrid%umesh%mesh%iface(ifa,1,1)%centre)
    enddo
  enddo
 
  pgrid => pgrid%next
 
enddo

do ib = 1, lzone%defsolver%nboco
  close(unit=600+ib)
enddo
 


endsubroutine analyse_zone

!------------------------------------------------------------------------------!
! Change history
!
! may  2005 : creation 
!------------------------------------------------------------------------------!
