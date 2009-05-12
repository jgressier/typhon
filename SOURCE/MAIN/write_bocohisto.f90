!------------------------------------------------------------------------------!
! write_bocohisto
!
! Fonction
!   Writing BOCO history
!------------------------------------------------------------------------------!
subroutine write_bocohisto(icycle, zone)

use TYPHMAKE
use OUTPUT
use DEFZONE
use MENU_GEN

implicit none

! -- INPUTS --
integer :: icycle

! -- OUTPUTS --

! -- INPUTS/OUTPUTS --
type(st_zone) :: zone

! -- Internal variables --
integer                     :: ib,  idef, uio, is, iv
character(len=300)          :: strline
character(len=50)           :: str_cycle, str
character(len=8), parameter :: sca_format = '(1e14.6)'
character(len=8), parameter :: vec_format = '(3e14.6)'
type(st_ustboco), pointer   :: pboco

! -- BODY --

write(str_cycle, '(i6)') icycle

do ib = 1, zone%gridlist%first%umesh%nboco       ! for all BOCO in USTMESH

  pboco => zone%gridlist%first%umesh%boco(ib)
  idef = pboco%idefboco                    ! link to boco definition in DEFSOLVER

  if (idef <= 0) cycle

  if (iand(zone%defsolver%boco(idef)%save_history, bchisto_quantity) /= 0) then

    uio = zone%defsolver%boco(idef)%histoquant_unit
    strline = str_cycle

    do is = 1, pboco%avg_quant%nscal
      write(str, sca_format) pboco%avg_quant%tabscal(is)%scal(1)
      strline = trim(strline)//str
    enddo
    do iv = 1, pboco%avg_quant%nvect
      write(str, vec_format) pboco%avg_quant%tabvect(iv)%vect(1)
      strline = trim(strline)//str
    enddo

    write(uio,'(a)') trim(strline)

  endif
  
  if (iand(zone%defsolver%boco(idef)%save_history, bchisto_flux) /= 0) then

    uio = zone%defsolver%boco(idef)%histoflux_unit
    strline = str_cycle

    do is = 1, pboco%sum_flux%nscal
      write(str, sca_format) pboco%sum_flux%tabscal(is)%scal(1)
      strline = trim(strline)//str
    enddo
    do iv = 1, pboco%sum_flux%nvect
      write(str, vec_format) pboco%sum_flux%tabvect(iv)%vect(1)
      strline = trim(strline)//str
    enddo

    write(uio,'(a)') trim(strline)

  endif
  
enddo

endsubroutine write_bocohisto

!------------------------------------------------------------------------------!
! Changes history
!
! Apr  2008: created
!------------------------------------------------------------------------------!
