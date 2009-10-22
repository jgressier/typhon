!------------------------------------------------------------------------------!
! init_bocohisto
!
! Fonction
!   Initialization of BOCO history
!------------------------------------------------------------------------------!
subroutine init_bocohisto(zone)

use TYPHMAKE
use OUTPUT
use DEFZONE
use MENU_GEN

implicit none

! -- INPUTS --

! -- OUTPUTS --

! -- INPUTS/OUTPUTS --
type(st_zone) :: zone

! -- Internal variables --
integer :: ib, uio

! -- BODY --


!!! DEV : GESTION des entrees/sorties et numeros d'unites par MODULE
uio = 600

do ib = 1, zone%defsolver%nboco

  if (iand(zone%defsolver%boco(ib)%save_history, bchisto_quantity) /= 0) then
    uio = uio + 1
    zone%defsolver%boco(ib)%histoquant_unit = uio
    open(unit=uio, file="histovar"//trim(zone%name)//"_"//trim(zone%defsolver%boco(ib)%family), form = "formatted")
    write(uio,'(a)') "#CYCLE AVERAGE PRIMITIVE QUANTITIES"
  endif

  if (iand(zone%defsolver%boco(ib)%save_history, bchisto_flux) /= 0) then
    uio = uio + 1
    zone%defsolver%boco(ib)%histoflux_unit = uio
    open(unit=uio, file="histoflux"//trim(zone%name)//"_"//trim(zone%defsolver%boco(ib)%family), form = "formatted")
    write(uio,'(a)') "#CYCLE INTEGRAL FLUX"
  endif

enddo

endsubroutine init_bocohisto

!------------------------------------------------------------------------------!
! Changes history
!
! Apr  2008: created
!------------------------------------------------------------------------------!
