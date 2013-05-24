!------------------------------------------------------------------------------!
! Procedure : init_probes               Auteur : J. Gressier
!                                         Date   : Janvier 2004
! Fonction                                Modif  : see history
!   Initialisation des capteurs
!     - capteurs par defaut
!     - capteurs defnis par l'utilisateur
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine init_probes(zone)

use TYPHMAKE
use OUTPUT
use DEFZONE
use MENU_GEN

implicit none

! -- Inputs --

! -- Outputs --

! -- Inputs/Outputs --
type(st_zone) :: zone

! -- Internal variables --
integer :: i             ! index de domaine/capteurs

! -- BODY --

select case(zone%info%time_model)

case(time_steady)
  open(unit=uf_monres, file="monres."//strof_full_int(1,3), form = "formatted")
  write(uf_monres,'(a)') "# variables: it residual"

case(time_unsteady, time_unsteady_inverse)
  open(unit=uf_monres, file="monres."//strof_full_int(1,3), form = "formatted")
  write(uf_monres,'(a)') "# variables: it residual"
  open(unit=uf_monphy, file="monphy."//strof_full_int(1,3), form = "formatted")
  write(uf_monphy,'(a)') "# variables: time"

case default
  call error_stop("internal error (init_probes): unknown time model")

endselect

do i = 1, zone%defsolver%nprobe

  if (zone%defsolver%probe(i)%write) then
    zone%defsolver%probe(i)%unit = getnew_io_unit()
    if (zone%defsolver%probe(i)%unit <= 0) &
         call error_stop("IO unit management: impossible to find free unit")
    open(unit = zone%defsolver%probe(i)%unit, &
         file=trim(zone%defsolver%probe(i)%name)//".tmon", form = "formatted")
    !write(zone%defsolver%probe(i)%unit, *) '#'
  endif

enddo

endsubroutine init_probes
!------------------------------------------------------------------------------!
! Changes history
!
! jan  2004: created
! july 2004: initialization of default probes
! June 2009: specific files per probe monitor
!------------------------------------------------------------------------------!
