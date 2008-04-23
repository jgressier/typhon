!------------------------------------------------------------------------------!
! Procedure : init_inverse

! Fonction
!   Initialization INVERSE solver
!
!------------------------------------------------------------------------------!
subroutine init_inverse(world)

use TYPHMAKE
use OUTPUT
use MODWORLD
use MENU_INVERSE

implicit none

! -- INPUTS/OUTPUTS --
type(st_world) :: world

! -- Internal Variables --
integer :: iz, ib, io
integer :: ndct, nmes
integer :: im, ifut

! -- BODY --

!---------------------------------------
! FIND BOCO

world%prj%inverse%iz_tmes    = 0
world%prj%inverse%iz_unknown = 0
world%prj%inverse%ib_tmes    = 0
world%prj%inverse%ib_unknown = 0

do iz = 1, world%prj%nzone
   do ib = 1, world%zone(iz)%gridlist%first%umesh%nboco
      if (samestring(world%zone(iz)%gridlist%first%umesh%boco(ib)%family, &
          world%prj%inverse%bc_tmes)) then
         world%prj%inverse%iz_tmes    = iz
         world%prj%inverse%ib_tmes    = ib
      endif
      if (samestring(world%zone(iz)%gridlist%first%umesh%boco(ib)%family, &
          world%prj%inverse%bc_unknown)) then
         world%prj%inverse%iz_unknown = iz
         world%prj%inverse%ib_unknown = ib
      endif
   enddo
enddo

if (world%prj%inverse%iz_tmes * world%prj%inverse%iz_unknown &
  * world%prj%inverse%ib_tmes * world%prj%inverse%ib_unknown == 0) then
  call erreur("Inverse Solver", "one BOCO has not been found")
endif

!---------------------------------------
! MEASURE face treatment

iz = world%prj%inverse%iz_tmes
ib = world%prj%inverse%ib_tmes

world%prj%inverse%nmes  = world%zone(iz)%gridlist%first%umesh%boco(ib)%nface

allocate(world%prj%inverse%tmes_expe(world%prj%inverse%nmes, world%prj%inverse%ncyc_futur))

!---------------------------------------
! FLUX face treatment

iz = world%prj%inverse%iz_unknown
ib = world%prj%inverse%ib_unknown

world%prj%inverse%nflux  = world%zone(iz)%gridlist%first%umesh%boco(ib)%nface

!!make flux non uniform and set bckdif%flux_nunif to 0


!---------------------------------------
! READ TMES

io = 401
world%prj%inverse%tmes_funit = io

open(unit=io, file=trim(world%prj%inverse%tmes_file), form='formatted')

!!read...

!---------------------------------------
! READ DCT MODES

io = 402
world%prj%inverse%dct_funit = io

open(unit=io, file=trim(world%prj%inverse%dct_file), form='formatted')

!!read .... ndct

world%prj%inverse%ndctmode = ndct
allocate(world%prj%inverse%modes(1:ndct, 1:2))

do im = 1, ndct
   !!read ...
enddo

allocate(world%prj%inverse%sensi(1:ndct, 1:nmes, 1:world%prj%inverse%ncyc_futur))

endsubroutine init_inverse
!------------------------------------------------------------------------------!
! Changes history
!
! Apr  2008: created
!------------------------------------------------------------------------------!
