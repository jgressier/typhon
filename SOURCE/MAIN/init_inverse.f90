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
integer :: ndct, nmes, nq
integer :: im, ifut, ic

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

do ifut = 1, world%prj%inverse%ncyc_futur
   read(io,*) (world%prj%inverse%tmes_expe(ic,ifut), ic=1,world%prj%inverse%nmes)
enddo

!---------------------------------------
! READ MODES

io = 402
world%prj%inverse%mode_funit = io

open(unit=io, file=trim(world%prj%inverse%mode_file), form='formatted')
 
read(io,*) ndct, nq        ! number of DCT modes and size of each mode

if (nq /= world%prj%inverse%nflux) &
  call erreur("Inverse initialization", "mode definition size is incompatible with BOCO size")

world%prj%inverse%defmode%nmode = ndct
world%prj%inverse%defmode%nq    = nq
allocate(world%prj%inverse%defmode%modes(1:ndct, 1:nq))

do im = 1, ndct
   read(io,*) (world%prj%inverse%defmode%modes(im, ic), ic=1,nq)
enddo

allocate(world%prj%inverse%sensi(1:ndct, 1:nmes, 1:world%prj%inverse%ncyc_futur))

endsubroutine init_inverse
!------------------------------------------------------------------------------!
! Changes history
!
! Apr  2008: created
!------------------------------------------------------------------------------!
