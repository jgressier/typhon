!------------------------------------------------------------------------------!
! Procedure : getpart_grid                Authors : J. Rodriguez/J. Gressier
!                                         Date    : Mars 2005
! Fonction 
!   METIS split : compute partition of a grid
!
!------------------------------------------------------------------------------!
subroutine getpart_grid(fullgrid, npart, ncell, partition)

use OUTPUT
use MGRID
use USTMESH
use MESHBASE
use CONNECTIVITY
use CONNECT_CSR
!use SUBGRID
!use METIS

implicit none

! -- INPUTS  --
type(st_grid), target :: fullgrid  ! Grille initiale a decouper
integer               :: npart     ! tot nb of parts
integer               :: ncell     ! number of interior cells

! -- OUTPUTS --
integer(kip)             :: partition(1:ncell)   ! results: part index for all internal cells


! -- Declaration des variables internes --
integer                         :: first_id      ! identifiants des grilles
integer                         :: second_id
integer                         :: first_cpu
integer                         :: second_cpu
type(st_grid) , pointer         :: pfullgrid ! Grille initiale a decouper


! Variables fonction METIS
type(st_csr)                       :: csr
integer, dimension(:), allocatable ::  vwgt, adjwgt  ! Weight of vertices and edges
integer                            ::  wgtflag       ! Weight option
integer                            ::  numflag       ! FLAG TO BE SET TO 1 IN FORTRAN
integer, dimension(5)              ::  options       ! Misc Options
integer                            ::  edgecut       ! Number of edge cut

! Autres variables
integer                            ::  size_adjncy   ! Size of adjncy
integer                            ::  size_xadj     ! Size of xadj
integer                            ::  cur_adjncy    ! Other variable

integer, dimension(:), allocatable :: tab_parts      ! Number of cells in each parts
integer                            :: i, j, k  ! compteur de boucle
integer                            :: partmin, partmax, partavg

! -- BODY --

!--------------------------------------------------------------------
! Compute CSR connectivity of USTMESH for METIS

ncell = fullgrid%umesh%ncell_int        ! only consider internal cells

call new(csr, fullgrid%umesh%facecell, ncell)

!--------------------------------------------------------------------
! Init METIS parameters

wgtflag      = 0                ! Flag (weighted graph or not)
options(1:5) = 0                ! Misc Options
numflag      = 1                ! TO BE SET TO 1 IN FORTRAN 

! -- uniform weights --
j = csr%nval / 2
allocate(adjwgt(j))
adjwgt(1:j) = 1

allocate(vwgt(ncell))
vwgt(1:ncell) = 1

write(str_w,*) ".          Num of cells:",ncell," to cut into ",npart," parts"
call print_info(8,adjustl(str_w))

#ifdef metis
if (npart <= 8) then
  call print_info(8,"  call metis_PartGraphKway...")
  call METIS_PartGraphKway(ncell, csr%row_index, csr%col_index, vwgt, adjwgt, wgtflag, numflag, &
                           npart, options, edgecut, partition)
else
  call print_info(8,"  call metis_PartGraphRecursive...")
  call METIS_PartGraphRecursive(ncell, csr%row_index, csr%col_index, vwgt, adjwgt, wgtflag, numflag, &
                                npart, options, edgecut, partition)
endif
#else
  call error_stop("METIS library was missing during TYPHON compilation")
#endif

call delete(csr)

pfullgrid=>fullgrid
allocate(tab_parts(npart))

do i = 1, npart
  tab_parts(i) = count(partition(1:ncell) == i)
enddo

! -- parts stats --
partmin = minval(tab_parts(1:npart))
partmax = maxval(tab_parts(1:npart))
partavg = sum(tab_parts(1:npart))/npart
write(str_w,'(a,3(i8,a),a,f5.1,a)') "    part sizes: ", &
  partmin," (min) / ",partavg," (avg) / ",partmax," (max)", &
  " and ",real(partmax-partmin)/partavg*100," % deviation"
call print_info(10,trim(str_w))


endsubroutine getpart_grid

!------------------------------------------------------------------------------!
! Change history
!
! Sept 2005 : initial subroutine from DIVISION/division_zone in pamr branch
! Oct  2005 : optimize CSR computation (JG)
!------------------------------------------------------------------------------!
