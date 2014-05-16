!------------------------------------------------------------------------------!
! MODULE : MESHPART
!------------------------------------------------------------------------------!
module MESHPART

use IOCFD
use USTMESH
use MESHBASE
use CONNECTIVITY
use CONNECT_CSR
use ISO_C_BINDING ! MAKE#NODEPENDENCY # do not remove

implicit none

! -- Variables globales du module -------------------------------------------

integer(kpp), parameter :: part_none           = 0
integer(kpp), parameter :: part_auto           = 1
integer(kpp), parameter :: part_metiskway      = 10
integer(kpp), parameter :: part_metisrecursive = 11

#ifdef METIS
  integer, parameter :: nometisoptions = 40   ! #define METIS_NOPTIONS          40
  integer, parameter :: metisint       = 4   ! if #define IDXTYPEWIDTH 32 in metis.h
  !integer, parameter :: metisint       = 8   ! if #define IDXTYPEWIDTH 64 in metis.h
  integer, parameter :: metisreal      = 4   ! if #define REALTYPEWIDTH 32 in metis.h
  !integer, parameter :: metisreal      = 8   ! if #define REALTYPEWIDTH 64 in metis.h
  integer, parameter :: metis_objtype   = 2
  integer, parameter :: metis_ctype     = 3
  integer, parameter :: metis_iptype    = 4
  integer, parameter :: metis_rtype     = 5
  integer, parameter :: metis_dbglevel  = 6
  integer, parameter :: metis_niter     = 7
  integer, parameter :: metis_ncuts     = 8
  integer, parameter :: metis_seed      = 9
  integer, parameter :: metis_ufactor   = 17
  integer, parameter :: metis_numbering = 18
#elif defined(METIS4)
  integer, parameter :: nometisoptions = 5
  integer, parameter :: metisint       = 4  
  ! nothing to define
#else 
  ! nothing to define
#endif

#ifdef METIS
interface
  subroutine METIS_PartGraphKway(nvtx, ncon, xadj, adjncy, &
                                      vwght, vsize, adjwgt, &
                                      nparts, tpwgts, ubvec, &
                                      options, edgecut, part) bind(C)
    use ISO_C_BINDING ! MAKE#NODEPENDENCY # do not remove
    integer :: nvtx, ncon, xadj(*), adjncy(*), nparts, options(*), edgecut, part(*)
    type(c_ptr), value :: vwght, vsize, adjwgt, tpwgts, ubvec
  endsubroutine
  subroutine METIS_PartGraphRecursive(nvtx, ncon, xadj, adjncy, &
                                      vwght, vsize, adjwgt, &
                                      nparts, tpwgts, ubvec, &
                                      options, edgecut, part) bind(C)
    use ISO_C_BINDING ! MAKE#NODEPENDENCY # do not remove
    integer :: nvtx, ncon, xadj(*), adjncy(*), nparts, options(*), edgecut, part(*)
    type(c_ptr), value :: vwght, vsize, adjwgt, tpwgts, ubvec
  endsubroutine
endinterface
#elif defined(METIS4)
  ! nothing to define
#else 
  ! nothing to define
#endif

contains
!------------------------------------------------------------------------------!
! Procedure : ustmesh_partition
! Fonction
!   METIS split : compute partition of a ustmesh
!------------------------------------------------------------------------------!
subroutine ustmesh_partition(part_method, umesh, npart, ncell, partition, verbose)
implicit none
! -- INPUTS  --
integer(kpp), intent(in) :: part_method
type(st_ustmesh)         :: umesh      ! unstructured mesh to split
integer                  :: npart     ! tot nb of parts
integer                  :: ncell     ! number of interior cells
logical, optional        :: verbose
! -- OUTPUTS --
integer(kip)     :: partition(1:ncell)   ! results: part index for all internal cells

! -- Private Data --
character(len=256) :: str_w
logical            :: debug

! Variables fonction METIS
type(st_csr)         :: csr
integer, allocatable :: vwgt(:), adjwgt(:)  ! Weight of vertices and edges
integer              :: wgtflag       ! Weight option
integer              :: nconst
integer              :: numflag       ! FLAG TO BE SET TO 1 IN FORTRAN / Metis4
integer              :: options(nometisoptions)       ! Misc Options
integer              :: edgecut       ! Number of edge cut
! Autres variables
integer                            :: size_adjncy   ! Size of adjncy
integer                            :: size_xadj     ! Size of xadj
integer                            :: cur_adjncy    ! Other variable
type(c_ptr)                        :: vsize, tpwgts, ubvec

integer, dimension(:), allocatable :: tab_parts      ! Number of cells in each parts
integer                            :: i, j, k  ! compteur de boucle
integer                            :: partmin, partmax, partavg
integer(kpp)                       :: imeth

! -- BODY --

if (present(verbose)) then
  debug = verbose
else
  debug = .false.
endif

!--------------------------------------------------------------------
! Compute CSR connectivity of USTMESH for METIS

call new(csr, umesh%facecell, ncell)  ! only internal cells

call cfd_print("    Num of cells: "//trim(strof(ncell))//" to cut into "//trim(strof(npart))//" parts")

imeth = part_none
select case(part_method)
case(part_auto)
  if (npart >= 8) then
    imeth = part_metiskway
  else  
    imeth = part_metisrecursive
  endif
case(part_metiskway, part_metisrecursive)
  imeth = part_method
case default
  call cfd_error("internal error: unknown required partition method")
endselect

! ------------------------------------------------
#ifdef METIS

call METIS_SetDefaultOptions(options)
options(metis_numbering) = 1
if (debug) options(metis_dbglevel)  = 1
nconst = 1

select case(imeth)
case(part_metiskway)
  call cfd_print("  call metis_PartGraphKway...")
  call METIS_PartGraphKway(ncell, nconst, csr%row_index, csr%col_index, &
                           C_NULL_PTR, C_NULL_PTR, C_NULL_PTR,  &
                           npart, C_NULL_PTR, C_NULL_PTR, options, edgecut, partition)
case(part_metisrecursive)
  call cfd_print("  call metis_PartGraphRecursive...")
  call METIS_PartGraphRecursive(ncell, nconst, csr%row_index, csr%col_index, &
                                C_NULL_PTR, C_NULL_PTR, C_NULL_PTR,  &
                                npart, C_NULL_PTR, C_NULL_PTR, options, edgecut, partition)
case default
  call cfd_error("internal error: unknown partition method (Metis V5)")
endselect

! ------------------------------------------------
#elif defined(METIS4)

j = csr%nval / 2
allocate(adjwgt(j))
adjwgt(1:j) = 1
allocate(vwgt(ncell))
vwgt(1:ncell) = 1
wgtflag      = 0                ! Flag (weighted graph or not)
options(1:5) = 0                ! Misc Options
numflag      = 1                ! TO BE SET TO 1 IN FORTRAN

select case(imeth)
case(part_metiskway)
  call cfd_print("  call metis_PartGraphKway (v4)...")
  call METIS_PartGraphKway(ncell, csr%row_index, csr%col_index, vwgt, adjwgt, wgtflag, numflag, &
                           npart, options, edgecut, partition)
case(part_metisrecursive)
  call cfd_print("  call metis_PartGraphRecursive  (v4)...")
  call METIS_PartGraphRecursive(ncell, csr%row_index, csr%col_index, vwgt, adjwgt, wgtflag, numflag, &
                                npart, options, edgecut, partition)
case default
  call cfd_error("internal error: unknown partition method (Metis V4)")
endselect
deallocate(adjwgt, vwgt)

! ------------------------------------------------
#else 
  call cfd_error("Internal error: METIS library was not activated at configuration time")
#endif

call delete(csr)

allocate(tab_parts(npart))

do i = 1, npart
  tab_parts(i) = count(partition(1:ncell) == i)
enddo

! -- parts stats --
partmin = minval(tab_parts(1:npart))
partmax = maxval(tab_parts(1:npart))
partavg = sum(tab_parts(1:npart))/npart
write(str_w,'(a,3(i8,a),a,f4.1,a)') "    part sizes: ", &
  partmin," (min) / ",partavg," (avg) / ",partmax," (max)", &
  " and ",real(partmax-partmin)/partavg*100,"% deviation"
call cfd_print(trim(str_w))
call cfd_print("      face cut: "//trim(strof(edgecut))//" ("//trim(stroff(100.*edgecut/ncell,2))//"% of cells)")

deallocate(tab_parts)

endsubroutine ustmesh_partition

endmodule
!------------------------------------------------------------------------------!
! Change history
!
! Sept 2005: initial subroutine from DIVISION/division_zone in pamr branch (J. Rodriguez)
! Oct  2005: optimize CSR computation (JG)
! Apr  2014: move from SOURCE/MGRID/getpart_grid to CFDTOOLS as a module
!------------------------------------------------------------------------------!
