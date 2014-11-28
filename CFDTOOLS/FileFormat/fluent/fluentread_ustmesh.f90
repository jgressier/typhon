!------------------------------------------------------------------------------!
! Routine: fluentread_ustmesh
!
! Function
!   Read a FLUENT format to USTMESH structure
!------------------------------------------------------------------------------!
subroutine fluentread_ustmesh(deffluent, umesh)

! use IO_UNIT
! use XBIN_IO
! use XBIN_DATA
! use TYPHON_FMT
! use TYFMT_MESH
use FLUENT
use USTMESH

implicit none

! -- INPUTS --
type(st_deffluent) , intent(inout) :: deffluent

! -- OUTPUTS --
type(st_ustmesh)   , intent(out)   :: umesh

! -- Internal variables --
integer :: sctid
!integer :: iunit1, iunit2
logical :: ieof

! -- BODY --

!-----------------------------------------------------------------
! Initialization of USTMESH

call init_ustmesh(umesh, 1)   ! default values initialization

! -- dimensions --
indopt = 1
namopt = "main_____"
hedopt = 0

ieof = .false.
do while (.not.ieof)
  stropt = " "
  indopt = 1

  ! todo: check section order...

!-----------------------------------------------------------------
! Read FLUENT section id

  call fluent_get_sctid(deffluent, sctid)

!-----------------------------------------------------------------
! Process FLUENT sections

  select case(sctid)

!-----------------------------------------------------------------
! Read FLUENT comment

  case(fluent_sct_comment)      ! Section : Comment
  call fluent_get_comment(deffluent)  ! todo: only use zone definition for check...

!-----------------------------------------------------------------
! Read FLUENT header

  case(fluent_sct_header)       ! Section : Header
  call fluent_get_header(deffluent)

!-----------------------------------------------------------------
! Read FLUENT space dimension

  case(fluent_sct_dim)          ! Section : Space dimension
  call fluent_get_spcdim(deffluent, umesh)

!-----------------------------------------------------------------
! Ignored FLUENT section

  case(fluent_sct_ignore1)      ! Section : ignore1 (ignored)
  call fluent_get_ignored(deffluent, sctid)

!-----------------------------------------------------------------
! Read FLUENT nodes (xy[z] coordinates)

  case(fluent_sct_nodes, &
       fluent_sct_spbinnodes, &
       fluent_sct_dpbinnodes)   ! Section : Nodes
  call fluent_get_nodes(deffluent, sctid, umesh)

!-----------------------------------------------------------------
! Read FLUENT cells (?...)

  case(fluent_sct_cells, &
       fluent_sct_spbincells)   ! Section : Cells
  call fluent_get_cells(deffluent, sctid, umesh)   ! todo: use cell definition for checks...

!-----------------------------------------------------------------
! Read FLUENT faces (connectivity)

  case(fluent_sct_faces, &
       fluent_sct_spbinfaces)   ! Section : Faces
  call fluent_get_faces(deffluent, sctid, umesh)    ! todo: FILL (deffluent &) mesh structure

!-----------------------------------------------------------------
! Read FLUENT boundary definitions & conditions

  case(fluent_sct_zonebcs)      ! Section : Zone boundary and bcs
  call fluent_get_zonebcs(deffluent, sctid, umesh)    ! todo: FILL (deffluent &) mesh structure

!-----------------------------------------------------------------
! Read FLUENT boundary definitions

  case(fluent_sct_zonebnd)      ! Section : Zone boundary
  call fluent_get_zonebnd(deffluent, sctid, umesh)    ! todo: FILL (deffluent &) mesh structure

!-----------------------------------------------------------------
! Unknown FLUENT section

  case default                  ! Section : UNKNOWN
    call cfd_error("(FLUENT) unknown section-id: '"//strof(sctid)//"'")
  endselect

!-----------------------------------------------------------------
! End of FLUENT section

  call fluent_get_sctend(deffluent, ieof)
enddo

call fluent2typhon_facecells(deffluent, umesh)

!------------------------------------------------------------------------------!
! Check MESH

call check_ustmesh_elements(umesh)

endsubroutine fluentread_ustmesh
!------------------------------------------------------------------------------!
! Change history
! Aug 2014: created
!------------------------------------------------------------------------------!
