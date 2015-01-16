!------------------------------------------------------------------------------!
!> @brief FLUENT module, definition, reading routines
!! FLUENT format is detailed on
!! http://aerojet.engr.ucdavis.edu/fluenthelp/html/ug/node1490.htm
!------------------------------------------------------------------------------!
! f=readmsh ; m=cone.msh ; ifort -o $f $f.f90 && ./$f <<< $m
module FLUENT

use IO_UNIT
use IOCFD
use STRING
use USTMESH
use CONNECTIVITY

implicit none

! -- Global Variables -------------------------------------------

! -- DECLARATIONS -----------------------------------------------------------

integer, parameter :: kfp = 4

character(len=262144):: stropt
character(len=4096) :: namoptlist
character(len=64)   :: namopt
integer             :: indopt
integer             :: hedopt
integer,parameter   :: lenopt   = 10
integer             :: incopt
integer,parameter   :: lennames = 30


character,parameter :: nl_char = achar(10)
character,parameter :: cr_char = achar(13)

!------------------------------------------------------------------------------!
! CONSTANTS for FLUENT ELEMENT TYPE
!------------------------------------------------------------------------------!

! Section headers
integer(kfp), parameter :: fluent_sct_comment   =  0
integer(kfp), parameter :: fluent_sct_header    =  1
integer(kfp), parameter :: fluent_sct_dim       =  2
integer(kfp), parameter :: fluent_sct_nodes      =   10
integer(kfp), parameter :: fluent_sct_spbinnodes = 2010
integer(kfp), parameter :: fluent_sct_dpbinnodes = 3010
integer(kfp), parameter :: fluent_sct_cells      =   12
integer(kfp), parameter :: fluent_sct_spbincells = 2012
integer(kfp), parameter :: fluent_sct_faces      =   13
integer(kfp), parameter :: fluent_sct_spbinfaces = 2013
integer(kfp), parameter :: fluent_sct_per       = 18
integer(kfp), parameter :: fluent_sct_zonebcs   = 39
integer(kfp), parameter :: fluent_sct_zonebnd   = 45
integer(kfp), parameter :: fluent_sct_ctree     = 58
integer(kfp), parameter :: fluent_sct_ftree     = 59
integer(kfp), parameter :: fluent_sct_fparent   = 61

! Ignored section headers
integer(kfp), parameter :: fluent_sct_ignore1   =  4

! Cell types
integer(kfp), parameter :: fluent_cell_mixed   =  0
integer(kfp), parameter :: fluent_cell_tri     =  1
integer(kfp), parameter :: fluent_cell_tetra   =  2
integer(kfp), parameter :: fluent_cell_quad    =  3
integer(kfp), parameter :: fluent_cell_hexa    =  4
integer(kfp), parameter :: fluent_cell_pyram   =  5
integer(kfp), parameter :: fluent_cell_wedge   =  6
integer(kfp), parameter :: fluent_cell_polyh   =  7

integer(kfp), parameter :: fluent_cell_types   =  7

! BC types
integer(kfp), parameter :: fluent_bc_unknown    =  0
integer(kfp), parameter :: fluent_bc_interior   =  2
integer(kfp), parameter :: fluent_bc_wall       =  3
integer(kfp), parameter :: fluent_bc_p_inlet    =  4
integer(kfp), parameter :: fluent_bc_p_outlet   =  5
integer(kfp), parameter :: fluent_bc_symmetry   =  7
integer(kfp), parameter :: fluent_bc_per_sdow   =  8
integer(kfp), parameter :: fluent_bc_p_farfld   =  9
integer(kfp), parameter :: fluent_bc_v_inlet    = 10
integer(kfp), parameter :: fluent_bc_periodic   = 12
integer(kfp), parameter :: fluent_bc_fan        = 14
integer(kfp), parameter :: fluent_bc_mflow_in   = 20
integer(kfp), parameter :: fluent_bc_interfce   = 24
integer(kfp), parameter :: fluent_bc_parent     = 31
integer(kfp), parameter :: fluent_bc_outflow    = 36
integer(kfp), parameter :: fluent_bc_axis       = 37

! Face types
integer(kfp), parameter :: fluent_faces_mixed   =  0
integer(kfp), parameter :: fluent_faces_linear  =  2
integer(kfp), parameter :: fluent_faces_tri     =  3
integer(kfp), parameter :: fluent_faces_quad    =  4
integer(kfp), parameter :: fluent_faces_polyg   =  5

! -- Private data --

! -- Parameters --

interface str_to_int
  module procedure str_to_intkfp
endinterface

interface display
  module procedure display_ustmesh, display_mesh, &
                   display_connect, display_genconnect, &
                   display_elemvtex, display_genelemvtex, &
                   display_ustboco
endinterface

! -- Data types --

type st_flucellsection
  integer              :: celltype, ncell, nface, nvtex
  integer, allocatable :: dface(:)      ! each cell : number of faces
  integer, allocatable :: face(:,:)     ! each cell : list of faces
  integer, allocatable :: nvtx(:,:)     ! each cell, each face : number of face vertices
  integer, allocatable :: vtex(:,:,:)   ! each cell, each face : list of face vertices
end type st_flucellsection

type st_flubczone
  character(len=64) :: bczonename ! each face section : zonename
  integer :: bczoneid
  integer :: bczonetype
  integer :: ifmin, ifmax, nfaces, isbctym
  integer :: off
  type(st_connect), pointer :: facevtex
  type(st_genconnect), pointer :: fvtxmixd
end type st_flubczone

type st_flufacevtexsection
  integer :: nbnodes
  integer :: nbfils
  integer, pointer :: fils(:,:)
  type(st_flufacevtexsection), pointer :: prev
  type(st_flufacevtexsection), pointer :: next
end type st_flufacevtexsection

!------------------------------------------------------------------------------!
! ST_DEFFLUENT
!------------------------------------------------------------------------------!
type st_deffluent
  integer       :: version
  integer       :: iu_bin
  logical       :: binary
  integer       :: spcdim
  integer       :: inmin, inmax, nnodes
  integer       :: icmin, icmax, ncells
  integer       :: ifmin, ifmax, nfaces
  integer       :: nsect
  integer       :: nbczone, nbctym
  integer       :: max_nvtexface
  integer       :: nfacevtex
  real*4, allocatable :: xyzsp(:,:)
  real*8, allocatable :: xyzdp(:,:)
  integer*4, allocatable :: celltype(:) ! for each absolute cell : fluent cell type
  integer*4, allocatable :: ielem(:)    ! for each absolute cell : section number
  integer*4, allocatable :: icell(:)    ! for each absolute cell : cell number in section
  type(st_flucellsection), pointer :: cellsect(:)
  type(st_flufacevtexsection), pointer :: facevtexhead, facevtextail
  type(st_flufacevtexsection), pointer :: facevtex(:)
  type(st_flubczone), pointer :: bczone(:)
  character(len=64) :: zonename = " "
  character(len=64) :: interiorname = " "
  character         :: c        ! one character read in advance
  character(len=lennames), allocatable :: bcnames(:)
end type st_deffluent


!------------------------------------------------------------------------------!
contains

subroutine readmem

  implicit none
  ! -- INPUTS --
  ! -- OUTPUTS --
  ! -- Internal variables --
  integer :: iumem, i
  character(len=256) :: str
  ! -- BODY --

  open(iumem,file='/proc/self/status',form='formatted')
  call cfd_write('---')
  do i = 1, 18
    read(iumem,'(a)') str
    select case(i)
    case(13, 15, 16, 17)
      call cfd_write(trim(str))
    case default
    endselect
  enddo
  call cfd_write('---')
  close(iumem)

endsubroutine readmem

!------------------------------------------------------------------------------!
! @brief new fluent cell section
!------------------------------------------------------------------------------!
subroutine fluent_addcellsection(def)

  implicit none
  ! -- INPUTS --
  type(st_deffluent) , intent(inout) :: def
  ! -- OUTPUTS --
  ! -- Internal variables --
  type(st_flucellsection), pointer :: cellsect(:)
  integer                    :: nsect
  ! -- BODY --

  nsect = def%nsect

  allocate(cellsect(nsect+1))

  if ( nsect >= 1 ) then
    cellsect(1:nsect) = def%cellsect(1:nsect)
    deallocate(def%cellsect)
  endif

  def%cellsect => cellsect
  def%nsect = nsect + 1

endsubroutine fluent_addcellsection

!------------------------------------------------------------------------------!
! @brief new fluent cell section
!------------------------------------------------------------------------------!
subroutine fluent_newcellsection(cellsect, celltype, ncell, nface, nvtex)

  implicit none
  ! -- INPUTS --
  type(st_flucellsection), intent(inout) :: cellsect
  integer,           intent(in)    :: celltype, ncell, nface, nvtex
  ! -- OUTPUTS --
  ! -- Internal variables --
  ! -- BODY --

  cellsect%celltype = celltype
  cellsect%ncell    = ncell
  cellsect%nface    = nface
  cellsect%nvtex    = nvtex

  allocate(cellsect%dface(ncell))
  allocate(cellsect%face(ncell,nface))
  allocate(cellsect%nvtx(ncell,nface))
  allocate(cellsect%vtex(ncell,nface,nvtex))

  cellsect%dface(1:ncell) = 0

endsubroutine fluent_newcellsection

!------------------------------------------------------------------------------!
! @brief delete fluent cell section
!------------------------------------------------------------------------------!
subroutine fluent_delcellsection(cellsect)

  implicit none
  ! -- INPUTS --
  type(st_flucellsection), intent(inout) :: cellsect
  ! -- OUTPUTS --
  ! -- Internal variables --
  ! -- BODY --

  cellsect%celltype = 0
  cellsect%ncell    = 0
  cellsect%nface    = 0
  cellsect%nvtex    = 0

  deallocate(cellsect%dface)
  deallocate(cellsect%face)
  deallocate(cellsect%nvtx)
  deallocate(cellsect%vtex)

endsubroutine fluent_delcellsection

!------------------------------------------------------------------------------!
! @brief add bc zone
!------------------------------------------------------------------------------!
subroutine fluent_addbczone(def)

  implicit none
  ! -- INPUTS --
  type(st_deffluent) , intent(inout) :: def
  ! -- OUTPUTS --
  ! -- Internal variables --
  type(st_flubczone), pointer :: bczone(:)
  integer                    :: nbczone
  ! -- BODY --

  nbczone = def%nbczone

  allocate(bczone(nbczone+1))

  if ( nbczone >= 1 ) then
    bczone(1:nbczone) = def%bczone(1:nbczone)
    deallocate(def%bczone)
  endif

  def%bczone => bczone
  def%nbczone = nbczone + 1

endsubroutine fluent_addbczone

!------------------------------------------------------------------------------!
! @brief new fluent cell section
!------------------------------------------------------------------------------!
subroutine fluent_newbczone(bczone, bczonename, bczoneid, bczonetype, &
                            ifmin, ifmax, nfaces)

  implicit none
  ! -- INPUTS --
  type(st_flubczone), intent(inout) :: bczone
  character(len=64) :: bczonename
  integer, intent(in) :: bczoneid, bczonetype
  integer, intent(in) :: ifmin, ifmax, nfaces
  ! -- OUTPUTS --
  ! -- Internal variables --
  ! -- BODY --

  bczone%bczonename = bczonename
  bczone%bczoneid   = bczoneid
  bczone%bczonetype = bczonetype
  bczone%ifmin = ifmin
  bczone%ifmax = ifmax
  bczone%nfaces = nfaces
  bczone%isbctym = 0
  bczone%off = 0

endsubroutine fluent_newbczone

!------------------------------------------------------------------------------!
! @brief fluent cell name
!------------------------------------------------------------------------------!
character(len=16) function fluent_cell_name(ctype)

  implicit none
  ! -- INPUTS --
  integer(kpp),      intent(in)  :: ctype
  ! -- OUTPUTS --
  ! -- Internal variables --
  ! -- BODY --

  select case(ctype)
  case(fluent_cell_mixed) ; fluent_cell_name = "Mixed"
  case(fluent_cell_tri)   ; fluent_cell_name = "Tri"
  case(fluent_cell_tetra) ; fluent_cell_name = "Tetra"
  case(fluent_cell_quad)  ; fluent_cell_name = "Quad"
  case(fluent_cell_hexa)  ; fluent_cell_name = "Hexa"
  case(fluent_cell_wedge) ; fluent_cell_name = "Wedge"
  case(fluent_cell_pyram) ; fluent_cell_name = "Pyram"
  case(fluent_cell_polyh) ; fluent_cell_name = "Polyh"
  case default            ; fluent_cell_name = "Unknown"
  endselect

endfunction fluent_cell_name

!------------------------------------------------------------------------------!
! @brief fluent bc name
!------------------------------------------------------------------------------!
character(len=8) function fluent_bc_name(bctype)

  implicit none
  ! -- INPUTS --
  integer(kpp),      intent(in)  :: bctype
  ! -- OUTPUTS --
  ! -- Internal variables --
  ! -- BODY --

  select case(bctype)
  case(fluent_bc_unknown)  ; fluent_bc_name = "unknown"
  case(fluent_bc_interior) ; fluent_bc_name = "interior"
  case(fluent_bc_wall)     ; fluent_bc_name = "wall"
  case(fluent_bc_p_inlet)  ; fluent_bc_name = "p_inlet"
  case(fluent_bc_p_outlet) ; fluent_bc_name = "p_outlet"
  case(fluent_bc_symmetry) ; fluent_bc_name = "symmetry"
  case(fluent_bc_per_sdow) ; fluent_bc_name = "per_sdow"
  case(fluent_bc_p_farfld) ; fluent_bc_name = "p_farfld"
  case(fluent_bc_v_inlet)  ; fluent_bc_name = "v_inlet"
  case(fluent_bc_periodic) ; fluent_bc_name = "periodic"
  case(fluent_bc_fan)      ; fluent_bc_name = "fan"
  case(fluent_bc_mflow_in) ; fluent_bc_name = "mflow_in"
  case(fluent_bc_interfce) ; fluent_bc_name = "interfce"
  case(fluent_bc_parent)   ; fluent_bc_name = "parent"
  case(fluent_bc_outflow)  ; fluent_bc_name = "outflow"
  case(fluent_bc_axis)     ; fluent_bc_name = "axis"
  case default             ; fluent_bc_name = "Unknown"
  endselect

endfunction fluent_bc_name

!------------------------------------------------------------------------------!
! @brief fluent BC name
!------------------------------------------------------------------------------!
integer(kpp) function fluent_bc_type(bctypename)

  implicit none
  ! -- INPUTS --
  character(len=*),      intent(in)  :: bctypename
  ! -- OUTPUTS --
  ! -- Internal variables --
  ! -- BODY --

  select case(bctypename)
  case("fluid")              ; fluent_bc_type = fluent_bc_interior
  case("solid")              ; fluent_bc_type = fluent_bc_interior
  !case("shadow")             ; fluent_bc_type = fluent_bc_shadow
  case("interior")           ; fluent_bc_type = fluent_bc_interior
  case("wall")               ; fluent_bc_type = fluent_bc_wall
  case("pressure-inlet")     ; fluent_bc_type = fluent_bc_p_inlet
  case("inlet-vent")         ; fluent_bc_type = fluent_bc_p_inlet
  case("intake-fan")         ; fluent_bc_type = fluent_bc_p_inlet
  case("pressure-outlet")    ; fluent_bc_type = fluent_bc_p_outlet
  case("exhaust-fan")        ; fluent_bc_type = fluent_bc_p_outlet
  case("outlet-vent")        ; fluent_bc_type = fluent_bc_p_outlet
  case("symmetry")           ; fluent_bc_type = fluent_bc_symmetry
  case("periodic-shadow")    ; fluent_bc_type = fluent_bc_per_sdow
  case("pressure-far-field") ; fluent_bc_type = fluent_bc_p_farfld
  case("velocity-inlet")     ; fluent_bc_type = fluent_bc_v_inlet
  case("periodic")           ; fluent_bc_type = fluent_bc_periodic
  case("fan")                ; fluent_bc_type = fluent_bc_fan
  case("porous-jump")        ; fluent_bc_type = fluent_bc_fan
  case("radiator")           ; fluent_bc_type = fluent_bc_fan
  case("mass-flow-inlet")    ; fluent_bc_type = fluent_bc_mflow_in
  case("interface")          ; fluent_bc_type = fluent_bc_interfce
  case("parent")             ; fluent_bc_type = fluent_bc_parent
  case("outflow")            ; fluent_bc_type = fluent_bc_outflow
  case("axis")               ; fluent_bc_type = fluent_bc_axis
  case default               ; fluent_bc_type = fluent_bc_unknown
  endselect

endfunction fluent_bc_type

!------------------------------------------------------------------------------!
! @brief fluent face name
!------------------------------------------------------------------------------!
character(len=16) function fluent_face_name(etype)

  implicit none
  ! -- INPUTS --
  integer(kpp),      intent(in)  :: etype
  ! -- OUTPUTS --
  ! -- Internal variables --
  ! -- BODY --

  select case(etype)
  case(fluent_faces_mixed) ; fluent_face_name = "mixed"
  case(fluent_faces_linear); fluent_face_name = "linear"
  case(fluent_faces_tri)   ; fluent_face_name = "tri"
  case(fluent_faces_quad)  ; fluent_face_name = "quad"
  case(fluent_faces_polyg) ; fluent_face_name = "polyg"
  case default             ; fluent_face_name = "Unknown"
  endselect

endfunction fluent_face_name

!------------------------------------------------------------------------------!
! @brief transfer element type
!------------------------------------------------------------------------------!
integer function fluent2typhon_elemtype(etype)

  implicit none
  ! -- INPUTS --
  integer(kpp),      intent(in)  :: etype
  ! -- OUTPUTS --
  ! -- Internal variables --
  ! -- BODY --

  select case(etype)
  case(fluent_cell_tri)   ; fluent2typhon_elemtype = elem_tri3
  case(fluent_cell_quad)  ; fluent2typhon_elemtype = elem_quad4
  case(fluent_cell_polyh) ; fluent2typhon_elemtype = elem_ngon
  case(fluent_cell_tetra) ; fluent2typhon_elemtype = elem_tetra4
  case(fluent_cell_pyram) ; fluent2typhon_elemtype = elem_pyra5
  case(fluent_cell_wedge) ; fluent2typhon_elemtype = elem_penta6
  case(fluent_cell_hexa)  ; fluent2typhon_elemtype = elem_hexa8
  case default            ; fluent2typhon_elemtype = -1
  endselect

endfunction fluent2typhon_elemtype

!------------------------------------------------------------------------------!
!> @brief open FLUENT file
!------------------------------------------------------------------------------!
subroutine fluent_openread(filename, def)

  implicit none
  ! -- INPUTS --
  character(len=*)   , intent(in)  :: filename
  ! -- OUTPUTS --
  type(st_deffluent) , intent(out) :: def
  ! -- Internal variables --
  integer :: info
  logical :: lexist
  ! -- BODY --

  INQUIRE( &
       FILE   = filename, &
       EXIST  = lexist)
  if (.not. lexist) then
    call cfd_error("(fluent) file does not exist: "//trim(filename))
  endif

  def%iu_bin = getnew_io_unit()
  OPEN(UNIT   = def%iu_bin, &
       FILE   = filename, &
       STATUS = "old", &
       ACCESS = "STREAM", &
       FORM   = "UNFORMATTED", CONVERT = 'LITTLE_ENDIAN', &
       IOSTAT = info)
  if (info /= 0) then
    call cfd_error("(fluent) unable to open file "//trim(filename))
  endif

  !read(def%iu_bin) def%c
  call cfd_print("(fluent) Start of file...")

  def%nsect = 0
  def%nbczone = 0
  def%nbctym = 0

endsubroutine fluent_openread

!------------------------------------------------------------------------------!
!> @brief close FLUENT file
!------------------------------------------------------------------------------!
subroutine fluent_close(def)

  implicit none
  ! -- INPUTS --
  type(st_deffluent) , intent(in) :: def
  ! -- OUTPUTS --
  ! -- Internal variables --
  ! -- BODY --

  call close_io_unit(def%iu_bin)

endsubroutine fluent_close


!------------------------------------------------------------------------------!
character        function chnl(c)
  implicit none
  character        :: c
  select case(ichar(c))
  case(0)
    chnl = " "
  case(10, 13)
    chnl = "\"
  case default
    chnl = c
  endselect
endfunction chnl

!------------------------------------------------------------------------------!
subroutine dumpp()
  implicit none
  call flush(6)
  write(6,'(a)') trim(stropt)
  call flush(6)
endsubroutine dumpp

!------------------------------------------------------------------------------!
subroutine stopp(iu, c)
  implicit none
  integer :: i, iu
  character        :: c
  character(len=2) :: str
  do i = 1, 16
    read(iu) c
    write(str,'(z0.2)') ichar(c)
    call affline("follow: c='"//chnl(c)//"'" &
               //" (0x"//str//")")
  enddo
  call dumpp()
  stop
endsubroutine stopp

!------------------------------------------------------------------------------!
subroutine affstarp(optname)
  implicit none
  character(len=*) :: optname
  hedopt = hedopt+1
  write(namopt,'(a)') " "//optname
  write(namoptlist(lenopt* hedopt+1 : &
                   lenopt*(hedopt+1)),'(a)') namopt(1:lenopt)
  incopt = 1
  call affline("(start)")
  incopt = 0
endsubroutine affstarp

!------------------------------------------------------------------------------!
subroutine affexitp(optname)
  implicit none
  character(len=*) :: optname
  incopt = 1
  call affline("(exit)")
  incopt = 0
  hedopt = hedopt-1
  write(namopt,'(a)') namoptlist(lenopt* hedopt+1 : &
                                 lenopt*(hedopt+1))
endsubroutine affexitp

!------------------------------------------------------------------------------!
subroutine affstar(optname, c)
  implicit none
  character(len=*) :: optname
  character        :: c
  hedopt = hedopt+1
  write(namopt,'(a)') " "//optname
  write(namoptlist(lenopt* hedopt+1 : &
                   lenopt*(hedopt+1)),'(a)') namopt(1:lenopt)
  incopt = 1
  call affline("(start: c='"//chnl(c)//"')")
  incopt = 0
endsubroutine affstar

!------------------------------------------------------------------------------!
subroutine affexit(optname, c)
  implicit none
  character(len=*) :: optname
  character        :: c
  incopt = 1
  call affline("(exit:  c='"//chnl(c)//"')")
  incopt = 0
  hedopt = hedopt-1
  write(namopt,'(a)') namoptlist(lenopt* hedopt+1 : &
                                 lenopt*(hedopt+1))
endsubroutine affexit

!------------------------------------------------------------------------------!
subroutine affline(str)
  implicit none
  character(len=*) :: str
  character(len=1024), parameter :: blkstr = " "
  write(stropt(indopt:len(stropt)),'(7a)') &
    blkstr(1:2*hedopt),"[",namopt(1+incopt:lenopt+incopt),"] ", &
    blkstr(1:2*hedopt),trim(str),nl_char
  indopt = indopt &
         + 2*hedopt + 1 + lenopt + 2 &
         + 2*hedopt + len(str) + 1
endsubroutine affline

!------------------------------------------------------------------------------!
subroutine affhead(str)
  implicit none
  character(len=*) :: str
  character(len=1024), parameter :: blkstr = " "
  write(stropt(indopt:len(stropt)),'(6a)') &
    blkstr(1:2*hedopt),"[",namopt(1+incopt:lenopt+incopt),"] ", &
    blkstr(1:2*hedopt),trim(str)
  indopt = indopt &
         + 2*hedopt + 1 + lenopt + 2 &
         + 2*hedopt + len(str)
endsubroutine affhead

!------------------------------------------------------------------------------!
subroutine affcont(str)
  implicit none
  character(len=*) :: str
  write(stropt(indopt:len(stropt)),'(a)') &
    trim(str)
  indopt = indopt + len(str)
endsubroutine affcont

!------------------------------------------------------------------------------!
subroutine afftail(str)
  implicit none
  character(len=*) :: str
  write(stropt(indopt:len(stropt)),'(2a)') &
    trim(str),nl_char
  indopt = indopt + len(str) + 1
endsubroutine afftail

!------------------------------------------------------------------------------!
subroutine str_index_spc(str, indspc, nspc)
  implicit none
  character(len=*) , intent(in)  :: str
  integer          , intent(out) :: indspc(:)
  integer          , intent(out) :: nspc
  integer          :: i, prevnonspc
  nspc = 0
  prevnonspc = 0

  i = 1

  do i = 1, len_trim(str)
    if ( str(i:i) == ' ' ) then         ! If space found
      if ( prevnonspc == 1 ) then       !   If follows non-space
        nspc = nspc+1                   !   Increment and set if size ok
        if ( nspc <= size(indspc) ) indspc(nspc) = i
      endif
      prevnonspc = 0                    ! space was found
    else
      prevnonspc = 1                    ! non-space was found
    endif
  enddo
  nspc=nspc+1
  if ( nspc <= size(indspc) ) indspc(nspc) = len_trim(str)+1
endsubroutine str_index_spc

!------------------------------------------------------------------------------!
subroutine str_read_strs(str, snb, slist, ierr)
  implicit none
  character(len=*) , intent(in)  :: str
  integer          , intent(out) :: snb
  character(len=*) , intent(out) :: slist(:)
  integer          , intent(out) :: ierr
  integer          :: indspc(64), nspc
  integer          :: ispc, ii
  ierr = 0
  call str_index_spc(str, indspc, nspc)
  if ( nspc > size(slist) ) then
    call cfd_warning("too many words ("//trim(strof(nspc))//">" &
                                       //trim(strof(size(slist)))//")" &
                   //" for array in string: '"//trim(str)//"'")
    ierr = 1
    return
  endif
  ii = 1
  snb = 0
  do ispc = 1, nspc
    snb = snb+1
    if ( indspc(ispc)-ii >= len(slist(snb)) ) then
      call cfd_warning("substring #"//trim(strof(snb))//" too long " &
                     //"("//trim(strof(indspc(ispc)-ii+1)) &
                     //">"//trim(strof(len(slist(snb))))//")" &
                     //" in string: '"//trim(str)//"'")
      ierr = 1
      return
    endif
    slist(snb) = str(ii:indspc(ispc))
    ii =indspc(ispc)+1
  enddo
endsubroutine str_read_strs

!------------------------------------------------------------------------------!
subroutine str_read_ints(str, formt, inb, ilist, ierr)
  implicit none
  character(len=*) , intent(in)  :: str
  character(len=*) , intent(in)  :: formt
  integer          , intent(out) :: inb
  integer(kfp)     , intent(out) :: ilist(:)
  integer          , intent(out) :: ierr
  integer          :: indspc(64), nspc
  integer          :: ispc, ii
  call str_index_spc(str, indspc, nspc)
  if ( nspc > size(ilist) ) then
    call cfd_warning("too many words ("//trim(strof(nspc))//">" &
                                       //trim(strof(size(ilist)))//")" &
                   //" for array in string: '"//trim(str)//"'")
    ierr = 1
    return
  endif
  ii = 1
  inb = 0
  do ispc = 1, nspc
    inb = inb+1
    ilist(inb) = str_to_int(str(ii:indspc(ispc)), formt, ierr)
    if ( ierr /= 0 ) then
      call cfd_warning("could not read integer #"//trim(strof(ispc)) &
                     //" from: '"//trim(str)//"'")
      return
    endif
    ii =indspc(ispc)+1
  enddo
endsubroutine str_read_ints

!------------------------------------------------------------------------------!
integer(kfp) function str_to_intkfp(str, formt, ierr)
  implicit none
  character(len=*) :: str
  character(len=*) :: formt
  integer(kfp) :: ivalue
  integer :: ierr
  read(str,formt,iostat=ierr) ivalue
  if ( ierr /= 0 ) then
    call cfd_warning("could not read integer" &
                   //" from: '"//trim(str)//"'" &
                   //" by:   '"//trim(formt)//"'")
    return
  endif
  str_to_intkfp = ivalue
endfunction str_to_intkfp

!------------------------------------------------------------------------------!
character(len=16) function chardesc(c)
  implicit none
  character        :: c
  select case(c)
  case(' ')
    chardesc = "space"
  case(nl_char)
    chardesc = "nline"
  case(cr_char)
    chardesc = "cgrtn"
  case default
    chardesc = "char"
  endselect
endfunction chardesc

!------------------------------------------------------------------------------!
!> @brief FLUENT-low-level subroutines
!------------------------------------------------------------------------------!

!------------------------------------------------------------------------------!
!> @brief FLUENT-low-level : skip blank (space or newline)
!------------------------------------------------------------------------------!
subroutine fll_ignore_blank(iu, c)
  implicit none
  ! -- INPUTS --
  integer          , intent(in)    :: iu
  character        , intent(inout) :: c
  ! -- OUTPUTS --
  ! -- Internal variables --
  ! -- BODY --
  call affstar("ignore_b_", c)
  do while ( c == ' ' &
        .or. c == nl_char &
        .or. c == cr_char )
    call affline("'"//chnl(c)//"'"// &
                 " "//trim(chardesc(c))//" skipped")
    read(iu) c
  enddo
  call affexit("ignore_b_", c)
endsubroutine fll_ignore_blank

!------------------------------------------------------------------------------!
!> @brief FLUENT-low-level : store char list
!------------------------------------------------------------------------------!
subroutine fll_storecharlist(iu, c, clist, str)
  implicit none
  ! -- INPUTS --
  integer          , intent(in)    :: iu
  character        , intent(inout) :: c
  character(len=*) , intent(in)    :: clist
  ! -- OUTPUTS --
  character(len=*) , intent(out)   :: str
  ! -- Internal variables --
  integer :: i, lstr
  ! -- BODY --
  !call affstar("store_dec", c)
  str = ""
  lstr = len(str)
  i = 0
  !call affhead("'")
  do while ( scan(clist,c) /= 0 )
  !call affcont(c)
    i = i+1
    if ( i > lstr ) then
      call cfd_error("(fluent internal) string length "//trim(strof(lstr))//" exceeded")
    endif
    str(i:i) = c
    read(iu) c
  enddo
  !call afftail("' stored")
  !call affexit("store_dec", c)
endsubroutine fll_storecharlist

!------------------------------------------------------------------------------!
!> @brief FLUENT-low-level : store decimal digits
!------------------------------------------------------------------------------!
#define fll_storedecdigits(iu, c, str) fll_storecharlist(iu, c, "0123456789", str)
!subroutine fll_storedecdigits(iu, c, str)
!  implicit none
!  ! -- INPUTS --
!  integer          , intent(in)    :: iu
!  character        , intent(inout) :: c
!  ! -- OUTPUTS --
!  character(len=*) , intent(out)   :: str
!  ! -- Internal variables --
!!  integer :: i, lstr
!  ! -- BODY --
!  call fll_storecharlist(iu, c, "0123456789", str)
!!  !call affstar("store_dec", c)
!!  str = ""
!!  lstr = len(str)
!!  i = 0
!!  !call affhead("'")
!!  do while ( c >= '0' .and. c <= '9' )
!!  !call affcont(c)
!!    i = i+1
!!    if ( i > lstr ) then
!!      call cfd_error("(fluent internal) string length "//trim(strof(lstr))//" exceeded")
!!    endif
!!    str(i:i) = c
!!    read(iu) c
!!  enddo
!!  !call afftail("' stored")
!!  !call affexit("store_dec", c)
!endsubroutine fll_storedecdigits

!------------------------------------------------------------------------------!
!> @brief FLUENT-low-level : store hexadecimal digits
!------------------------------------------------------------------------------!
#define fll_storehexdigits(iu, c, str) fll_storecharlist(iu, c, "0123456789abcdef", str)
!subroutine fll_storehexdigits(iu, c, str)
!  implicit none
!  ! -- INPUTS --
!  integer          , intent(in)    :: iu
!  character        , intent(inout) :: c
!  ! -- OUTPUTS --
!  character(len=*) , intent(out)   :: str
!  ! -- Internal variables --
!!  integer :: i, lstr
!  ! -- BODY --
!  call fll_storecharlist(iu, c, "0123456789abcdef", str)
!!  !call affstar("store_hex", c)
!!  lstr = len(str)
!!  str = ""
!!  i = 0
!!  !call affhead("'")
!!  do while ( ( c >= '0' .and. c <= '9' ) &
!!        .or. ( c >= 'a' .and. c <= 'f' ) )
!!  !call affcont(c)
!!    i = i+1
!!    if ( i > lstr ) then
!!      call cfd_error("(fluent internal) string length "//trim(strof(lstr))//" exceeded")
!!    endif
!!    str(i:i) = c
!!    read(iu) c
!!  enddo
!!  !call afftail("' stored")
!!  !call affexit("store_hex", c)
!endsubroutine fll_storehexdigits

!------------------------------------------------------------------------------!
!> @brief FLUENT-low-level : store up to list of chars
!------------------------------------------------------------------------------!
subroutine fll_storeuptocharlist(iu, c, clist, str)
  implicit none
  ! -- INPUTS --
  integer          , intent(in)    :: iu
  character        , intent(inout) :: c
  character(len=*) , intent(in)    :: clist
  ! -- OUTPUTS --
  character(len=*) , intent(out)   :: str
  ! -- Internal variables --
  integer :: i, lstr
  ! -- BODY --
  !call affstar("store_chl", c)
  str = ""
  lstr = len(str)
  i = 0
  !call affhead("'")
  do while ( scan(clist,c) == 0 )
  !call affcont(c)
    i = i+1
    if ( i > lstr ) then
      call cfd_error("(fluent internal) string length "//trim(strof(lstr))//" exceeded")
    endif
    str(i:i) = c
    read(iu) c
  enddo
  !call affcont("' stored")
  !call afftail(" ; str='"//trim(str)//"'")
  !call affexit("store_chl", c)
endsubroutine fll_storeuptocharlist

!------------------------------------------------------------------------------!
!> @brief FLUENT-low-level : store up to separator (blank or bracket)
!------------------------------------------------------------------------------!
#define fll_storeuptosepbb(iu, c, str) fll_storeuptocharlist(iu, c, "() "//nl_char//cr_char, str)
!subroutine fll_storeuptosepbb(iu, c, str)
!  implicit none
!  ! -- INPUTS --
!  integer          , intent(in)    :: iu
!  character        , intent(inout) :: c
!  ! -- OUTPUTS --
!  character(len=*) , intent(out)   :: str
!  ! -- Internal variables --
!  integer :: i, lstr
!  ! -- BODY --
!  !call affstar("store_sep", c)
!  str = ""
!  lstr = len(str)
!  i = 0
!  !call affhead("'")
!  do while ( c /= ' ' &
!       .and. c /= nl_char &
!       .and. c /= cr_char &
!       .and. c /= '(' &
!       .and. c /= ')' )
!  !call affcont(c)
!    i = i+1
!    if ( i > lstr ) then
!      call cfd_error("(fluent internal) string length "//trim(strof(lstr))//" exceeded")
!    endif
!    str(i:i) = c
!    read(iu) c
!  enddo
!  !call affcont("' stored")
!  !call afftail(" ; str='"//trim(str)//"'")
!  !call affexit("store_sep", c)
!endsubroutine fll_storeuptosepbb

!------------------------------------------------------------------------------!
!> @brief FLUENT-low-level : store up to blank (space or newline)
!------------------------------------------------------------------------------!
#define fll_storeuptoblank(iu, c, str) fll_storeuptocharlist(iu, c, " "//nl_char//cr_char, str)
!subroutine fll_storeuptoblank(iu, c, str)
!  implicit none
!  ! -- INPUTS --
!  integer          , intent(in)    :: iu
!  character        , intent(inout) :: c
!  ! -- OUTPUTS --
!  character(len=*) , intent(out)   :: str
!  ! -- Internal variables --
!  integer :: i, lstr
!  ! -- BODY --
!  !call affstar("store_nb_", c)
!  str = ""
!  lstr = len(str)
!  i = 0
!  !call affhead("'")
!  do while ( c /= ' ' &
!       .and. c /= nl_char &
!       .and. c /= cr_char )
!  !call affcont(c)
!    i = i+1
!    if ( i > lstr ) then
!      call cfd_error("(fluent internal) string length "//trim(strof(lstr))//" exceeded")
!    endif
!    str(i:i) = c
!    read(iu) c
!  enddo
!  !call affcont("' stored")
!  !call afftail(" ; str='"//trim(str)//"'")
!  !call affexit("store_nb_", c)
!endsubroutine fll_storeuptoblank

!------------------------------------------------------------------------------!
!> @brief FLUENT-low-level : store up to some given character
!! and DO NOT step on
!------------------------------------------------------------------------------!
subroutine fll_storeuptocharnostep(iu, c, cc, str)
  implicit none
  ! -- INPUTS --
  integer          , intent(in)    :: iu
  character        , intent(inout) :: c
  character        , intent(in)    :: cc
  ! -- OUTPUTS --
  character(len=*) , intent(out)   :: str
  ! -- Internal variables --
  integer :: i, lstr
  ! -- BODY --
  !call affstar("store_chr", c)
  str = ""
  lstr = len(str)
  i = 0
  !call affhead("'")
  do while ( c /= cc )
  !call affcont(c)
    i = i+1
    if ( i > lstr ) then
      call stopp(iu, c)
    endif
    str(i:i) = c
    read(iu) c
  enddo
  !call affcont("' stored")
  !call afftail(" ; str='"//trim(str)//"'")
  !call affexit("store_chr", c)
endsubroutine fll_storeuptocharnostep

!------------------------------------------------------------------------------!
!> @brief FLUENT-low-level : store up to some given character
!------------------------------------------------------------------------------!
subroutine fll_storeuptochar(iu, c, cc, str)
  implicit none
  ! -- INPUTS --
  integer          , intent(in)    :: iu
  character        , intent(inout) :: c
  character        , intent(in)    :: cc
  ! -- OUTPUTS --
  character(len=*) , intent(out)   :: str
  ! -- Internal variables --
  ! -- BODY --
  !call affstar("store_chr", c)
  call fll_storeuptocharnostep(iu, c, cc, str)
  read(iu) c
  !call affline("next : '"//c//"'")
  !call affexit("store_chr", c)
endsubroutine fll_storeuptochar

!------------------------------------------------------------------------------!
!> @brief FLUENT-low-level : check next non-blank is some given character
!! and DO NOT step on
!------------------------------------------------------------------------------!
subroutine fll_checknextcharnostep(iu, c, cc)
  implicit none
  ! -- INPUTS --
  integer          , intent(in)    :: iu
  character        , intent(inout) :: c
  character        , intent(in)    :: cc
  ! -- OUTPUTS --
  ! -- Internal variables --
  ! -- BODY --
  call affstar("chknxnstp", c)
  call fll_ignore_blank(iu, c)
  if ( c /= cc ) then
    call cfd_error("(fluent internal) unexpected character ("//cc//") : '"//c//"'")
  endif
  call affline("'"//chnl(c)//"' checked")
  call affexit("chknxnstp", c)
endsubroutine fll_checknextcharnostep

!------------------------------------------------------------------------------!
!> @brief FLUENT-low-level : check next non-blank is some given character
!------------------------------------------------------------------------------!
subroutine fll_checknextchar(iu, c, cc)
  implicit none
  ! -- INPUTS --
  integer          , intent(in)    :: iu
  character        , intent(inout) :: c
  character        , intent(in)    :: cc
  ! -- OUTPUTS --
  ! -- Internal variables --
  ! -- BODY --
  call affstar("check_nxt", c)
  call fll_checknextcharnostep(iu, c, cc)
  read(iu) c
  call affexit("check_nxt", c)
endsubroutine fll_checknextchar

!------------------------------------------------------------------------------!
!> @brief FLUENT-low-level : read integer
!------------------------------------------------------------------------------!
subroutine fll_getint(iu, c, val, ierr)
  implicit none
  ! -- INPUTS --
  integer          , intent(in)    :: iu
  character        , intent(inout) :: c
  ! -- OUTPUTS --
  integer          , intent(out)   :: val
  integer          , intent(out)   :: ierr
  ! -- Internal variables --
  character(len=256) :: str
  ! -- BODY --
  call affstar("get_int__", c)
  call fll_ignore_blank(iu, c)
  call fll_storedecdigits(iu, c, str)
  call affline("read integer from string : '"//trim(str)//"'")
  val = str_to_int(str, '(i)', ierr)
  ! Error could be put inside str_to_int
  if ( ierr /= 0 ) then
    call cfd_warning("could not read integer" &
                   //" from: '"//trim(str)//"'")
  else
    call affline("integer read : "//trim(str))
  endif
  call affexit("get_int__", c)
endsubroutine fll_getint

!------------------------------------------------------------------------------!
!> @brief FLUENT-low-level : read hexadecimal integer
!------------------------------------------------------------------------------!
subroutine fll_gethexint(iu, c, val, ierr)
  implicit none
  ! -- INPUTS --
  integer          , intent(in)    :: iu
  character        , intent(inout) :: c
  ! -- OUTPUTS --
  integer          , intent(out)   :: val
  integer          , intent(out)   :: ierr
  ! -- Internal variables --
  character(len=256) :: str
  ! -- BODY --
  call affstar("get_hex__", c)
  call fll_ignore_blank(iu, c)
  call fll_storeuptoblank(iu, c, str)
  call affline("read integer from string : '"//trim(str)//"'")
  val = str_to_int(str, '(z)', ierr)
  if ( ierr /= 0 ) then
    call cfd_warning("could not read hex integer" &
                   //" from: '"//trim(str)//"'")
  else
    write(str,'(i)') val
    call affline("integer read : "//trim(str))
  endif
  call affexit("get_hex__", c)
endsubroutine fll_gethexint

!------------------------------------------------------------------------------!
!> @brief FLUENT-low-level : read string
!------------------------------------------------------------------------------!
subroutine fll_getstring(iu, c, str)
  implicit none
  ! -- INPUTS --
  integer          , intent(in)    :: iu
  character        , intent(inout) :: c
  ! -- OUTPUTS --
  character(len=*) , intent(out)   :: str
  ! -- Internal variables --
  ! -- BODY --
  call affstar("get_str__", c)
  call fll_checknextchar(iu, c, '"')
  call fll_storeuptochar(iu, c, '"', str)
  call affline("read string : '"//trim(str)//"'")
  call affexit("get_str__", c)
endsubroutine fll_getstring

!------------------------------------------------------------------------------!
!> @brief FLUENT-low-level : read list
!------------------------------------------------------------------------------!
subroutine fll_getlist(iu, c, str)
  implicit none
  ! -- INPUTS --
  integer          , intent(in)    :: iu
  character        , intent(inout) :: c
  ! -- OUTPUTS --
  character(len=*) , intent(out)   :: str
  ! -- Internal variables --
  ! -- BODY --
  call affstar("getlist_", c)
  call fll_checknextchar(iu, c, '(')
  call fll_storeuptochar(iu, c, ')', str)
  call affline("read list : '"//trim(str)//"'")
  call affexit("getlist_", c)
endsubroutine fll_getlist

!------------------------------------------------------------------------------!
!> @brief FLUENT-low-level : extract list
!------------------------------------------------------------------------------!
subroutine fll_extractlist(iu, c, strlistsize, strlist)
  implicit none
  ! -- INPUTS --
  integer          , intent(in)    :: iu
  character        , intent(inout) :: c
  integer          , intent(inout) :: strlistsize
  ! -- OUTPUTS --
  character(len=*) , intent(out)   :: strlist(strlistsize)
  ! -- Internal variables --
  integer           :: nstr, inst
  character         :: s
  ! -- BODY --
  call affstar("extract__", c)
  call fll_checknextchar(iu, c, '(')
  do nstr = 1, strlistsize
    inst = 0
    strlist(nstr) = ""
    call fll_ignore_blank(iu, c)
    if ( c == ')' ) exit
    call fll_storeuptosepbb(iu, c, strlist(nstr))
  write(s,'(i1)') nstr
  call affline("strlist["//s//"] = '"//trim(strlist(nstr))//"'")
    inst = len_trim(strlist(nstr))
  enddo
  nstr = nstr-1
  write(6,'(a,i4,a,$)') "nstr =",nstr,"    "
  strlistsize = nstr
  call fll_checknextchar(iu, c, ')')
  call affexit("extract__", c)
endsubroutine fll_extractlist

!------------------------------------------------------------------------------!
!> @brief FLUENT-low-level subroutines end
!------------------------------------------------------------------------------!

!------------------------------------------------------------------------------!
!> @brief FLUENT : read string list
!------------------------------------------------------------------------------!
subroutine fluent_read_strlist(def, str)

  implicit none
  ! -- INPUTS --
  type(st_deffluent) , intent(inout) :: def
  ! -- OUTPUTS --
  character(len=*)   , intent(out)   :: str
  ! -- Internal variables --
  ! -- BODY --
  call affstar("get_list_", def%c)
  call fll_getlist(def%iu_bin, def%c, str)
  call affexit("get_list_", def%c)

endsubroutine fluent_read_strlist

!------------------------------------------------------------------------------!
!> @brief FLUENT : read integer list
!------------------------------------------------------------------------------!
subroutine fluent_read_intlist(def, formt, inb, ilist, str, ierr)

  implicit none
  ! -- INPUTS --
  type(st_deffluent) , intent(inout) :: def
  character(len=*)   , intent(in)    :: formt
  ! -- OUTPUTS --
  integer            , intent(out)   :: inb
  integer(kfp)       , intent(out)   :: ilist(:)
  character(len=*)   , intent(out)   :: str
  integer            , intent(out)   :: ierr
  ! -- Internal variables --
  ! -- BODY --
  call affstar("get_list_", def%c)
  call fll_getlist(def%iu_bin, def%c, str)
  call str_read_ints(str, formt, inb, ilist, ierr)
  if ( ierr /= 0 ) then
    call cfd_warning("(fluent) could not read integer list")
  endif
  call affexit("get_list_", def%c)

endsubroutine fluent_read_intlist

!------------------------------------------------------------------------------!
!> @brief FLUENT : check space dimension
!------------------------------------------------------------------------------!
subroutine fluent_check_spcdim(spcdim, iswrite)

  implicit none
  ! -- INPUTS --
  integer , intent(in) :: spcdim
  integer , intent(in) , optional :: iswrite
  ! -- OUTPUTS --
  ! -- Internal variables --
  ! -- BODY --
  select case(spcdim)
  case(2)
    if ( present(iswrite) .and. iswrite == 1 ) &
    call cfd_print("2-dimensional mesh (planar)")
  case(3)
    if ( present(iswrite) .and. iswrite == 1 ) &
    call cfd_print("3-dimensional mesh")
  case default
    call cfd_error("(fluent) unknown space dimension: " &
                   //trim(strof(spcdim)))
  endselect

endsubroutine fluent_check_spcdim

!------------------------------------------------------------------------------!
!> @brief FLUENT : add only new nodes to array
!------------------------------------------------------------------------------!
subroutine fluent_addnodes(nfn, facenodes, nnl, nodelist)

  implicit none
  ! -- INPUTS --
  integer, intent(in)    :: nfn
  integer, intent(in)    :: facenodes(:)
  ! -- OUTPUTS --
  integer, intent(inout) :: nnl
  integer, intent(inout) :: nodelist(:)
  ! -- Internal variables --
  integer :: ifn, inl
  ! -- BODY --

  do ifn = 1, nfn
    do inl = 1, nnl
      if ( nodelist(inl) == facenodes(ifn) ) exit
    enddo
    nnl = nnl+1
    nodelist(nnl) = facenodes(ifn)
  enddo

endsubroutine fluent_addnodes

!------------------------------------------------------------------------------!
!> @brief FLUENT : read section id (and leading '(')
!------------------------------------------------------------------------------!
subroutine fluent_get_sctid(def, sctid)

  implicit none
  ! -- INPUTS --
  type(st_deffluent) , intent(inout) :: def
  ! -- OUTPUTS --
  integer            , intent(out)   :: sctid
  ! -- Internal variables --
  integer :: ierr
  ! -- BODY --
  call affstar("get_s_id_", def%c)
  call fll_checknextchar(def%iu_bin, def%c, '(')
  call fll_getint(def%iu_bin, def%c, sctid, ierr)
  if ( ierr /= 0 ) then
    call cfd_error("could not read section id")
  endif
  call affline("read section head : '"//trim(strof(sctid))//"'")
  call affexit("get_s_id_", def%c)

endsubroutine fluent_get_sctid

!------------------------------------------------------------------------------!
!> @brief FLUENT : read section : comment
!------------------------------------------------------------------------------!
subroutine fluent_get_comment(def)

  implicit none
  ! -- INPUTS --
  type(st_deffluent) , intent(inout) :: def
  ! -- OUTPUTS --
  ! -- Internal variables --
  character(len=64)  :: str
  integer            :: isname = 0
  ! -- BODY --
  call fll_getstring(def%iu_bin, def%c, str)
  if ( str(1:22) == "Interior faces of zone" ) then
    isname = 24
  elseif ( str(1:13) == "Faces of zone" ) then
    isname = 15
  endif
  if ( isname /= 0 ) then
    if ( def%zonename /= "" ) then
      call cfd_print("zonename "//'"'//trim(def%zonename)//'"'//" is ignored")
    endif
    def%zonename = trim(str(isname:))
  else
    call cfd_print('"'//trim(str)//'"')
  endif

endsubroutine fluent_get_comment

!------------------------------------------------------------------------------!
!> @brief FLUENT : read section : header
!------------------------------------------------------------------------------!
subroutine fluent_get_header(def)

  implicit none
  ! -- INPUTS --
  type(st_deffluent) , intent(inout) :: def
  ! -- OUTPUTS --
  ! -- Internal variables --
  character(len=64) :: str
  ! -- BODY --
  call fll_getstring(def%iu_bin, def%c, str)
  write(6,'(3a)') "Header    : '",trim(str),"'"

endsubroutine fluent_get_header

!------------------------------------------------------------------------------!
!> @brief FLUENT : read section : space dimension
!------------------------------------------------------------------------------!
subroutine fluent_get_spcdim(def, umesh)

  implicit none
  ! -- INPUTS --
  type(st_deffluent) , intent(inout) :: def
  ! -- OUTPUTS --
  type(st_ustmesh)   , intent(inout) :: umesh
  ! -- Internal variables --
  integer :: ierr
  ! -- BODY --
  call fll_getint(def%iu_bin, def%c, def%spcdim, ierr)
  if ( ierr /= 0 ) then
    call cfd_error("could not read space dimension")
  endif
  if ( umesh%elemdim == 0 ) then
    continue
  elseif ( umesh%elemdim == def%spcdim ) then
    call cfd_warning("space dimension already set")
  else
    call cfd_warning("space dimension already set to " &
                     //trim(strof(umesh%elemdim))//" (will be reset)")
  endif
  umesh%elemdim = def%spcdim
  call fluent_check_spcdim(def%spcdim, 1)

endsubroutine fluent_get_spcdim

!------------------------------------------------------------------------------!
!> @brief FLUENT : read section : nodes
!------------------------------------------------------------------------------!
subroutine fluent_get_nodes(def, sctid, umesh)

  implicit none
  ! -- INPUTS --
  type(st_deffluent) , intent(inout) :: def
  integer            , intent(in)    :: sctid
  ! -- OUTPUTS --
  type(st_ustmesh)   , intent(inout) :: umesh
  ! -- Internal variables --
  integer           :: zoneid
  integer           :: inmin, inmax, ndim
  integer           :: itype
  integer           :: ityperef
  character(len=64) :: strzone
  character(len=64) :: strtype
  character(len=64) :: str, strref
  integer           :: i, inb
  integer           :: j
  integer(kfp)      :: ilist(5)
  integer           :: ierr
  ! -- BODY --

  !-----------------------------------------------
  ! Read parameters
  call fluent_read_intlist(def, '(z)', inb, ilist, str, ierr)
  if ( ierr /= 0 ) then
    call cfd_error("(fluent) could not read parameters of zones section")
  endif

  ! Check number of parameters
  if ( inb == 4 ) then
    ilist(5) = def%spcdim
  elseif ( inb /= 5 ) then
    call cfd_write("Error: string = '"//trim(str)//"'")
    call cfd_write("       expected number of fields = 5")
    call cfd_write("       actual   number of fields = "//trim(strof(inb)))
    call cfd_write("       wrong    number of fields !")
    call cfd_error("Stop")
  endif
  zoneid = ilist(1)
  inmin  = ilist(2)
  inmax  = ilist(3)
  itype  = ilist(4)
  ndim   = ilist(5)

  ! Print parameters
  if ( zoneid == 0 ) then
    write(strzone,'(a9)')    "all zones"
  else
    write(strzone,'(i3,1X,a1,z3.3,a1)') zoneid,"[",zoneid,"]"
    if ( ndim == 2 .or. def%spcdim == 2 ) then
      strtype = " x, y"
    elseif ( ndim == 2 .or. def%spcdim == 3 ) then
      strtype = " x, y, z"
    else
      strtype = ""
    endif
  endif
  write(str,'(3a,2(i8,a))') "(",strzone(1:9),"): (",inmin,":",inmax,")"
  call cfd_print("- reading FLUENT nodes "//trim(str)//trim(strtype))

  ! Check itype
  if ( zoneid == 0 ) then
    ityperef = 0
  else
    ityperef = 1
  endif
  if ( itype /= ityperef ) then
    call cfd_warning("(fluent) type"   //" = "//trim(strof(itype)) &
                        //" /= typeref"//" = "//trim(strof(ityperef)))
  endif

  ! Check space dimension
  if ( def%spcdim == 0 ) then
    if ( ndim == 0 ) then
      if ( zoneid == 0 ) then
        call cfd_warning("(fluent) spcdim = 0 and ndim = 0")
      else
        call cfd_error("(fluent) spcdim = 0 and ndim = 0")
      endif
    else
      if ( zoneid == 0 ) then
        call cfd_warning("(fluent) spcdim = 0 , set to ndim ="//trim(strof(ndim)))
        def%spcdim = ndim
        umesh%elemdim = def%spcdim
      else
        call cfd_warning("(fluent) spcdim = 0 , use ndim ="//trim(strof(ndim)))
      endif
    endif
  elseif ( ndim /= def%spcdim ) then
    call cfd_write("Error: spcdim = "//trim(strof(def%spcdim)))
    call cfd_write("       ndim   = "//trim(strof(ndim)))
    call cfd_error("wrong space dimension of zone"//trim(strzone))
  endif

  !-----------------------------------------------
  ! Case global parameters
  if ( zoneid == 0 ) then

    def%inmin = inmin
    def%inmax = inmax
    def%nnodes = inmax-inmin+1

    umesh%nvtex = inmax-inmin+1
    umesh%mesh%nvtex = umesh%nvtex
    umesh%mesh%idim = umesh%nvtex
    umesh%mesh%jdim = 1
    umesh%mesh%kdim = 1

    ! Allocate umesh vertices
    allocate(umesh%mesh%vertex(umesh%mesh%idim, umesh%mesh%jdim, umesh%mesh%kdim))

  !-----------------------------------------------
  ! Case specific zone
  else ! if ( zoneid /= 0 ) then

    ! Check allocation of vertex array
    if ( umesh%mesh%nvtex /= umesh%nvtex ) then
      call cfd_error("umesh%mesh%vertex is not allocated")
    elseif ( inmax > umesh%nvtex ) then
      call cfd_write("Error: nvtex = "//trim(strof(umesh%nvtex)))
      call cfd_write("       inmax = "//trim(strof(inmax))//" > nvtex")
      call cfd_error("wrong dimension of vertex array")
    endif

    ! Allocate local xyz array
    select case(sctid)
    case(fluent_sct_spbinnodes)
      allocate(def%xyzsp(ndim,inmax-inmin+1))
    case(fluent_sct_nodes, &
         fluent_sct_dpbinnodes)
      allocate(def%xyzdp(ndim,inmax-inmin+1))
    endselect

    ! Read opening parenthesis
    !!!! fll_checknextcharnostep instead of fll_checknextchar
    call fll_checknextcharnostep(def%iu_bin, def%c, '(')
    !!!! '(' is to be checked then leave next char unread

    ! Read vertices
    select case(sctid)
    ! Ascii values
    case(fluent_sct_nodes)
      !!!! next char was left unread
      read(def%iu_bin) def%c
      !!!! it is read now
      call fll_ignore_blank(def%iu_bin, def%c)
      do i = 1, inmax-inmin+1
        call fll_storeuptochar(def%iu_bin, def%c, nl_char, str)
        read(str,*) (def%xyzdp(j,i),j=1,ndim)
      enddo
    ! Binary values (simple precision)
    case(fluent_sct_spbinnodes)
      read(def%iu_bin) ((def%xyzsp(j,i),j=1,ndim),i=1,inmax-inmin+1)
      !!!! next char was left unread
      read(def%iu_bin) def%c
      !!!! it is read now
    ! Binary values (double precision)
    case(fluent_sct_dpbinnodes)
      read(def%iu_bin) ((def%xyzdp(j,i),j=1,ndim),i=1,inmax-inmin+1)
      !!!! next char was left unread
      read(def%iu_bin) def%c
      !!!! it is read now
    endselect
    call fll_checknextchar(def%iu_bin, def%c, ')')

    ! Transfer vertices
    select case(sctid)
    case(fluent_sct_spbinnodes)
      do i = inmin, inmax
        umesh%mesh%vertex(i,1,1)%x = def%xyzsp(1,i-inmin+1)
        umesh%mesh%vertex(i,1,1)%y = def%xyzsp(2,i-inmin+1)
        if ( ndim == 3 ) &
        umesh%mesh%vertex(i,1,1)%z = def%xyzsp(3,i-inmin+1)
      enddo
    case(fluent_sct_nodes, &
         fluent_sct_dpbinnodes)
      do i = inmin, inmax
        umesh%mesh%vertex(i,1,1)%x = def%xyzdp(1,i-inmin+1)
        umesh%mesh%vertex(i,1,1)%y = def%xyzdp(2,i-inmin+1)
        if ( ndim == 3 ) &
        umesh%mesh%vertex(i,1,1)%z = def%xyzdp(3,i-inmin+1)
      enddo
    endselect

    ! Deallocate local xyz array
    select case(sctid)
    case(fluent_sct_spbinnodes)
      deallocate(def%xyzsp)
    case(fluent_sct_nodes, &
         fluent_sct_dpbinnodes)
      deallocate(def%xyzdp)
    endselect

  endif

  ! Read end of section
  call fll_ignore_blank(def%iu_bin, def%c)
  call fll_storeuptocharnostep(def%iu_bin, def%c, ')', str)
  select case(sctid)
  case(fluent_sct_nodes)
    strref = " "
  case(fluent_sct_spbinnodes, &
       fluent_sct_dpbinnodes)
    write(strref,'(a,i04)') "End of Binary Section ",sctid
  endselect
  if ( trim(str) /= trim(strref) ) then
    call cfd_write("Error: expected : '"//trim(strref)//"'")
    call cfd_write("       found    : '"//trim(str)//"'")
    call cfd_error("wrong end of section")
  endif

endsubroutine fluent_get_nodes

!------------------------------------------------------------------------------!
!> @brief FLUENT : read section : cells
!------------------------------------------------------------------------------!
subroutine fluent_get_cells(def, sctid, umesh)

  implicit none
  ! -- INPUTS --
  type(st_deffluent) , intent(inout) :: def
  integer            , intent(in)    :: sctid
  ! -- OUTPUTS --
  type(st_ustmesh)   , intent(inout) :: umesh
  ! -- Internal variables --
  type(st_elemvtex) :: elem_new, elem_old
  type(st_flucellsection) :: cell_new, cell_old
  integer           :: zoneid
  integer           :: icmin, icmax
  integer           :: ctype
  integer           :: itype
  integer           :: ityperef
  character(len=64) :: strzone
  character(len=64) :: strtype
  character(len=64) :: str, strref
  integer           :: i, inb
  integer(kfp)      :: ilist(5)
  integer           :: nbcells(0:fluent_cell_types)
  integer           :: ictype, ityphontype
  integer           :: nelem, ielem, icell
  integer           :: nelem_old
  integer           :: ierr
  ! -- BODY --

  !-----------------------------------------------
  ! Read parameters
  call fluent_read_intlist(def, '(z)', inb, ilist, str, ierr)
  if ( ierr /= 0 ) then
    call cfd_error("(fluent) could not read parameters of cells section")
  endif

  ! Check number of parameters
  if ( inb == 4 ) then
    ilist(5) = 0
  elseif ( inb /= 5 ) then
    call cfd_write("Error: string = '"//trim(str)//"'")
    call cfd_write("       expected number of fields = 5")
    call cfd_write("       actual   number of fields = "//trim(strof(inb)))
    call cfd_write("       wrong    number of fields !")
    call cfd_error("Stop")
  endif
  zoneid = ilist(1)
  icmin  = ilist(2)
  icmax  = ilist(3)
  itype  = ilist(4)
  ctype  = ilist(5)

  ! Print parameters
  if ( zoneid == 0 ) then
    write(strzone,'(a9)')    "all zones"
    write(strtype,'()')
  else
    write(strzone,'(i3,1X,a1,z3.3,a1)') zoneid,"[",zoneid,"]"
    if ( ctype == fluent_cell_mixed ) then
      write(str,'(a)') "Mixed"
    else
      ityphontype = fluent2typhon_elemtype(ctype)
      if ( ityphontype == -1 ) then
        call cfd_error("(fluent) unknown cell type: "//trim(strof(ctype)))
      endif
      write(str,'(a)') name_element(fluent2typhon_elemtype(ctype))
    endif
    write(strtype,'(1X,4a)') str(1:len(name_element(-1))), &
                             " (fluent:",trim(fluent_cell_name(ctype)),")"
  endif
  write(str,'(3a,2(i8,a))') "(",strzone(1:9),"): (",icmin,":",icmax,")"
  call cfd_print("- reading FLUENT cells "//trim(str)//trim(strtype))

  ! Check itype
  if ( zoneid == 0 ) then
    ityperef = 0
  else
    ityperef = 1
  endif
  if ( itype /= ityperef ) then
    call cfd_warning("(fluent) type"   //" = "//trim(strof(itype)) &
                        //" /= typeref"//" = "//trim(strof(ityperef)))
  endif

  ! Check ctype
  ! will be checked in celltype array

  !-----------------------------------------------
  ! Case global parameters
  if ( zoneid == 0 ) then

    def%icmin = icmin
    def%icmax = icmax
    def%ncells = icmax-icmin+1

    !umesh%(???) = icmax-icmin+1
    !umesh%mesh%(???) = umesh%(???)
    !umesh%mesh%(??) = umesh%(???)
    !umesh%mesh%(??) = (??)
    !umesh%mesh%(??) = (??)

    ! Allocate def%celltype array
    allocate(def%celltype(icmin:icmax))
    allocate(def%ielem(icmin:icmax))
    allocate(def%icell(icmin:icmax))

  !-----------------------------------------------
  ! Case specific zone
  else ! if ( zoneid /= 0 ) then

  ! If mixed cells then a list follows
  if ( ctype == fluent_cell_mixed ) then

    ! Read opening parenthesis
    !!!! fll_checknextcharnostep instead of fll_checknextchar
    call fll_checknextcharnostep(def%iu_bin, def%c, '(')
    !!!! '(' is to be checked then leave next char unread

    ! Read cells
    select case(sctid)
    ! Ascii values
    case(fluent_sct_cells)
      !!!! next char was left unread
      read(def%iu_bin) def%c
      !!!! it is read now
      call fll_ignore_blank(def%iu_bin, def%c)
      i = icmin - 1
      do while ( i < icmax )
        call fll_storeuptochar(def%iu_bin, def%c, nl_char, str)
        call str_read_ints(str, '(i)', inb, def%celltype(i+1:icmax), ierr)
        if ( ierr /= 0 ) then
          call cfd_error("(fluent) could not read integer list")
        endif
        i = i + inb
      enddo
    ! Binary values (simple precision)
    case(fluent_sct_spbincells)
      read(def%iu_bin) (def%celltype(i),i=icmin,icmax)
      !!!! next char was left unread
      read(def%iu_bin) def%c
      !!!! it is read now
    endselect
    call fll_checknextchar(def%iu_bin, def%c, ')')

  ! If not mixed cells
  else ! if ( ctype /= fluent_cell_mixed ) then
    do i = icmin, icmax
      def%celltype(i) = ctype
    enddo

  ! End of test on ctype
  endif

    ! Transfer cells

    !-----------------------------------------------
    ! Count number of cells per section type
    !-----------------------------------------------
    ! Initialize nbcells
    nbcells(0:fluent_cell_types) = 0
    ! If mixed cells : check cell types
    if ( ctype == fluent_cell_mixed ) then
      do i = icmin, icmax
        ictype = def%celltype(i)
        ityphontype = fluent2typhon_elemtype(ictype)
        if ( ityphontype == -1 ) then
          call cfd_error("(fluent) unknown cell type: "//trim(strof(ictype)))
        endif
        nbcells(ictype) = nbcells(ictype) + 1
      enddo
    ! If not mixed cells : check done only count
    else ! if ( ctype /= fluent_cell_mixed ) then
      nbcells(ctype) = icmax - icmin + 1
    endif
    !-----------------------------------------------
    ! Create or extend sections
    !-----------------------------------------------
    ! Loop on all fluent cell types
    do ictype = 0, fluent_cell_types
      ! Test fluent cell type is present
      if ( nbcells(ictype) > 0 ) then
        nelem = nbcells(ictype)
        ityphontype = fluent2typhon_elemtype(ictype)
        ! Check section already exists
        do ielem = 1, umesh%cellvtex%nsection
          if ( umesh%cellvtex%elem(ielem)%elemtype == ityphontype ) exit
        enddo
        ! Extend section if section already exists
        if ( ielem <= umesh%cellvtex%nsection ) then
          call cfd_print("  . extend existing element section:  "//strofr(nelem,16) &
                              //"  "//name_element(ityphontype) &
                              //" (fluent:"//trim(fluent_cell_name(ictype))//")")
          ! copy old section to new section
          elem_old = umesh%cellvtex%elem(ielem)
          cell_old = def%cellsect(ielem)
          nelem_old = elem_old%nelem
!          write(6,*) "re-allocated elem(",ielem,")" &
!                     //"%elemvtex(",nelem_old+nelem,",",elem_new%nvtex,")"
          call new_elemvtex(elem_new, nelem_old+nelem, ityphontype)
          call fluent_newcellsection(cell_new, ictype, nelem_old+nelem, &
                                     nface_element(ityphontype), &
                                     nvtexperface_element(ityphontype))
          elem_new%ielem(1:nelem_old) = elem_old%ielem(1:nelem_old)
          ! pointer to copy
          umesh%cellvtex%elem(ielem) = elem_new
          def%cellsect(ielem) = cell_new
          ! delete old section
          call delete_elemvtex(elem_old)
          call fluent_delcellsection(cell_old)
        ! Create section if section does not already exist
        else ! if ( ielem == umesh%cellvtex%nsection + 1 ) then
          call cfd_print("  . create new      element section:  "//strofr(nelem,16) &
                              //"  "//name_element(ityphontype) &
                              //" (fluent:"//trim(fluent_cell_name(ictype))//")")
          call addelem_genelemvtex(umesh%cellvtex)
!!call readmem
          call fluent_addcellsection(def)
!!call readmem
          ielem = umesh%cellvtex%nsection
          call new_elemvtex(umesh%cellvtex%elem(ielem), nelem, ityphontype)
!          write(6,*) "allocated    elem(",ielem,")" &
!                     //"%elemvtex(",nelem,",",umesh%cellvtex%elem(ielem)%nvtex,")"
          call fluent_newcellsection(def%cellsect(ielem), ictype, nelem, &
                                     nface_element(ityphontype), &
                                     nvtexperface_element(ityphontype))
          nelem_old = 0
        endif
        ! Fill sections
        ! If mixed cells : check cell types
        if ( ctype == fluent_cell_mixed ) then
          icell = nelem_old
          do i = icmin, icmax
            if ( ictype == def%celltype(i) ) then
              icell = icell + 1
              umesh%cellvtex%elem(ielem)%ielem(icell) = i
!              write(6,*) "def%ielem(",i,") =",ielem,"  %icell =",icell
              def%ielem(i) = ielem
              def%icell(i) = icell ! i: absolute cell number ; icell: cell number in section ielem
            endif
          enddo
        ! If not mixed cells : check done only set
        else ! if ( ctype /= fluent_cell_mixed ) then
          do i = icmin, icmax
            umesh%cellvtex%elem(ielem)%ielem(nelem_old+i-icmin+1) = i
            icell = nelem_old+i-icmin+1
!            write(6,*) "def%ielem(",i,") =",ielem,"  %icell =",icell
            def%ielem(i) = ielem
            def%icell(i) = icell ! i: absolute cell number ; icell: cell number in section ielem
          enddo
        endif
      ! End of test fluent cell type is present
      endif
    ! End of loop on all fluent cell types
    enddo

  endif

  ! Read end of section
  call fll_ignore_blank(def%iu_bin, def%c)
  call fll_storeuptocharnostep(def%iu_bin, def%c, ')', str)
  select case(sctid)
  case(fluent_sct_cells)
    strref = " "
  case(fluent_sct_spbincells)
    write(strref,'(a,i04)') "End of Binary Section ",sctid
  endselect
  if ( trim(str) /= trim(strref) ) then
    call cfd_write("Error: expected : '"//trim(strref)//"'")
    call cfd_write("       found    : '"//trim(str)//"'")
    call cfd_error("wrong end of section")
  endif

endsubroutine fluent_get_cells

!------------------------------------------------------------------------------!
!> @brief FLUENT : read section : faces
!------------------------------------------------------------------------------!
subroutine fluent_get_faces(def, sctid)

  implicit none
  ! -- INPUTS --
  type(st_deffluent) , intent(inout) :: def
  integer            , intent(in)    :: sctid
  ! -- OUTPUTS --
  ! -- Internal variables --
  integer           :: zoneid
  integer           :: ifmin, ifmax, nfaces
  integer           :: bctype
  integer           :: bctyperef
  integer           :: ftype
  integer           :: ftyperef
  character(len=64) :: strzone
  character(len=96) :: str, strref, strtmp
  !!! ALL THIS IS TO READ FASTER CHAR BY CHAR...
  integer,parameter :: bkndot =   16, bknpct = 4, bknlin = 8
  integer,parameter :: bksize = 1024
  integer,parameter :: bksdot = bksize*bkndot
  integer,parameter :: bkspct = bksize*bkndot*bknpct
  integer,parameter :: bkslin = bksize*bkndot*bknpct*bknlin
  integer           :: bkprnl
  character(len=64) :: bkstrl(1:bksize)
  integer           :: bkread
  integer           :: bkidot, bkipct, bkilin
  !!!... YES UP TO HERE
  integer           :: max_nvtexface
  integer(kfp)      :: ftab(64)
  integer           :: i, inb, j, itabmin, ntab, ntabref
  integer           :: ic, icell, ielem, nface
  integer(kfp)      :: ilist(5)
  integer           :: nvtex
  integer           :: ncell
  integer           :: nbfils
  integer           :: ierr
  integer, allocatable :: nvtexface(:)
  integer, allocatable :: lvtexface(:,:)
  integer, allocatable :: cellface(:,:)
  type(st_connect), pointer :: facevtexhdle
  type(st_genconnect), pointer :: fvtxmixdhdle
  ! -- BODY --

  !-----------------------------------------------
  ! Read parameters
  call fluent_read_intlist(def, '(z)', inb, ilist, str, ierr)
  if ( ierr /= 0 ) then
    call cfd_error("(fluent) could not read parameters of faces section")
  endif

  ! Check number of parameters
  if ( inb == 4 ) then
    ilist(5) = 0
  elseif ( inb /= 5 ) then
    call cfd_write("Error: string = '"//trim(str)//"'")
    call cfd_write("       expected number of fields = 5")
    call cfd_write("       actual   number of fields = "//trim(strof(inb)))
    call cfd_write("       wrong    number of fields !")
    call cfd_error("Stop")
  endif
  zoneid = ilist(1)
  ifmin  = ilist(2)
  ifmax  = ilist(3)
  bctype = ilist(4)
  ftype  = ilist(5)

  ! Print parameters
  if ( zoneid == 0 ) then
    write(strzone,'(a9)')    "all zones"
  else
    write(strzone,'(i3,1X,a1,z3.3,a1)') zoneid,"[",zoneid,"]"
  endif
  write(str,'(3a,2(i8,a))') "(",strzone(1:9),"): (",ifmin,":",ifmax,")"
  strtmp = " "
  if ( def%zonename /= "" ) then
    write(strtmp,'(3a)') '# "',trim(def%zonename),'"'
  endif
  write(str,'(a,a,z2.2,4a)') trim(str)," [",bctype,"] ",strofl(fluent_face_name(ftype),6)," ",trim(strtmp)
  call cfd_print("- reading FLUENT faces "//trim(str))

  ! Check bctype
  if ( zoneid == 0 ) then
    bctyperef = 0
  else
    bctyperef = bctype
  endif
  if ( bctype /= bctyperef ) then
    call cfd_warning("(fluent) bctype"   //" = "//trim(strof(bctype)) &
                        //" /= bctyperef"//" = "//trim(strof(bctyperef)))
  endif

  ! Check ftype
  if ( zoneid == 0 ) then
    ftyperef = 0
  else
    ftyperef = ftype
  endif
  if ( ftype /= ftyperef ) then
    call cfd_warning("(fluent) ftype"   //" = "//trim(strof(ftype)) &
                        //" /= ftyperef"//" = "//trim(strof(ftyperef)))
  endif

  if ( ftype == fluent_faces_polyg ) then
    call cfd_error("(fluent) polygonal faces read procedure was not implemented")
    !call cfd_warning("(fluent) polygonal faces read procedure was not checked")
  endif

  !-----------------------------------------------
  ! Case global parameters
  if ( zoneid == 0 ) then

    def%ifmin = ifmin
    def%ifmax = ifmax
    def%nfaces = ifmax-ifmin+1

    !umesh%(???) = ifmax-ifmin+1
    !umesh%mesh%(???) = umesh%(???)
    !umesh%mesh%(??) = umesh%(???)
    !umesh%mesh%(??) = (??)
    !umesh%mesh%(??) = (??)

    ! Allocate umesh vertices
    !allocate(umesh%mesh%vertex(umesh%mesh%idim, umesh%mesh%jdim, umesh%mesh%kdim))

    def%nfacevtex = 0
    allocate(def%facevtexhead)
    def%facevtextail => def%facevtexhead

  !-----------------------------------------------
  ! Case specific zone
  else ! if ( zoneid /= 0 ) then

    nfaces = ifmax-ifmin+1

    ! Create bc zone
    call fluent_addbczone(def)
    call fluent_newbczone(def%bczone(def%nbczone), def%zonename, zoneid, bctype, &
                          ifmin, ifmax, nfaces)
    def%zonename = ""

    allocate(nvtexface(1:nfaces))
    allocate(lvtexface(1:nfaces,4))
    allocate(cellface (1:nfaces,2))

  !if ( ftype == fluent_faces_mixed ) then
    ! Read opening parenthesis
    !!!! fll_checknextcharnostep instead of fll_checknextchar
    call fll_checknextcharnostep(def%iu_bin, def%c, '(')
    !!!! '(' is to be checked then leave next char unread

!    call cfd_warning("Faces are read but NOT STORED !!!")
    ! Read faces
    select case(sctid)
    ! Ascii values
    case(fluent_sct_faces)
      !!!! next char was left unread
      read(def%iu_bin) def%c
      !!!! it is read now
      call fll_ignore_blank(def%iu_bin, def%c)

      !!! ALL THIS IS TO READ FASTER CHAR BY CHAR...
      ! Define sizes for print
      bkidot = bksdot
      bkipct = bkspct
      bkilin = bkslin
      bkprnl = 0
      i = 0
      do while ( i < nfaces )
        ! Define size of block to read
        bkread = min(nfaces-i,bksize)
        ! Read block
        do j = 1, bkread
          call fll_storeuptochar(def%iu_bin, def%c, nl_char, bkstrl(j))
        enddo
        ! Print dots and line
        if ( i >= bkidot ) then
          call bkprintdotsln(i, bkidot, bksdot, bkprnl, 1, '(1H.,$)')
          call bkprintdotsln(i, bkipct, bkspct, bkprnl, 1, '(i2.2,1H%,$)', &
                             int((100.*(i+bkread))/nfaces+.5))
          call bkprintdotsln(i, bkilin, bkslin, bkprnl, 0, '()')
        endif
        ! Process data read
        do j = 1, bkread
          call str_read_ints(bkstrl(j), '(z)', ntab, ftab, ierr)
          if ( ierr /= 0 ) then
            call cfd_error("(fluent) could not read integer list")
          endif
          if ( ftype == fluent_faces_mixed ) then
            nvtex = ftab(1)
            itabmin = 2
          else
            nvtex = ftype
            itabmin = 1
          endif
          ntabref = itabmin+nvtex+1
          if ( ntab /= ntabref ) then
            call cfd_write("Error: string = '"//trim(bkstrl(j))//"'")
            call cfd_write("       expected number of integers = "//trim(strof(ntabref)))
            call cfd_write("       actual   number of integers = "//trim(strof(ntab)))
            call cfd_write("       wrong    number of integers !")
            call cfd_error("Stop")
          endif
          nvtexface(i+j) = nvtex
          lvtexface(i+j,1:nvtex) = ftab(itabmin      :itabmin+nvtex-1)
          cellface (i+j,1:2)     = ftab(itabmin+nvtex:itabmin+nvtex+1)
        enddo
        i = i + bkread
      stropt = " "
      indopt = 1
      enddo
      !if ( bkilin-bkidot /= bksdot*(bknlin-1) ) then
      if ( bkprnl == 1 ) then
        write(6,'(i3,1H%)') 100 ; call flush(6)
      endif
      !!!... YES UP TO HERE

      !do i = ifmin, ifmax
      !  call fll_storeuptochar(def%iu_bin, def%c, nl_char, str)
      !  call str_read_ints(str, '(z)', ntab, ftab, ierr)
      !  if ( ierr /= 0 ) then
      !    call cfd_error("(fluent) could not read integer list")
      !  endif
      !stropt = " "
      !indopt = 1
      !enddo

    ! Binary values (simple precision)
    case(fluent_sct_spbinfaces)
      if ( ftype == fluent_faces_mixed ) then
        read(def%iu_bin) &
          ((nvtexface(i), &
            lvtexface(i,1:nvtexface(i)), cellface (i,1:2)),i=1,nfaces)
      else
        nvtexface(1:nfaces) = ftype
        read(def%iu_bin) &
          ((lvtexface(i,1:nvtexface(i)), cellface (i,1:2)),i=1,nfaces)
      endif
      !!!! next char was left unread
      read(def%iu_bin) def%c
      !!!! it is read now
    endselect
    call fll_checknextchar(def%iu_bin, def%c, ')')
  !endif

    max_nvtexface = 0
    ! Transfer faces to cells
    do i = 1, nfaces
      do ic = 1, 2
        icell = cellface(i,ic)
        if ( icell /= 0 ) then
          ielem = def%ielem(icell)
          ncell = def%icell(icell)
          nface = def%cellsect(ielem)%dface(ncell) + 1
          def%cellsect(ielem)%dface(ncell) = nface
          def%cellsect(ielem)%face(ncell,nface) = i
          def%cellsect(ielem)%nvtx(ncell,nface) = nvtexface(i)
          def%cellsect(ielem)%vtex(ncell,nface,1:nvtexface(i)) = lvtexface(i,1:nvtexface(i))
          max_nvtexface = max(max_nvtexface, nvtexface(i))
        endif
      enddo
    enddo

    if ( ftype == fluent_faces_mixed ) then
      allocate(def%bczone(def%nbczone)%fvtxmixd)
      fvtxmixdhdle => def%bczone(def%nbczone)%fvtxmixd
      call new_genconnect_arraynelem(fvtxmixdhdle, nfaces, nvtexface)
      !write(6,*) 'bczone = ',def%nbczone
      !write(6,*) 'nbndes = ',nfaces
      !write(6,*) 'nbfils = (array)'
      call flush(6)
      do i = 1, nfaces
        nbfils = nvtexface(i)
        call new_elemc(fvtxmixdhdle%node(i), nbfils)
        fvtxmixdhdle%node(i)%elem(1:nbfils) = lvtexface(i,1:nbfils)
      enddo
    else
      allocate(def%bczone(def%nbczone)%facevtex)
      facevtexhdle => def%bczone(def%nbczone)%facevtex
      nbfils = max_nvtexface
      call new_connect(facevtexhdle,nfaces,nbfils)
      !write(6,*) 'bczone = ',def%nbczone
      !write(6,*) 'nbndes = ',nfaces
      !write(6,*) 'nbfils = ',nbfils
      call flush(6)
      facevtexhdle%fils(1:nfaces,1:nbfils) = lvtexface(1:nfaces,1:nbfils)
    endif

    def%max_nvtexface = max(def%max_nvtexface, max_nvtexface)

  endif

  ! Read end of section
  call fll_ignore_blank(def%iu_bin, def%c)
  call fll_storeuptocharnostep(def%iu_bin, def%c, ')', str)
  select case(sctid)
  case(fluent_sct_faces)
    strref = " "
  case(fluent_sct_spbinfaces)
    write(strref,'(a,i04)') "End of Binary Section ",sctid
    ! "Section" may be followed by two spaces...
    ! (for "2013" in tested msh files)
    if ( str(22:23) == "  " ) then
      strref(23:) = strref(22:)
    endif
  endselect
  if ( trim(str) /= trim(strref) ) then
    call cfd_write("Error: expected : '"//trim(strref)//"'")
    call cfd_write("       found    : '"//trim(str)//"'")
    call cfd_error("wrong end of section")
  endif

  contains

  subroutine bkprintdotsln(i, bkidtl, bkstep, bkprnl, bkval, bkform, bkint)
  implicit none
  integer , intent(IN)    :: i, bkstep, bkval
  integer , intent(IN) , optional :: bkint
  integer , intent(INOUT) :: bkidtl
  integer , intent(OUT)   :: bkprnl
  character(len=*) , intent(IN) :: bkform
  ! -- BODY --
  if ( i >= bkidtl ) then
    if ( present(bkint) ) then
    write(6,bkform) bkint ; call flush(6)
    else
    write(6,bkform) ; call flush(6)
    endif
    bkidtl = bkidtl + bkstep
    bkprnl = bkval
  endif
  endsubroutine bkprintdotsln

endsubroutine fluent_get_faces

!------------------------------------------------------------------------------!
!> @brief FLUENT : add and check vertex to cell
!------------------------------------------------------------------------------!
logical function addcheckvtex(cvtex, fvtex)

  implicit none
  ! -- INPUTS --
  integer           , intent(inout) :: cvtex
  integer           , intent(in)    :: fvtex
  ! -- OUTPUTS --
  ! -- Internal variables --
  ! -- BODY --

  addcheckvtex = .true.
  ! If empty: fill
  if ( cvtex == 0 ) then
    cvtex = fvtex
  ! If not empty: check
  elseif ( cvtex /= fvtex ) then
    addcheckvtex = .false.
  endif

endfunction addcheckvtex

!------------------------------------------------------------------------------!
!> @brief FLUENT : locate tri nodes
!------------------------------------------------------------------------------!
subroutine fluent2typhon_locate_trinodes(def, isect, umesh)

  implicit none
  ! -- INPUTS --
  type(st_deffluent) , intent(inout) :: def
  integer            , intent(in)    :: isect
  ! -- OUTPUTS --
  type(st_ustmesh)   , intent(inout) :: umesh
  ! -- Internal variables --
  type(st_flucellsection) :: celllist
  integer(kip), pointer :: umesh_vtex(:,:)
  integer           :: icell, iface
  integer           :: icvn, icvp
  integer           :: ifvn, ifvp
  integer           :: cvtex(3)
  integer           :: fvtex(2)
  logical           :: isok
  integer           :: errortag
  character(len=16) :: str
  ! -- BODY --

    celllist = def%cellsect(isect)

    umesh_vtex => umesh%cellvtex%elem(isect)%elemvtex

    ! Loop on cells
    do icell = 1, celllist%ncell

      isok = .true.
      iface = 1
      ! Arbitrarily select first face
      cvtex(1:2) = celllist%vtex(icell,iface,:)
!      write(6,'(a,2i4)')  "cvtex: ",cvtex(1:2)
      cvtex(3) = 0
      umesh_vtex(icell,:) = cvtex(1:2)
      ! Loop on 2 other faces
      faceloop: &
      do iface = 2, 3
        fvtex(1:2) = celllist%vtex(icell,iface,:)
!      write(6,'(a,2i4)')  "fvtex: ",fvtex(1:2)
        ! Loop on face vertices to be located
        ifvloop: &
        do ifvn = 1, 2
          ifvp = 1 + mod(ifvn,2)
         ! Loop on cell vertices to be found
         icvloop: &
         do icvn = 1, 2
          icvp = 1 + mod(icvn,2)
!          write(6,'(i4,a,i4)')  fvtex(ifvn),"==",cvtex(icvn)
          if ( fvtex(ifvn) == cvtex(icvn) ) then
            isok = isok .and. addcheckvtex(umesh_vtex(icell,3), fvtex(ifvp))
            errortag = 1
            exit ifvloop
          endif
         ! End loop on cell vertices to be found
         enddo icvloop
         ! No "base" face cell vertices matched : this is the "top" face
         ! It is ignored (although it should be checked)
        ! End loop on face vertices to be located
        enddo ifvloop
        if ( .not. isok ) exit faceloop
      ! End loop on 5 other faces
      enddo faceloop

      if ( .not. isok ) then
        call cfd_print("errortag: "//strofr(errortag,4))
        call cfd_print("in section: "//strofr(isect,4))
        call cfd_print("of type:    fluent:"//trim(fluent_cell_name(celllist%celltype)))
        call cfd_print("with cell:  "//strofr(icell,8))
        str = "vertices:"
        do icvn = 1, 3
          call cfd_print(str(1:12)//strofr(cvtex(icvn),10))
          str = " "
        enddo
        do iface = 1, 3
          call cfd_print("face "//trim(strof(iface))//":" &
                         //strofr(celllist%vtex(icell,iface,1),10) &
                         //strofr(celllist%vtex(icell,iface,2),10) &
                         )
        enddo
        call cfd_error("Cell cannot be reconstructed")
      endif

    ! End loop on cells
    enddo

endsubroutine fluent2typhon_locate_trinodes

!------------------------------------------------------------------------------!
!> @brief FLUENT : locate tetra nodes
!------------------------------------------------------------------------------!
subroutine fluent2typhon_locate_tetranodes(def, isect, umesh)

  implicit none
  ! -- INPUTS --
  type(st_deffluent) , intent(inout) :: def
  integer            , intent(in)    :: isect
  ! -- OUTPUTS --
  type(st_ustmesh)   , intent(inout) :: umesh
  ! -- Internal variables --
  type(st_flucellsection) :: celllist
  integer(kip), pointer :: umesh_vtex(:,:)
  integer           :: icell, iface
  integer           :: icvn, icvp, icvm
  integer           :: ifvn, ifvp, ifvm
  integer           :: cvtex(4)
  integer           :: fvtex(3)
  logical           :: isok
  integer           :: errortag
  character(len=16) :: str
  ! -- BODY --

    celllist = def%cellsect(isect)

    umesh_vtex => umesh%cellvtex%elem(isect)%elemvtex

    ! Loop on cells
    do icell = 1, celllist%ncell

      isok = .true.
      iface = 1
      ! Arbitrarily select first face
      cvtex(1:3) = celllist%vtex(icell,iface,:)
!      write(6,'(a,2i4)')  "cvtex: ",cvtex(1:2)
      cvtex(4) = 0
      umesh_vtex(icell,:) = cvtex(1:3)
      ! Loop on 3 other faces
      faceloop: &
      do iface = 2, 4
        fvtex(1:3) = celllist%vtex(icell,iface,:)
!      write(6,'(a,3i4)')  "fvtex: ",fvtex(1:3)
        ! Loop on face vertices to be located
        ifvloop: &
        do ifvn = 1, 3
          ifvp = 1 + mod(ifvn,2)
          ifvm = 1 + mod(ifvp,2)
         ! Loop on cell vertices to be found
         icvloop: &
         do icvn = 1, 2
          icvp = 1 + mod(icvn,2)
          icvm = 1 + mod(icvp,2)
!          write(6,'(i4,a,i4)')  fvtex(ifvn),"==",cvtex(icvn)
          if ( fvtex(ifvn) == cvtex(icvn) ) then
            if     ( fvtex(ifvp) == cvtex(icvp) ) then
              isok = isok .and. addcheckvtex(umesh_vtex(icell,4), fvtex(ifvm))
              errortag = 1
              exit ifvloop
            elseif ( fvtex(ifvp) == cvtex(icvm) ) then
              isok = isok .and. addcheckvtex(umesh_vtex(icell,4), fvtex(ifvm))
              errortag = 2
              exit ifvloop
            elseif ( fvtex(ifvm) == cvtex(icvp) ) then
              isok = isok .and. addcheckvtex(umesh_vtex(icell,4), fvtex(ifvp))
              errortag = 3
              exit ifvloop
            elseif ( fvtex(ifvm) == cvtex(icvm) ) then
              isok = isok .and. addcheckvtex(umesh_vtex(icell,4), fvtex(ifvp))
              errortag = 4
              exit ifvloop
            endif
          endif
         ! End loop on cell vertices to be found
         enddo icvloop
         ! No "base" face cell vertices matched : this is the "top" face
         ! It is ignored (although it should be checked)
        ! End loop on face vertices to be located
        enddo ifvloop
        if ( .not. isok ) exit faceloop
      ! End loop on 5 other faces
      enddo faceloop

      if ( .not. isok ) then
        call cfd_print("errortag: "//strofr(errortag,4))
        call cfd_print("in section: "//strofr(isect,4))
        call cfd_print("of type:    fluent:"//trim(fluent_cell_name(celllist%celltype)))
        call cfd_print("with cell:  "//strofr(icell,8))
        str = "vertices:"
        do icvn = 1, 4
          call cfd_print(str(1:12)//strofr(cvtex(icvn),10))
          str = " "
        enddo
        do iface = 1, 4
          call cfd_print("face "//trim(strof(iface))//":" &
                         //strofr(celllist%vtex(icell,iface,1),10) &
                         //strofr(celllist%vtex(icell,iface,2),10) &
                         //strofr(celllist%vtex(icell,iface,3),10) &
                         )
        enddo
        call cfd_error("Cell cannot be reconstructed")
      endif

    ! End loop on cells
    enddo

endsubroutine fluent2typhon_locate_tetranodes

!------------------------------------------------------------------------------!
!> @brief FLUENT : locate quad nodes
!------------------------------------------------------------------------------!
subroutine fluent2typhon_locate_quadnodes(def, isect, umesh)

  implicit none
  ! -- INPUTS --
  type(st_deffluent) , intent(inout) :: def
  integer            , intent(in)    :: isect
  ! -- OUTPUTS --
  type(st_ustmesh)   , intent(inout) :: umesh
  ! -- Internal variables --
  type(st_flucellsection) :: celllist
  integer(kip), pointer :: umesh_vtex(:,:)
  integer           :: icell, iface
  integer           :: icvn, icvp
  integer           :: ifvn, ifvp
  integer           :: cvtex(4)
  integer           :: fvtex(2)
  logical           :: isok
  integer           :: errortag
  character(len=16) :: str
  ! -- BODY --

    celllist = def%cellsect(isect)

    umesh_vtex => umesh%cellvtex%elem(isect)%elemvtex

    ! Loop on cells
    do icell = 1, celllist%ncell

      isok = .true.
      iface = 1
      ! Arbitrarily select first face
      cvtex(1:2) = celllist%vtex(icell,iface,:)
!      write(6,'(a,2i4)')  "cvtex: ",cvtex(1:2)
      cvtex(3:4) = 0
      umesh_vtex(icell,:) = cvtex(1:2)
      ! Loop on 3 other faces
      faceloop: &
      do iface = 2, 4
        fvtex(1:2) = celllist%vtex(icell,iface,:)
!      write(6,'(a,2i4)')  "fvtex: ",fvtex(1:2)
        ! Loop on face vertices to be located
        ifvloop: &
        do ifvn = 1, 2
          ifvp = 1 + mod(ifvn,2)
         ! Loop on cell vertices to be found
         icvloop: &
         do icvn = 1, 2
          icvp = 1 + mod(icvn,2)
          if ( fvtex(ifvn) == cvtex(icvn) ) then
            isok = isok .and. addcheckvtex(umesh_vtex(icell,icvp+2), fvtex(ifvp))
            errortag = 1
            exit ifvloop
          endif
         ! End loop on cell vertices to be found
         enddo icvloop
         ! No "base" face cell vertices matched : this is the "top" face
         ! It is ignored (although it should be checked)
        ! End loop on face vertices to be located
        enddo ifvloop
        if ( .not. isok ) exit faceloop
      ! End loop on 5 other faces
      enddo faceloop

      if ( .not. isok ) then
        call cfd_print("errortag: "//strofr(errortag,4))
        call cfd_print("in section: "//strofr(isect,4))
        call cfd_print("of type:    fluent:"//trim(fluent_cell_name(celllist%celltype)))
        call cfd_print("with cell:  "//strofr(icell,8))
        str = "vertices:"
        do icvn = 1, 4
          call cfd_print(str(1:12)//strofr(cvtex(icvn),10))
          str = " "
        enddo
        do iface = 1, 4
          call cfd_print("face "//trim(strof(iface))//":" &
                         //strofr(celllist%vtex(icell,iface,1),10) &
                         //strofr(celllist%vtex(icell,iface,2),10) &
                         )
        enddo
        call cfd_error("Cell cannot be reconstructed")
      endif

    ! End loop on cells
    enddo

endsubroutine fluent2typhon_locate_quadnodes

!------------------------------------------------------------------------------!
!> @brief FLUENT : locate hexa nodes
!------------------------------------------------------------------------------!
subroutine fluent2typhon_locate_hexanodes(def, isect, umesh)

  implicit none
  ! -- INPUTS --
  type(st_deffluent) , intent(inout) :: def
  integer            , intent(in)    :: isect
  ! -- OUTPUTS --
  type(st_ustmesh)   , intent(inout) :: umesh
  ! -- Internal variables --
  type(st_flucellsection) :: celllist
  integer(kip), pointer :: umesh_vtex(:,:)
  integer           :: icell, iface
  integer           :: icvn, icvp, icvq, icvm
  integer           :: ifvn, ifvp, ifvq, ifvm
  integer           :: cvtex(8)
  integer           :: fvtex(4)
  logical           :: isok
  integer           :: errortag
  character(len=16) :: str
  ! -- BODY --

    celllist = def%cellsect(isect)

    umesh_vtex => umesh%cellvtex%elem(isect)%elemvtex

    ! Loop on cells
    do icell = 1, celllist%ncell

      isok = .true.
      iface = 1
      ! Arbitrarily select first face
      cvtex(1:4) = celllist%vtex(icell,iface,:)
!      write(6,'(a,4i4)')  "cvtex: ",cvtex(1:4)
      cvtex(5:8) = 0
      umesh_vtex(icell,:) = cvtex(1:4)
      ! Loop on 5 other faces
      faceloop: &
      do iface = 2, 6
        fvtex(1:4) = celllist%vtex(icell,iface,:)
!      write(6,'(a,4i4)')  "fvtex: ",fvtex(1:4)
        ! Loop on face vertices to be located
        ifvloop: &
        do ifvn = 1, 4
          ifvp = 1 + mod(ifvn,4)
          ifvq = 1 + mod(ifvp,4)
          ifvm = 1 + mod(ifvq,4)
         ! Loop on cell vertices to be found
         icvloop: &
         do icvn = 1, 4
          icvp = 1 + mod(icvn,4)
          icvq = 1 + mod(icvp,4)
          icvm = 1 + mod(icvq,4)
!          write(6,'(i4,a,i4)')  fvtex(ifvn),"==",cvtex(icvn)
!          write(6,'(2x,i4,a,i4)') fvtex(ifvp),"==",cvtex(icvp)
!          write(6,'(2x,i4,a,i4)') fvtex(ifvp),"==",cvtex(icvm)
!          write(6,'(2x,i4,a,i4)') fvtex(ifvm),"==",cvtex(icvp)
!          write(6,'(2x,i4,a,i4)') fvtex(ifvm),"==",cvtex(icvm)
          if ( fvtex(ifvn) == cvtex(icvn) ) then
            ! Face 1265
            if     ( fvtex(ifvp) == cvtex(icvp) ) then
!            write(6,*) "icell =",icell, "icvp+4 =",icvp+4, "ifvq =", ifvq
!            write(6,*) "fvtex(",ifvq,") =",fvtex(ifvq) ; call flush(6)
              isok = isok .and. addcheckvtex(umesh_vtex(icell,icvp+4), fvtex(ifvq))     !           fq
              isok = isok .and. addcheckvtex(umesh_vtex(icell,icvn+4), fvtex(ifvm))     !        fm |
              errortag = 1                                                              !     cq-|--cp=fp
              exit ifvloop                                                              !  cm----cn=fn
            ! Face 1485
            elseif ( fvtex(ifvp) == cvtex(icvm) ) then
              isok = isok .and. addcheckvtex(umesh_vtex(icell,icvm+4), fvtex(ifvq))     !           fm
              isok = isok .and. addcheckvtex(umesh_vtex(icell,icvn+4), fvtex(ifvm))     !        fq |
              errortag = 2                                                              !     cp-|--cn=fn
              exit ifvloop                                                              !  cq----cm=fp
            ! Face 1562
            elseif ( fvtex(ifvm) == cvtex(icvp) ) then
              isok = isok .and. addcheckvtex(umesh_vtex(icell,icvp+4), fvtex(ifvq))     !           fq
              isok = isok .and. addcheckvtex(umesh_vtex(icell,icvn+4), fvtex(ifvp))     !        fp |
              errortag = 3                                                              !     cq-|--cp=fm
              exit ifvloop                                                              !  cm----cn=fn
            ! Face 1584
            elseif ( fvtex(ifvm) == cvtex(icvm) ) then
              isok = isok .and. addcheckvtex(umesh_vtex(icell,icvm+4), fvtex(ifvq))     !           fp
              isok = isok .and. addcheckvtex(umesh_vtex(icell,icvn+4), fvtex(ifvp))     !        fq |
              errortag = 4                                                              !     cp-|--cn=fn
              exit ifvloop                                                              !  cq----cm=fm
            ! Face wrongly defined
            else
              isok = .false.
              errortag = 5
              exit ifvloop
            endif
          endif
         ! End loop on cell vertices to be found
         enddo icvloop
         ! No "base" face cell vertices matched : this is the "top" face
         ! It is ignored (although it should be checked)
        ! End loop on face vertices to be located
        enddo ifvloop
        if ( .not. isok ) exit faceloop
      ! End loop on 5 other faces
      enddo faceloop

      if ( .not. isok ) then
        call cfd_print("errortag: "//strofr(errortag,4))
        call cfd_print("in section: "//strofr(isect,4))
        call cfd_print("of type:    fluent:"//trim(fluent_cell_name(celllist%celltype)))
        call cfd_print("with cell:  "//strofr(icell,8))
        str = "vertices:"
        do icvn = 1, 8
          call cfd_print(str(1:12)//strofr(cvtex(icvn),10))
          str = " "
        enddo
        do iface = 1, 6
          call cfd_print("face "//trim(strof(iface))//":" &
                         //strofr(celllist%vtex(icell,iface,1),10) &
                         //strofr(celllist%vtex(icell,iface,2),10) &
                         //strofr(celllist%vtex(icell,iface,3),10) &
                         //strofr(celllist%vtex(icell,iface,4),10) &
                         )
        enddo
        call cfd_error("Cell cannot be reconstructed")
      endif

    ! End loop on cells
    enddo

endsubroutine fluent2typhon_locate_hexanodes

!------------------------------------------------------------------------------!
!> @brief FLUENT : locate penta nodes
!------------------------------------------------------------------------------!
subroutine fluent2typhon_locate_pentanodes(def, isect, umesh)

  implicit none
  ! -- INPUTS --
  type(st_deffluent) , intent(inout) :: def
  integer            , intent(in)    :: isect
  ! -- OUTPUTS --
  type(st_ustmesh)   , intent(inout) :: umesh
  ! -- Internal variables --
  type(st_flucellsection) :: celllist
  integer(kip), pointer :: umesh_vtex(:,:)
  integer           :: icell, iface, itri
  integer           :: icvn, icvp, icvm
  integer           :: ifvn, ifvp, ifvq, ifvm
  integer           :: cvtex(6)
  integer           :: fvtex(4)
  logical           :: isok
  integer           :: errortag
  character(len=16) :: str
  ! -- BODY --

    celllist = def%cellsect(isect)

    umesh_vtex => umesh%cellvtex%elem(isect)%elemvtex

    ! Loop on cells
    do icell = 1, celllist%ncell

      isok = .true.
      ! Select first tri cell
      do iface = 1, 5
        if ( celllist%nvtx(icell,iface) == 3 ) exit
      enddo
      itri = iface
      cvtex(1:3) = celllist%vtex(icell,iface,:)
!      write(6,'(a,4i4)')  "cvtex: ",cvtex(1:4)
      cvtex(4:6) = 0
      umesh_vtex(icell,:) = cvtex(1:3)
      ! Loop on all 5 faces
      faceloop: &
      do iface = 1, 5
        if ( iface == itri ) cycle faceloop
        ! Test quad face
        if ( celllist%nvtx(icell,iface) == 4 ) then
        fvtex(1:4) = celllist%vtex(icell,iface,:)
!      write(6,'(a,4i4)')  "fvtex: ",fvtex(1:4)
        ! Loop on face vertices to be located
        ifvloop: &
        do ifvn = 1, 4
          ifvp = 1 + mod(ifvn,4)
          ifvq = 1 + mod(ifvp,4)
          ifvm = 1 + mod(ifvq,4)
         ! Loop on cell vertices to be found
         icvloop: &
         do icvn = 1, 4
          icvp = 1 + mod(icvn,4)
          icvm = 1 + mod(icvp,4)
!          write(6,'(i4,a,i4)')  fvtex(ifvn),"==",cvtex(icvn)
!          write(6,'(2x,i4,a,i4)') fvtex(ifvp),"==",cvtex(icvp)
!          write(6,'(2x,i4,a,i4)') fvtex(ifvp),"==",cvtex(icvm)
!          write(6,'(2x,i4,a,i4)') fvtex(ifvm),"==",cvtex(icvp)
!          write(6,'(2x,i4,a,i4)') fvtex(ifvm),"==",cvtex(icvm)
          if ( fvtex(ifvn) == cvtex(icvn) ) then
            ! Face 1265
            if     ( fvtex(ifvp) == cvtex(icvp) ) then
!            write(6,*) "icell =",icell, "icvp+4 =",icvp+4, "ifvq =", ifvq
!            write(6,*) "fvtex(",ifvq,") =",fvtex(ifvq) ; call flush(6)
              isok = isok .and. addcheckvtex(umesh_vtex(icell,icvp+3), fvtex(ifvq))     !           fq
              isok = isok .and. addcheckvtex(umesh_vtex(icell,icvn+3), fvtex(ifvm))     !        fm |
              errortag = 1                                                              !        |  cp=fp
              exit ifvloop                                                              !  cm----cn=fn
            ! Face 1485
            elseif ( fvtex(ifvp) == cvtex(icvm) ) then
              isok = isok .and. addcheckvtex(umesh_vtex(icell,icvm+3), fvtex(ifvq))     !           fm
              isok = isok .and. addcheckvtex(umesh_vtex(icell,icvn+3), fvtex(ifvm))     !        fq |
              errortag = 2                                                              !        |  cn=fn
              exit ifvloop                                                              !  cp----cm=fp
            ! Face 1562
            elseif ( fvtex(ifvm) == cvtex(icvp) ) then
              isok = isok .and. addcheckvtex(umesh_vtex(icell,icvp+3), fvtex(ifvq))     !           fq
              isok = isok .and. addcheckvtex(umesh_vtex(icell,icvn+3), fvtex(ifvp))     !        fp |
              errortag = 3                                                              !        |  cp=fm
              exit ifvloop                                                              !  cm----cn=fn
            ! Face 1584
            elseif ( fvtex(ifvm) == cvtex(icvm) ) then
              isok = isok .and. addcheckvtex(umesh_vtex(icell,icvm+3), fvtex(ifvq))     !           fp
              isok = isok .and. addcheckvtex(umesh_vtex(icell,icvn+3), fvtex(ifvp))     !        fq |
              errortag = 4                                                              !        |  cn=fn
              exit ifvloop                                                              !  cp----cm=fm
            ! Face wrongly defined
            else
              isok = .false.
              errortag = 5
              exit ifvloop
            endif
          endif
         ! End loop on cell vertices to be found
         enddo icvloop
         ! No "base" face cell vertices matched : this is the "top" face
         ! It is ignored (although it should be checked)
        ! End loop on face vertices to be located
        enddo ifvloop
        if ( .not. isok ) exit faceloop
        ! End test quad face
        endif
      ! End loop on all 5 faces
      enddo faceloop

      if ( .not. isok ) then
        call cfd_print("errortag: "//strofr(errortag,4))
        call cfd_print("in section: "//strofr(isect,4))
        call cfd_print("of type:    fluent:"//trim(fluent_cell_name(celllist%celltype)))
        call cfd_print("with cell:  "//strofr(icell,8))
        str = "vertices:"
        do icvn = 1, 6
          call cfd_print(str(1:12)//strofr(cvtex(icvn),10))
          str = " "
        enddo
        do iface = 1, 5
          if ( celllist%nvtx(icell,iface) == 4 ) then
          call cfd_print("face "//trim(strof(iface))//":" &
                         //strofr(celllist%vtex(icell,iface,1),10) &
                         //strofr(celllist%vtex(icell,iface,2),10) &
                         //strofr(celllist%vtex(icell,iface,3),10) &
                         //strofr(celllist%vtex(icell,iface,4),10) &
                         )
          else
          call cfd_print("face "//trim(strof(iface))//":" &
                         //strofr(celllist%vtex(icell,iface,1),10) &
                         //strofr(celllist%vtex(icell,iface,2),10) &
                         //strofr(celllist%vtex(icell,iface,3),10) &
                         )
          endif
        enddo
        call cfd_error("Cell cannot be reconstructed")
      endif

    ! End loop on cells
    enddo

endsubroutine fluent2typhon_locate_pentanodes

!------------------------------------------------------------------------------!
!> @brief FLUENT : create cells from faces
!------------------------------------------------------------------------------!
subroutine fluent2typhon_facecells(def, umesh)

  implicit none
  ! -- INPUTS --
  type(st_deffluent) , intent(inout) :: def
  ! -- OUTPUTS --
  type(st_ustmesh)   , intent(inout) :: umesh
  ! -- Internal variables --
  integer           :: celltype
  integer           :: isect
  ! -- BODY --

  do isect = 1, def%nsect
    celltype = def%cellsect(isect)%celltype
    select case(celltype)
  ! TRI_3
    case(fluent_cell_tri)
    call fluent2typhon_locate_trinodes(def, isect, umesh)
  ! TETRA_4
    case(fluent_cell_tetra)
    call fluent2typhon_locate_tetranodes(def, isect, umesh)
  ! QUAD_4
    case(fluent_cell_quad)
    call fluent2typhon_locate_quadnodes(def, isect, umesh)
  ! HEXA_8
    case(fluent_cell_hexa)
    call fluent2typhon_locate_hexanodes(def, isect, umesh)
  ! PENTA_6
    case(fluent_cell_wedge)
    call fluent2typhon_locate_pentanodes(def, isect, umesh)
  ! Default
    case default
      call cfd_error("cell type fluent:"//trim(fluent_cell_name(celltype))//" is not implemented")
    endselect

  enddo

endsubroutine fluent2typhon_facecells

!------------------------------------------------------------------------------!
!> @brief FLUENT : create cells from faces
!------------------------------------------------------------------------------!
subroutine fluent2typhon_createbocos(def, umesh)

  implicit none
  ! -- INPUTS --
  type(st_deffluent) , intent(in) :: def
  ! -- OUTPUTS --
  type(st_ustmesh)   , intent(inout) :: umesh
  ! -- Internal variables --
  integer       :: nbcfaces, nfaces, iboco
  integer       :: nbnodes, nbfils
  integer       :: nnnodes
  integer       :: i, j, ibc
  integer       :: ismixed
  type(st_connect), pointer :: facevtexhdle
  type(st_genconnect), pointer :: fvtxmixdhdle
  ! -- BODY --

  call createboco(umesh, def%nbctym)

  iboco = 0
  nbcfaces = 0
  ismixed = 0
  ! Loop on bczones
  do ibc = 1,def%nbczone
    ! if bczone was flagged as typhon bc
    if ( def%bczone(ibc)%isbctym == 1 ) then
      iboco = iboco + 1
      ! This should not happen
      if ( iboco > def%nbctym ) cycle
      nfaces = def%bczone(ibc)%nfaces
      call new_ustboco(umesh%boco(iboco), trim(def%bczone(ibc)%bczonename), nfaces)
      call cfd_print("creating boco("//strofr(nfaces,6)//" faces) : "//trim(def%bczone(ibc)%bczonename))
      !!call new_ustboco(umesh%boco(iboco), trim(def%bczone(ibc)%bczonename), 0)
      !!umesh%boco(iboco)%ilocation = iloc_elemface
      !!umesh%boco(iboco)%ntag = nfaces
      !!allocate(umesh%boco(iboco)%itag(nfaces))
      !!umesh%boco(iboco)%itag(1:nfaces) = (/ (nbcfaces+j, j=1,nfaces) /)
      umesh%boco(iboco)%iface(1:nfaces) = (/ (nbcfaces+j, j=1,nfaces) /)
      nbcfaces = nbcfaces+nfaces
      if ( associated(def%bczone(ibc)%fvtxmixd) .or. &
           def%bczone(ibc)%facevtex%nbfils /= def%max_nvtexface ) then
        ismixed = 1
      endif
    endif
  enddo

  ! EITHER (count internal faces)
  !umesh%nface     = def%nfaces
  !umesh%nface_int = def%nfaces - nbcfaces
  !umesh%nface_lim = nbcfaces
  ! OR (do NOT count internal faces)
  umesh%nface     = nbcfaces
  umesh%nface_lim = nbcfaces

  ! If this happened
  if ( iboco > def%nbctym ) then
    call cfd_write("(fluent) nbocotym = "//trim(strof(iboco)))
    call cfd_write("(fluent) nbocodef = "//trim(strof(def%nbctym)))
    call cfd_error("         unmatching numbers of bocos...")
  endif

  if ( ismixed == 0 ) then
    ! Only zones of faces with same number of vertices
    nbfils = def%max_nvtexface
    call new_connect(umesh%facevtex, nbcfaces, nbfils)
    ! Fill umesh%facevtex%fils(nbcfaces,nbfils)
    nnnodes = 0
    do ibc = 1,def%nbczone
      if ( def%bczone(ibc)%isbctym /= 1 ) cycle
      facevtexhdle => def%bczone(ibc)%facevtex
      nbnodes = facevtexhdle%nbnodes
      !write(6,*) 'bczone = ',ibc, '/', def%nbczone
      !write(6,*) 'nbndes = ',nbnodes
      !write(6,*) 'nbfils = ',def%max_nvtexface
      call flush(6)
      umesh%facevtex%fils(nnnodes+1:nnnodes+nbnodes,1:nbfils) = facevtexhdle%fils(1:nbnodes,1:nbfils)
      nnnodes = nnnodes + nbnodes
    enddo
  else
    ! Zones of faces with varying numbers of vertices
    call new_genconnect(umesh%vtexface, nbcfaces, def%max_nvtexface)
    ! Fill umesh%fvtxmixd%fils(nbnodes,nbfils)
    nnnodes = 0
    do ibc = 1,def%nfacevtex
      if ( def%bczone(ibc)%isbctym /= 1 ) cycle
      if ( associated(def%bczone(ibc)%fvtxmixd) ) then
        ! Zone of faces with mixed numbers of vertices
        fvtxmixdhdle => def%bczone(ibc)%fvtxmixd
        nbnodes = fvtxmixdhdle%nbnodes
        !write(6,*) 'bczone = ',ibc, '/', def%nbczone
        !write(6,*) 'nbndes = ',nbnodes
        !write(6,*) 'nbfils = (array)'
        call flush(6)
        do i = 1, nbnodes
          nnnodes = nnnodes + 1
          call new_elemc_copy(umesh%vtexface%node(nnnodes), fvtxmixdhdle%node(i))
        enddo
      else
        ! Zone of faces with same number of vertices
        facevtexhdle => def%bczone(ibc)%facevtex
        nbnodes = facevtexhdle%nbnodes
        nbfils = facevtexhdle%nbfils
        !write(6,*) 'bczone = ',ibc, '/', def%nbczone
        !write(6,*) 'nbndes = ',nbnodes
        !write(6,*) 'nbfils = ',nbfils
        call flush(6)
        do i = 1, nbnodes
          nnnodes = nnnodes + 1
          call new_elemc(umesh%vtexface%node(nnnodes), nbfils)
          umesh%vtexface%node(nnnodes)%elem(1:nbfils) = facevtexhdle%fils(i,1:nbfils)
        enddo
      endif
    enddo
  endif

endsubroutine fluent2typhon_createbocos

!------------------------------------------------------------------------------!
!> @brief FLUENT : read zone boundary and bcs
!------------------------------------------------------------------------------!
subroutine fluent_get_zonebcs(def)

  implicit none
  ! -- INPUTS --
  type(st_deffluent) , intent(inout) :: def
  ! -- OUTPUTS --
  ! -- Internal variables --
  character(len=256) :: str
  ! -- BODY --
  call fluent_get_zonebnd(def)
  call fluent_read_strlist(def, str)

endsubroutine fluent_get_zonebcs

!------------------------------------------------------------------------------!
!> @brief FLUENT : read zone boundary
!------------------------------------------------------------------------------!
subroutine fluent_get_zonebnd(def)

  implicit none
  ! -- INPUTS --
  type(st_deffluent) , intent(inout) :: def
  ! -- OUTPUTS --
  ! -- Internal variables --
  character(len=256) :: str
  character(len=64) :: strtab(4)
  character(len=64) :: bctypename, prtbctypename
  character(len=64) :: bczonename, prtbczonename
  character(len=64) :: bczonquote, prtbczonquote
  integer           :: zoneid, i
  integer           :: snb
  integer           :: ierr, ifound
  ! -- BODY --
  call fluent_read_strlist(def, str)

  call str_read_strs(str, snb, strtab, ierr)
  if ( ierr /= 0 ) then
    call cfd_error("(fluent) could not read parameters for boundary section")
  endif
  if ( snb > 3 ) then
    call cfd_warning("(fluent) expected 3 strings, found "//strofr(snb,2)//" in '"//trim(str)//"'")
  elseif ( snb < 3 ) then
    call cfd_error("(fluent) expected 3 strings, found "//strofr(snb,2)//" in '"//trim(str)//"'")
  endif
  zoneid = str_to_int(strtab(1), '(i)', ierr)
  bctypename = strtab(2)
  bczonename = strtab(3)
  bczonquote = '"'//trim(bczonename)//'"'
  str = "( "//bczonquote(1:max(12,len_trim(bczonquote))) &
     //" , "//bctypename(1:max(15,len_trim(bctypename)))//" )"
  call cfd_print("- reading FLUENT bzone "//strofr(zoneid,4) &
               //": "//trim(str))

  ifound = 0
  ! Loop on bczones
  do i = 1,def%nbczone
    ! If bczone already processed then proceed to next bczone
    if ( def%bczone(i)%off == 1 ) cycle
    ! If bczone corresponds to other zone then proceed to next bczone
    if ( def%bczone(i)%bczoneid /= zoneid ) then
      if ( fluent_bc_type(bctypename) .ne. fluent_bc_interior ) then
        call cfd_warning("(fluent) bczone "//trim(bczonename)// &
                                        "("//trim(bctypename)//") is skipped")
        call cfd_write("         bcs might be wrong numbered")
      endif
      cycle
    endif
    ! If bczone is interior then change printed zonename
    if ( bctypename .eq. fluent_bc_name(fluent_bc_interior) .and. &
         bczonename .eq. "int_"//def%bczone(i)%bczonename ) then
      prtbczonename = bczonename
    else
      prtbczonename = def%bczone(i)%bczonename
    endif
    if ( bczonename /= prtbczonename .or. &
         fluent_bc_type(bctypename) /= def%bczone(i)%bczonetype ) then
      prtbctypename = fluent_bc_name(def%bczone(i)%bczonetype)
      prtbczonename = trim(def%bczone(i)%bczonename)
      prtbczonquote = '"'//trim(prtbczonename)//'"'
      str = "( "//prtbczonquote(1:max(12,len_trim(prtbczonquote))) &
         //" , "//prtbctypename(1:max(15,len_trim(prtbctypename)))//" )"
      call cfd_print("                             "//trim(str)//" ignored")
    endif
    if ( bctypename .ne. fluent_bc_name(fluent_bc_interior) ) then
      def%nbctym = def%nbctym+1
      def%bczone(i)%isbctym = 1
    endif
    def%bczone(i)%off = 1
    ifound = 1
    exit
  ! End loop on bczones
  enddo

  ! If not found and not special fluid/interior cell zone then warning
  if ( ifound == 0 .and. &
       ( fluent_bc_type(bctypename) /= fluent_bc_interior .or. &
         fluent_bc_name(fluent_bc_type(bctypename)) == bctypename) ) then
    call cfd_print("                 Boundary zone is ignored")
  endif

endsubroutine fluent_get_zonebnd

!------------------------------------------------------------------------------!
!> @brief FLUENT : ignored section
!------------------------------------------------------------------------------!
subroutine fluent_get_ignored(def)

  implicit none
  ! -- INPUTS --
  type(st_deffluent) , intent(inout) :: def
  ! -- OUTPUTS --
  ! -- Internal variables --
  character(len=256) :: str
  ! -- BODY --
  call fluent_read_strlist(def, str)
  write(6,'(3a)') "(ignored   : '",trim(str),"')"

endsubroutine fluent_get_ignored

!------------------------------------------------------------------------------!
!> @brief FLUENT : read section end (trailing ')')
!------------------------------------------------------------------------------!
subroutine fluent_get_sctend(def)

  implicit none
  ! -- INPUTS --
  type(st_deffluent) , intent(inout) :: def
  ! -- OUTPUTS --
  ! -- Internal variables --
  integer :: ierr
  ! -- BODY --
  call affstar("get_s_end", def%c)
  call fll_checknextchar(def%iu_bin, def%c, ')')
  call affexit("get_s_end", def%c)

endsubroutine fluent_get_sctend

!------------------------------------------------------------------------------!
! Printing subroutines
!------------------------------------------------------------------------------!
subroutine display_mesh(mesh)

  implicit none
  ! -- INPUTS --
  type(st_mesh), intent(in) :: mesh
  ! -- OUTPUTS --
  ! -- Internal variables --
  ! -- BODY --

  !type(info_mesh) :: info
  write(6,'(a,3(1X,i4))') "  idim, jdim, kdim =", mesh%idim, mesh%jdim, mesh%kdim
  write(6,'(a,2(1X,i4))') "  nvtex, maxvtex   =", mesh%nvtex, mesh%maxvtex
  write(6,'(a,  1X,i4 )') "  nface =", mesh%nface
  write(6,'(a,  1X,i4 )') "  ncell =", mesh%ncell
  !integer(kip), pointer :: mesh%ivtex(:)              ! global index of vertex (1..maxvtex)
  !type(v3d),     pointer     :: mesh%vertex(:,:,:),mesh% centre(:,:,:) ! coordonnees des sommets et centres de cellules (i,j,k)
  !type(v3d),     allocatable :: mesh%vertex_orig(:,:,:)           ! MRF addition: table of original vertex positions
  !type(st_face), pointer     :: mesh%iface(:,:,:) !, jface, kface ! tableaux de faces
  !real(krp),     pointer     :: mesh%volume(:,:,:)                ! volume des cellules
  !real(krp),     pointer     :: mesh%metricsvm(:,:,:)             ! transformation of a triangular cell (SV) to a standard triangle

endsubroutine display_mesh

subroutine display_connect(connect)

  implicit none
  ! -- INPUTS --
  type(st_connect), intent(in) :: connect
  ! -- OUTPUTS --
  ! -- Internal variables --
  integer :: nn,nf,i,j
  ! -- BODY --

  nn = connect%nbnodes
  nf = connect%nbfils
  write(6,'(a,2i4,$)') "  nbnodes, nbfils =", nn, nf
  if ( associated(connect%fils) ) then
  write(6,'()')
  do i = 1, min(nn,64)
    write(6,'(a,i4,a,i4,a,$)') "     fils(",i,",1:",nf,") = ["
    write(6,'(i4,$)') (connect%fils(i,j),j=1,nf)
    write(6,'(a)') "]"
  enddo
  if ( nn > 64 ) then
    write(6,'(a)') "..."
  endif
  else
  write(6,'(a)') " - fils: unassociated"
  endif

endsubroutine display_connect

subroutine display_genconnect(genconnect)

  implicit none
  ! -- INPUTS --
  type(st_genconnect), intent(in) :: genconnect
  ! -- OUTPUTS --
  ! -- Internal variables --
  ! -- BODY --

  write(6,'(a,i4,$)') "  nbnodes =", genconnect%nbnodes
  if ( associated(genconnect%node) ) then
  write(6,'(a,i4,a)') " - node: (",size(genconnect%node),")"
  else
  write(6,'(a)') " - node: unassociated"
  endif

endsubroutine display_genconnect

subroutine display_elemvtex(elem)

  implicit none
  ! -- INPUTS --
  type(st_elemvtex), intent(in) :: elem
  ! -- OUTPUTS --
  ! -- Internal variables --
  integer :: ne,nv,i,j
  ! -- BODY --

  ne = elem%nelem
  nv = elem%nvtex
  write(6,'(a,1i4)') "    elemtype     =", elem%elemtype
  write(6,'(a,2i4)') "    nelem, nvtex =", ne, nv
  do i = 1, min(ne,64)
    write(6,'(a,3(i4,a),$)') "      elem(",i,"::",elem%ielem(i),",1:",nv,") = ["
    write(6,'(i4,$)') (elem%elemvtex(i,j),j=1,nv)
    write(6,'(a)') "]"
  enddo
  if ( ne > 64 ) then
    write(6,'(a)') "..."
  endif
  !integer(kip), pointer :: ielem(:)       ! element global index
  !integer(kip), pointer :: elemvtex(:,:)  ! element to vtex connectivity (1:nelem, 1:nvtex)

endsubroutine display_elemvtex

subroutine display_genelemvtex(gelem)

  implicit none
  ! -- INPUTS --
  type(st_genelemvtex), intent(in) :: gelem
  ! -- OUTPUTS --
  ! -- Internal variables --
  integer :: i
  ! -- BODY --

  write(6,'(a,i4)') "  nsection =", gelem%nsection
  do i = 1, gelem%nsection
  write(6,'(3a)') "  section",strofr(i,4),": [elemvtex]"
  call display(gelem%elem(i))
  enddo

endsubroutine display_genelemvtex

subroutine display_ustboco(boco)

  implicit none
  ! -- INPUTS --
  type(st_ustboco), intent(in) :: boco
  ! -- OUTPUTS --
  ! -- Internal variables --
  ! -- BODY --

  write(6,'(a, 1a)') "  family = ", trim(boco%family)
  !write(6,'(a,1i4)') "  iexttag   =", boco%iexttag
  !write(6,'(a,1i4)') "  ilocation =", boco%ilocation
  !write(6,'(a,1i4)') "  idefboco  =", boco%idefboco
  write(6,'(a,1i4,$)') "  ntag   =", boco%ntag
  if ( associated(boco%itag) ) then
  write(6,'(a,i4,a)') " - itag: (",size(boco%itag),")"
  else
  write(6,'(a)') " - itag: unassociated"
  endif
  write(6,'(a,1i4,$)') "  nface  =", boco%nface
  if ( associated(boco%iface) ) then
  write(6,'(a,i4,a)') " - iface: (",size(boco%iface),")"
  else
  write(6,'(a)') " - iface: unassociated"
  endif
  !write(6,'(a,e)') "  area  =", boco%area
  !type(st_genericfield), pointer :: bocofield  ! pointer to chained list of boco fields (in MGRID)
  !type(st_genericfield)          :: avg_quant  ! average quantity on boco 
  !type(st_genericfield)          :: sum_flux   ! sum of fluxes    on boco
  !type(st_gridconnect)           :: gridcon    ! connectivity to grid

endsubroutine display_ustboco

subroutine display_ustmesh(umesh)

  implicit none
  ! -- INPUTS --
  type(st_ustmesh), intent(in) :: umesh
  ! -- OUTPUTS --
  ! -- Internal variables --
  integer :: i
  ! -- BODY --

  write(6,'(a)') "--- ustmesh >>>"
  write(6,'(a,1i4)') "id      =", umesh%id
  write(6,'(a,1i4)') "level   =", umesh%level
  write(6,'(a,1i4)') "elemdim =", umesh%elemdim
  write(6,'(a,3i4)') "nvtex, nface, ncell  =", umesh%nvtex, umesh%nface, umesh%ncell
  write(6,'(a,2i4)') "nface_int, ncell_int =", umesh%nface_int, umesh%ncell_int
  write(6,'(a,2i4)') "nface_lim, ncell_lim =", umesh%nface_lim, umesh%ncell_lim
  write(6,'(a)') "mesh: [mesh]"
  call display(umesh%mesh)                  !> mesh (geometrical data)
  write(6,'(a)') "facevtex: [connect]"
  call display(umesh%facevtex)
  write(6,'(a)') "facecell: [connect]"
  call display(umesh%facecell)
  write(6,'(a)') "umesh%colors: [genconnect]"
  call display(umesh%colors)
  write(6,'(a)') "umesh%vtexface: [genconnect]"
  call display(umesh%vtexface)
  write(6,'(a)') "umesh%cellvtex: [genelemvtex]"
  call display(umesh%cellvtex)
  write(6,'(a,1i4)') "umesh%nboco =", umesh%nboco
  do i = 1, umesh%nboco
  write(6,'(a,2a)') "umesh%boco(",strofr(i,4),"): [ustboco]"
  call display(umesh%boco(i))
  enddo
  write(6,'(a)') "umesh%face_Ltag: [connect]"
  call display(umesh%face_Ltag)
  write(6,'(a)') "umesh%face_Rtag: [connect]"
  call display(umesh%face_Rtag)
  ! --- specific SVM structure ---
  !write(6,'(a,1i4)') "nface_intsvm =", umesh%nface_intsvm
  write(6,'(a)') "--- ustmesh <<<"

endsubroutine display_ustmesh

endmodule FLUENT
