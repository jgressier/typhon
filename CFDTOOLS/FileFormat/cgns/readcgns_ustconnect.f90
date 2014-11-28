!------------------------------------------------------------------------------!
! Procedure : readcgns_ustconnect
!
! Function
!   Read CGNS connectivity in TYPHON structure
!
!------------------------------------------------------------------------------!
subroutine readcgns_ustconnect(iunit, ib, iz, umesh)

use IOCFD
use STRING
use USTMESH
use CGNS_STRUCT

implicit none

! -- INPUTS --
integer             :: iunit      ! i/o unit for reading
integer             :: ib, iz     ! base and zone numbers

! -- OUTPUTS --
type(st_ustmesh)    :: umesh      ! cell->vertex, face->vertex connectivity

! -- Internal variables --
integer             :: ier        ! error code
integer             :: ifam, nfam ! family index and total number of families
integer             :: ideb, ifin ! cell indices in the section
integer             :: nbd, ip    ! CGNS unused integers
integer, dimension(:,:), allocatable &
                    :: elem       ! intermediate CGNS connectivity
character(len=cgnslen) :: cgnsname ! cgns   string
character(len=100)     :: str_w    ! output string
integer, parameter   :: nmax_cell = 20        ! max nb of cells in vtex->cell connectivity
integer              :: nface                 ! estimated number of faces
integer              :: icell                 ! indices
integer              :: ielem, nelem , nvtex
integer              :: ElementDataSize       ! size of data element
integer, allocatable :: imixedtype(:)         ! type  of element in MIXED section
integer, allocatable :: imixedindex(:)        ! index of element in MIXED section
integer              :: icgnstype, ityphontype, icgtype
integer              :: nelem_mixed, ie, ind

! -- BODY --

!-----------------------------------------------------------------
! Number of CGNS element sections
! (cells are gathered in sections by type)

call cg_nsections_f(iunit, ib, iz, nfam, ier)
if (ier /= 0) call cfd_error("(CGNS) cannot read number of element sections")

call cfd_print("* reading CGNS element connectivity: "//trim(strof(nfam))//" sections")

!-----------------------------------------------------------------
! LOOP on CGNS sections

nface = 0

do ifam = 1, nfam               ! LOOP on CGNS sections

  call cg_section_read_f(iunit, ib, iz, ifam, cgnsname, icgnstype, ideb, ifin, nbd, ip, ier)
  if (ier /= 0) call cfd_error("(CGNS) cannot read section information")

  !-----------------------------------------------------------------
  ! read CGNS section

  call cg_npe_f(icgnstype, nvtex, ier)
  if (ier /= 0)    call cfd_error("(CGNS) cannot get number of vertex per element")

  nelem = ifin - ideb + 1

  write(str_w,'(a,i8,a,i2,a)') ". section "//cgnsname(1:10)//":",nelem, &
    " "//trim(ElementTypeName(icgnstype))//" elements (",nvtex," vertices)"
  call cfd_print(adjustl(str_w))

  if (nvtex == 0) then ! for special elements
    call cg_ElementDataSize_f(iunit, ib, iz, ifam, ElementDataSize, ier)
    allocate(elem(1,ElementDataSize))
  else
    allocate(elem(nvtex,nelem))  ! temporary array (help to swap dimensions)
  endif
  elem = 0
  call cg_elements_read_f(iunit, ib, iz, ifam, elem, ip, ier)       ! lecture
  if (ier /= 0) call cfd_error("(CGNS) cannot read element->vertex connectivity")

  select case(icgnstype)
  case(NODE)
    call cfd_print("  . skipping section")

  case(BAR_3, TRI_6, QUAD_8, QUAD_9, TETRA_10, PYRA_14, PENTA_15, PENTA_18, HEXA_20, HEXA_27)
    call cfd_error("Unexpected type of CGNS element ("//trim(ElementTypeName(icgnstype))//")")

  case(BAR_2, TRI_3, QUAD_4, TETRA_4, PYRA_5, PENTA_6, HEXA_8)
    ityphontype = cgns2typhon_elemtype(icgnstype)
    call cfd_print("  . create new element section: "//strofr(nelem,7)//" "//trim(ElementTypeName(icgnstype)))
    call addelem_genelemvtex(umesh%cellvtex)
    ielem = umesh%cellvtex%nsection
    call new_elemvtex(umesh%cellvtex%elem(ielem), nelem, ityphontype)

    do icell = 1, nelem  ! loop because of stack size problems
      umesh%cellvtex%elem(ielem)%elemvtex(icell, 1:nvtex) = elem(1:nvtex, icell)
      umesh%cellvtex%elem(ielem)%ielem   (icell)          = ideb-1+icell
    enddo

  case(MIXED)
    nelem_mixed = nelem
    allocate(imixedindex(nelem_mixed))
    allocate(imixedtype(nelem_mixed))
    call sort_elementtype(ElementDataSize, elem, nelem_mixed, imixedindex, imixedtype)
    do icgtype = min(BAR_2, TRI_3, QUAD_4, TETRA_4, PYRA_5, PENTA_6, HEXA_8),&
                 max(BAR_2, TRI_3, QUAD_4, TETRA_4, PYRA_5, PENTA_6, HEXA_8)
      nelem = count(imixedtype(1:nelem_mixed) == icgtype)
      if (nelem > 0) then
        nvtex       = nvtex_cgnselement(icgtype)
        ityphontype = cgns2typhon_elemtype(icgtype)
        call cfd_print("  . create new element section: "//strofr(nelem,7)//" "//trim(ElementTypeName(icgtype)))
        call addelem_genelemvtex(umesh%cellvtex)
        ielem = umesh%cellvtex%nsection
        call new_elemvtex(umesh%cellvtex%elem(ielem), nelem, ityphontype)
        icell = 0
        do ie = 1, nelem_mixed
          if (imixedtype(ie) == icgtype) then
            icell = icell + 1
            ind   = imixedindex(ie)
            umesh%cellvtex%elem(ielem)%elemvtex(icell, 1:nvtex) = elem(1, ind+1:ind+nvtex)
            umesh%cellvtex%elem(ielem)%ielem   (icell)          = ideb-1+ie
          endif
        enddo
        if (icell /= nelem) call cfd_error("unexpected number of element in MIXED section")
      endif
    enddo
    deallocate(imixedindex, imixedtype)

  case(NGON_N)
    call cfd_error("Unexpected type of CGNS element (NGON_N)")

  case default
    call cfd_error("Unknown CGNS element")
  endselect

  deallocate(elem)

enddo ! End of LOOP on CGNS sections

!------------------------------
contains

subroutine sort_elementtype(dim, element, nelem, iindex, itype)
implicit none
! -- dummy arguments --
integer, intent(in)  :: dim                          ! number of element
integer, intent(in)  :: element(1,dim)               ! MIXED element connectivity
integer, intent(in)  :: nelem                        ! number of element
integer, intent(out) :: iindex(nelem), itype(nelem)  ! index and type of MIXED element connectivity
! -- internal variables --
integer :: ie, nvtex, ielem

! -- BODY --

ie     = 1                ! index in element()
ielem  = 0                ! index in iindex() and itype()
iindex = 0
itype  = 0
do while (ie <= dim)
  ielem         = ielem + 1
  iindex(ielem) = ie                 ! index of element
  itype(ielem)  = element(1,ie)      ! type  of element
  nvtex         = nvtex_cgnselement(itype(ielem))
  !print*,ie,ielem,itype(ielem),nvtex
  if (nvtex <= 0)  &
    call cfd_error("Unexpected CGNS element in MIXED section: "//trim(strof(itype(ielem))))
  ie  = ie + 1 + nvtex
enddo
if ((ielem /= nelem).or.(ie /= dim+1)) &
  call cfd_error("Unexpected end of MIXED section")

endsubroutine sort_elementtype

!------------------------------
endsubroutine readcgns_ustconnect
!------------------------------------------------------------------------------!
! Changes history
!
! Dec  2010: direct fill in ustmesh structures
!------------------------------------------------------------------------------!
