!------------------------------------------------------------------------------!
! Procedure : readrpmblock                Author : J. Gressier
!                                         Date   : Feb 2002
!                                         Modif  :
! Function :
!   Reading and buffering of line blocks in RPMBLOCK structures
!   managed as lists.
!
! Defaults/Limitations/Misc :
!
!------------------------------------------------------------------------------!
subroutine readrpmblock(nio, nerr, iaff, firstblock)
  use STRING
  implicit none

! -- Inputs --
  integer nio    ! unit number to read
  integer nerr   ! error type at parameter reading
  integer iaff   ! information display option

! -- Outputs --
  type(rpmblock), pointer  :: firstblock    ! first RPM block pointer

! -- Internal variables --
  integer, parameter       :: dimbuf = 100  ! size of line buffer
  integer                  :: lectstat      ! reading status
  integer                  :: ilig          ! line number
  integer                  :: posc          ! character position
  logical                  :: inblock       ! block being processed
  character(len=dimrpmlig) :: strc          ! current string
  character(len=dimrpmlig), dimension(:), allocatable &
                           :: buffer
  type(rpmblock), pointer  :: newblock, blockcourant
  type(rpmdata),  pointer  :: newdata,  datacourant

! -- Body --
  lectstat = 0
  ilig     = 1
  inblock  = .false.
  nullify(firstblock)
  allocate(buffer(dimbuf))

  do while ((lectstat == 0).and.(ilig <= dimbuf))

    read(unit=nio, fmt='(a)', iostat=lectstat) strc
    if (lectstat == 0) then
      if (iaff >= 4) write(nerr,*) "RPM: reading - ",ilig," : ",trim(strc)
      buffer(ilig) = trait_rpmlig(strc)
    else
      buffer(ilig) = ""
      if (iaff >= 2) write(nerr,*) "RPM: end of file"
    endif

    if (len_trim(buffer(ilig)) /= 0) then

      ! ----- test beginning of block -----
      posc = index(buffer(ilig),':')
      if (samestring(buffer(ilig)(1:posc-1), 'BLOCK')) then
        if (ilig > 1) call rpmerr("Unexpected data before block definition")
        if (inblock) call rpmerr("Incomplete previous block")

        ! the block is assumed to be valid
        inblock = .true.
        buffer(ilig) = adjustl(buffer(ilig)(posc+1:))
        call create_rpmblock(newblock, trim(buffer(ilig)))  ! alloc. and init.
        if (associated(firstblock)) then
          blockcourant%next => newblock  ! definition of chaind list link
        else
          firstblock => newblock         ! definition of first block
        endif
        blockcourant => newblock         ! redefinition of current block
        if (iaff >= 2) write(nerr,*) "RPM: reading block : ",trim(newblock%name)
        cycle                            ! avoid ilig increment
      endif

      ! ----- test end of bloc -----
      if (samestring(trim(buffer(ilig)), 'ENDBLOCK')) then
        if (.not.inblock) call rpmerr("Unexpected end of block")
        call set_rpmblock(blockcourant, ilig-1, buffer(1:ilig-1))
        ! The ENDBLOCK line is not counted for
        ! re-initialization of current data
        ilig    = 1
        inblock = .false.
        cycle   ! trailing processes are skipped
      endif

      ! ----- test end of data block (incorrect) -----
      if (samestring(trim(buffer(ilig)), 'ENDDATA')) then
        call rpmerr("Unexpected end of data block")
      endif

      ! ----- test beginning of data block -----
      posc = index(buffer(ilig),'=')
      if (samestring(buffer(ilig)(1:posc-1), 'DATA')) then
        ! reading of DATA block
        call readrpmdata(nio, newdata, buffer(ilig), iaff)
        ! definition of chained list links
        if (associated(blockcourant%data)) then
          datacourant%next => newdata
        else
          blockcourant%data => newdata
        endif
        datacourant => newdata
        if (iaff >= 2) write(nerr,*) "RPM: reading data block : ",&
                                     datacourant%nbvar," variables"
        cycle   ! trailing processes are skipped
      endif ! directly to ENDDATA line

      ! ----- inside block internal instruction -----
      ilig = ilig + 1
    endif
  enddo

  if (inblock) call rpmerr("Still looking for end of block")
  if (ilig > dimbuf) call rpmerr("maximal block buffer size exceeded")
  deallocate(buffer)

endsubroutine readrpmblock
!------------------------------------------------------------------------------!




!------------------------------------------------------------------------------!
! Procedure : readrpmdata                 Author : J. Gressier
!                                         Date   : Feb 2002
!                                         Modif  :
! Function :
!   Reading data blocks starting/ending with DATA/ENDDATA.
!   Returns RPMDATA structure pointer
!
! Defaults/Limitations/Misc :
!
!------------------------------------------------------------------------------!
subroutine readrpmdata(nio, pdata, header, iaff)
  use STRING
  implicit none

! -- Inputs --
  integer           :: nio    ! unit number to read
  integer           :: iaff   ! action display option
  character(len=*)  :: header ! DATA block header, to be processed

! -- Outputs --
  type(rpmdata), pointer :: pdata ! pointer to DATA structure

! -- Internal variables --
  integer, parameter                :: dimbuffer = 200
  integer                           :: lectstat    ! reading status
  integer                           :: n, i, pos
  real, dimension(:,:), allocatable :: buffer      ! buffer before assignment
  character(len=dimrpmlig)          :: strc        ! temporary line
  logical                           :: fin         ! test

! -- Body --

  call create_rpmdata(pdata)

  ! Compute the number of variables
  strc = adjustl(header(index(header,'=')+1:))  ! Extraction of DATA=
  pdata%nbvar = numbchar(strc,',') + 1
  if (iaff >= 4) write(6,*) "DATA : ",pdata%nbvar," variables found"

  ! Extraction of variables names
  allocate(pdata%name(pdata%nbvar))
  do n = 1, pdata%nbvar
    if (n == pdata%nbvar) then  ! last variable, process without comma
      pdata%name(n) = trim(strc)
    else
      pos = index(strc,',')
      pdata%name(n) = strc(1:pos-1)  ! name extraction
      strc = adjustl(strc(pos+1:))   ! shift
    endif
    if (len_trim(pdata%name(n)) == 0) call rpmerr("Nom de variable incorrect")
    ! check for variables names redundancy
    do i = 1, n-1
      if (samestring(pdata%name(n),pdata%name(i))) &
        call rpmerr("Redundant variable name")
    enddo
  enddo
  if (iaff >= 4) write(6,*) "DATA variables : ",pdata%name

  ! Data reading
  allocate(buffer(pdata%nbvar, dimbuffer))
  n   = 0
  fin = .false.
  do while ((n < dimbuffer).and.(.not.fin))
   read(unit=nio, fmt='(a)', iostat=lectstat) strc
   strc = trait_rpmlig(strc)
   if (lectstat /= 0) call rpmerr("Unexpected file read error")
   if (samestring(strc,'ENDDATA')) then
     fin = .true.
   else
     n = n + 1
     read(strc,*,iostat=lectstat) buffer(:,n)
     if (lectstat /= 0) call rpmerr("Incoherent data")
   endif
  enddo
  pdata%nbpts = n
  allocate(pdata%tab(pdata%nbvar,pdata%nbpts))
  pdata%tab = buffer(:,1:pdata%nbpts)
  deallocate(buffer)

endsubroutine readrpmdata
!------------------------------------------------------------------------------!




!------------------------------------------------------------------------------!
! Procedure : trait_rpmlig                Author : J. Gressier
!                                         Date   : Feb 2002
!                                         Modif  :
! Function :
!   Processing a line of RPM file
!   . Remove leading space in string
!   . Remove string starting with ! or # (rpmcommentchar)
!   . Change to uppercase except for string between "quotes"
!
! Defaults/Limitations/Misc :
!
!------------------------------------------------------------------------------!
function trait_rpmlig(strin) result(strout)

  use STRING
  implicit none

! -- Inputs --
  character(len=dimrpmlig), intent(in)   :: strin
! -- Outputs --
  character(len=len(strin))              :: strout

! -- Internal variables --
  character(len=len(strin)) :: s
  integer                   :: ipos

! -- Body --

  ! Search for comment starting characters
  ipos = scan(strin, rpmcommentchar)
  if (ipos >= 1) then
    ! remove comments and change to uppercase
    s = rpmuppercase(strin(1:ipos-1))
    ! remove leading space
    s = adjustl(s)
  else
    ! change to uppercase and remove leading space
    s = adjustl(rpmuppercase(strin))
  endif
  strout = s

contains
!------------------------------------------------------------------------------!
! Change to uppercase except for string between quotes ("")
!------------------------------------------------------------------------------!
function rpmuppercase(strc)
implicit none
character(len=*)        :: strc
character(len=len(strc)) :: rpmuppercase
integer   :: i
logical   :: inquote

  rpmuppercase = strc
  inquote      = .false.
  do i = 1, len(rpmuppercase)
    if (rpmuppercase(i:i) == rpmquotechar) inquote = .not.inquote
    if (.not.inquote) rpmuppercase(i:i) = uppercase(rpmuppercase(i:i))
  enddo
  ! No test for opening '"' without closing '"'

endfunction rpmuppercase

endfunction trait_rpmlig
!------------------------------------------------------------------------------!
