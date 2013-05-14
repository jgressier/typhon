!------------------------------------------------------------------------------!
! procedure : seekrpmblock                Author : J. Gressier
!                                         Date   : Avril 2002
!                                         Modif  :
! Function :
!   Searches for the pointer to a block named bkname, returns
!   - a pointer to the block found, NULL otherwise
!   - the total count of found occurrences (ntot), if num == 0
!
! Defaults/Limitations/Misc :
!
!------------------------------------------------------------------------------!
subroutine seekrpmblock(block, bkname, num, blkres, ntot)
implicit none

! -- Inputs --
type(rpmblock), pointer :: block   ! pointer to searched block
character(len=*)        :: bkname  ! searched keyword
integer                 :: num     ! number of searched occurrence

! -- Outputs --
type(rpmblock), pointer :: blkres  ! pointer to block found
integer                 :: ntot    ! total count of occurrences, computed if num == 0

! -- Internal variables --
type(rpmblock), pointer :: pcour   ! pointer to current block
integer                 :: inum    ! current valid block number

! -- Body --

  inum  = 0
  pcour => block
  !print*,'start seek'
  do while (associated(pcour))
    !print*,'seek ', inum," :",pcour%name,":",bkname
    if (samestring(bkname, pcour%name)) then
      blkres => pcour
      inum   =  inum + 1
      if ((num /= 0).and.(inum == num)) exit ! if occurrence found, exit loop
    endif
    pcour => pcour%next

  enddo

  if (num == 0) ntot = inum

endsubroutine seekrpmblock
!------------------------------------------------------------------------------!




!------------------------------------------------------------------------------!
! procedure : seekinrpmblock              Author : J. Gressier
!                                         Date   : Feb 2002
!                                         Modif  :
! Function :
!   Searches for the keyword key in a given block, returns
!   - the line number nlig of occurrence num (the first if num == 0),
!       0 if no occurrence is found
!   - the total count of occurrences found ntot (if num == 0)
!
! Defaults/Limitations/Misc :
!
!------------------------------------------------------------------------------!
subroutine seekinrpmblock(block, key, num, nlig, ntot)
implicit none

! -- Inputs --
type(rpmblock), pointer :: block   ! pointer to searched block
character(len=*)        :: key     ! searched keyword
integer                 :: num     ! number of searched occurrence
! -- Outputs --
integer ntot   ! total count of occurrences, computed if num == 0
integer nlig   ! line number of occurrence num

! -- Internal variables --
integer                  ilig, nkey
character(len=dimrpmlig) strvalue

! -- Body --

  nlig = 0
  ilig = 1
  nkey = 0

  do while (ilig <= block%nblig)
    strvalue = block%txt(ilig)(1:index(block%txt(ilig),'=')-1)

    if (samestring(key, strvalue)) then
      nkey = nkey + 1
      if (num == 0) then           ! every occurrence is added
        if (nkey == 1) nlig = ilig
      else
        if (nkey == num) then      ! only num is searched for
          nlig = ilig
          exit
        endif
      endif

    endif

    ilig = ilig + 1
  enddo
  if (num  == 0) ntot = nkey

endsubroutine seekinrpmblock
!------------------------------------------------------------------------------!




!------------------------------------------------------------------------------!
! function : rpm_existkey                 Author : J. Gressier
!                                         Date   : Nov 2002
!                                         Modif  :
! Function :
!   Returns boolean on existence of key in block
!
! Defaults/Limitations/Misc :
!
!------------------------------------------------------------------------------!
logical function rpm_existkey(block, key)
implicit none

! -- Inputs --
type(rpmblock), pointer :: block   ! pointer to searched block
character(len=*)        :: key     ! searched keyword
! -- Outputs --

! -- Internal variables --
integer                  i, nkey

! -- Body --

  call seekinrpmblock(block, key, 0, i, nkey)
  rpm_existkey = (nkey >= 1)

endfunction rpm_existkey
!------------------------------------------------------------------------------!




!------------------------------------------------------------------------------!
! Function : numvar_inrpmdata             Author : J. Gressier
!                                         Date   : Feb 2002
!                                         Modif  :
! Function :
!   Searches the variable number in RPMDATA structure
!
! Defaults/Limitations/Misc :
! - it is assumed that one exists
! - returns 0 if no one exists
!
!------------------------------------------------------------------------------!
function numvar_inrpmdata(strvar, data)
implicit none

! -- Inputs --
character(len=*)        :: strvar  ! name of searched variable
type(rpmdata),  pointer :: data    ! data structure to be processed
! -- Outputs --
integer                 :: numvar_inrpmdata

! -- Internal variables --
integer i

! -- Body --
  numvar_inrpmdata = 0
  do i = 1, data%nbvar
    if (samestring(strvar,data%name(i))) numvar_inrpmdata = i
  enddo

endfunction numvar_inrpmdata
!------------------------------------------------------------------------------!




!------------------------------------------------------------------------------!
! Procedure : seekrpmdata                 Author : J. Gressier
!                                         Date   : Feb 2002
!                                         Modif  :
! Function :
!   Searches for the pointer numbered num to a data containing variable strvar
!   The total count of occurrences is returned if num == 0
!
! Defaults/Limitations/Misc :
! - the variable name must be uppercase
!
!------------------------------------------------------------------------------!
subroutine seekrpmdata(block, strvar, num, data, ntot)
implicit none

! -- Inputs --
type(rpmblock), pointer :: block   ! block to be searched only
character(len=*)        :: strvar  ! name of searched variable
integer                 :: num     ! number of searched occurrence

! -- Outputs --
type(rpmdata),  pointer :: data    ! pointer containing the variable
integer                 :: ntot    ! total count of found occurrences

! -- Internal variables --
type(rpmdata),  pointer :: pdata   ! temporary pointer
integer                 :: n       ! current number of occurrence

! -- Body --

  n = 0
  pdata => block%data
  do while (associated(pdata))
    if (numvar_inrpmdata(strvar, pdata) /= 0) then
      n    =  n + 1
      data => pdata
      if ((num /= 0).and.(n == num)) exit ! if occurrence found, exit loop
    endif
    pdata => pdata%next
  enddo
  ntot = n

endsubroutine seekrpmdata
!------------------------------------------------------------------------------!




!------------------------------------------------------------------------------!
! Procedure :                             Author : J. Gressier
!                                         Date   : Feb 2002
!                                         Modif  :
! Function :
!
! Input parameters :
!
! Output parameters :
!
! Defaults/Limitations/Misc :
!
!------------------------------------------------------------------------------!
!subroutine

!  use
!  implicit none

! -- Inputs --

! -- Outputs --

! -- Internal variables --

! -- Body --

!endsubroutine
!------------------------------------------------------------------------------!

