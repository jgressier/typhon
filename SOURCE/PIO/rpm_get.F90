!------------------------------------------------------------------------------!
! rpmgetvalreal : returns the RHS of line/string "str" as a real(sp)
!------------------------------------------------------------------------------!
subroutine rpmgetvalrealsp(str, res)
implicit none
! -- Inputs --
character(len=*), intent(in) :: str  ! line holding the value    (input)
! -- Outputs --
real(4)        , intent(out) :: res  ! result, value on the line (output)
! -- Internal variables --
integer           :: i, iores

! -- Body --

  i = index(str, '=')
  read(str(i+1:), *, iostat=iores) res
  if (iores /= 0) call rpmerr("Floating point read failed")

endsubroutine rpmgetvalrealsp
!------------------------------------------------------------------------------!


!------------------------------------------------------------------------------!
! rpmgetvalreal : returns the RHS of line/string "str" as a real
!------------------------------------------------------------------------------!
subroutine rpmgetvalrealdp(str, res)
implicit none
! -- Inputs --
character(len=*), intent(in) :: str  ! line holding the value    (input)
! -- Outputs --
real(8)        , intent(out) :: res  ! result, value on the line (output)
! -- Internal variables --
integer           :: i, iores

! -- Body --

  i = index(str, '=')
  read(str(i+1:), *, iostat=iores) res
  if (iores /= 0) call rpmerr("Floating point read failed")

endsubroutine rpmgetvalrealdp
!------------------------------------------------------------------------------!


!------------------------------------------------------------------------------!
! rpmgetvalstr : returns the RHS of line/string "str" as a string
!------------------------------------------------------------------------------!
subroutine rpmgetvalstr(str, res)
implicit none
! -- Inputs --
character(len=*), intent(in) :: str  ! ligne contenant la valeur
! -- Outputs --
!character(len=len_trim(str)) :: res  ! resultat, valeur de la ligne
character(len=dimrpmlig) &
               , intent(out) :: res  ! resultat, valeur de la ligne
! -- Internal variables --
integer i, iores

! -- Body --

  i   = index(str, '=')                   ! extraction of the string after "="
  res = trim(adjustl(str(i+1:len(str))))

  i   = len_trim(res)                     ! test string between quotes
  if ((res(1:1) == rpmquotechar).and.(res(i:i) == rpmquotechar)) then
    res = res(2:i-1)
  endif

endsubroutine rpmgetvalstr
!------------------------------------------------------------------------------!


!------------------------------------------------------------------------------!
! rpmgetvalint : returns the RHS of line/string "str" as an integer
!------------------------------------------------------------------------------!
subroutine rpmgetvalint(str, res)
implicit none
! -- Inputs --
character(len=*), intent(in) :: str  ! ligne contenant la valeur
! -- Outputs --
integer        , intent(out) :: res  ! resultat, valeur de la ligne
! -- Internal variables --
integer i, iores

! -- Body --

  i = index(str, '=')
  read(str(i+1:), *, iostat=iores) res
  if (iores /= 0) call rpmerr("Integer read failed")

endsubroutine rpmgetvalint
!------------------------------------------------------------------------------!



!------------------------------------------------------------------------------!
! Procedure : rpmgetkeyvalrealsp/dp       Author : J. Gressier
!                                         Date   : Feb 2002
!                                         Modif  : Jul 2003
! Function :
!   Searches the "block" for the "key" and returns the expected result ("res")
!   of type real.
!
! Defaults/Limitations/Misc :
!   The key is supposed to be unique in the block.
!   An error is raised otherwise.
!   If the key is not found, the default value is used if provided.
!   ! The key must be in UPPERCASE !
!
!------------------------------------------------------------------------------!
subroutine rpmgetkeyvalrealsp(block, key, res, defval)
implicit none

! -- Inputs --
type(rpmblock), pointer :: block  ! Block looked for the key
character(len=*)        :: key    ! searched key
real(4), optional       :: defval ! default value
! -- Outputs --
real(4), intent(out)    :: res    ! result, key value
! -- Internal variables --
integer ntot, ilig

! -- Body --

  call seekinrpmblock(block, key, 0, ilig, ntot)

  select case(ntot)
    case(0)
      if (present(defval)) then
        res = defval
      else
        call rpmerr("Keyword "//trim(key)//" is not defined")
      endif
    case(1)
      call rpmgetvalreal(block%txt(ilig), res)
      block%flagtxt(ilig) = .true.
    case(2:)
      call rpmerr("Keyword "//trim(key)//" is multiply defined")
  endselect

endsubroutine rpmgetkeyvalrealsp
!------------------------------------------------------------------------------!


!------------------------------------------------------------------------------!
subroutine rpmgetkeyvalrealdp(block, key, res, defval)
implicit none

! -- Inputs --
type(rpmblock), pointer :: block  ! Block looked for the key
character(len=*)        :: key    ! searched key
real(8), optional       :: defval ! default value
! -- Outputs --
real(8), intent(out)    :: res    ! result, key value
! -- Internal variables --
integer ntot, ilig

! -- Body --

  call seekinrpmblock(block, key, 0, ilig, ntot)

  select case(ntot)
    case(0)
      if (present(defval)) then
        res = defval
      else
        call rpmerr("Keyword "//trim(key)//" is not defined")
      endif
    case(1)
      call rpmgetvalreal(block%txt(ilig), res)
      block%flagtxt(ilig) = .true.
    case(2:)
      call rpmerr("Keyword "//trim(key)//" is multiply defined")
  endselect

endsubroutine rpmgetkeyvalrealdp
!------------------------------------------------------------------------------!


!------------------------------------------------------------------------------!
! Procedure : rpmgetkeyvalstr             Author : J. Gressier
!                                         Date   : Feb 2002
!                                         Modif  : Nov 2002
! Function :
!   Searches the "block" for the "key" and returns the expected result ("res")
!   of type character(*).
!
! Defaults/Limitations/Misc :
!   The key is supposed to be unique in the block.
!   An error is raised otherwise.
!   If the key is not found, the default value is used if provided.
!   ! The key must be in UPPERCASE !
!
!------------------------------------------------------------------------------!
subroutine rpmgetkeyvalstr(block, key, res, defval)
implicit none

! -- Inputs --
type(rpmblock), pointer    :: block  ! Block looked for the key
character(len=*)           :: key    ! searched key
character(len=*), optional :: defval ! default value
! -- Outputs --
character(len=*), intent(out) :: res    ! result, key value
! -- Internal variables --
integer ntot, ilig

! -- Body --

  call seekinrpmblock(block, key, 0, ilig, ntot)

  select case(ntot)
    case(0)
      if (present(defval)) then
        res = defval
      else
        call rpmerr("Keyword "//trim(key)//" is not defined")
      endif
    case(1)
      call rpmgetvalstr(block%txt(ilig), res)
      block%flagtxt(ilig) = .true.
    case(2:)
      call rpmerr("Keyword "//trim(key)//" is multiply defined")
  endselect

endsubroutine rpmgetkeyvalstr
!------------------------------------------------------------------------------!



!------------------------------------------------------------------------------!
! Procedure : rpmgetkeyvalint             Author : J. Gressier
!                                         Date   : Feb 2002
!                                         Modif  : Nov 2002
! Function :
!   Searches the "block" for the "key" and returns the expected result ("res")
!   of type real.
!
! Defaults/Limitations/Misc :
!   The key is supposed to be unique in the block.
!   An error is raised otherwise.
!   If the key is not found, the default value is used if provided.
!   ! The key must be in UPPERCASE !
!
!------------------------------------------------------------------------------!
subroutine rpmgetkeyvalint(block, key, res, defval)
implicit none

! -- Inputs --
type(rpmblock), pointer :: block  ! Block looked for the key
character(len=*)        :: key    ! searched key
integer, optional       :: defval ! default value
! -- Outputs --
integer, intent(out)    :: res    ! result, key value
! -- Internal variables --
integer ntot, ilig

! -- Body --

  call seekinrpmblock(block, key, 0, ilig, ntot)

  select case(ntot)
    case(0)
      if (present(defval)) then
        res = defval
      else
        call rpmerr("Keyword "//trim(key)//" is not defined")
      endif
    case(1)
      call rpmgetvalint(block%txt(ilig), res)
      block%flagtxt(ilig) = .true.
    case(2:)
      call rpmerr("Keyword "//trim(key)//" is multiply defined")
  endselect

endsubroutine rpmgetkeyvalint
!------------------------------------------------------------------------------!


