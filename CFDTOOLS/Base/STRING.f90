module STRING

implicit none

! -- Variables globales du module -------------------------------------------

integer, parameter :: iposamin = iachar('a')
integer, parameter :: iposzmin = iachar('z')
integer, parameter :: iposamaj = iachar('A')
integer, parameter :: iposzmaj = iachar('Z')

! -- INTERFACES -------------------------------------------------------------

type st_string
  integer            :: len
  character, pointer :: c(:)
endtype

interface strof ! adjust left
  module procedure strof_int2, strof_int4, strof_real, strof_double !, strof_realf, strof_doublef, strof_reale
endinterface

interface strofr ! adjust right
  module procedure strofr_int2, strofr_int4
endinterface

interface strofe  ! exponential form
  module procedure strof_reale, strof_doublee, strofr_reale, strofr_doublee
endinterface

interface stroff ! decimal form
  module procedure strof_realf, strof_doublef, strofr_realf, strofr_doublef
endinterface

contains

!------------------------------------------------------------------------------!
! Fonction : newstring from character "array"
!------------------------------------------------------------------------------!
subroutine newstring_s(string, str)
  implicit none
  type(st_string)              :: string
  character(len=*), intent(in) :: str
  ! -- body --
  string%len = len(str)
  allocate(string%c(string%len))
  string%c = str
endsubroutine

!------------------------------------------------------------------------------!
! Fonction : newstring from character "array"
!------------------------------------------------------------------------------!
subroutine newstring_n(string, l)
  implicit none
  type(st_string) :: string
  integer         :: l
  ! -- body --
  string%len = l
  allocate(string%c(l))
endsubroutine

!------------------------------------------------------------------------------!
! Fonction : delete string
!------------------------------------------------------------------------------!
subroutine deletestring(string)
  implicit none
  type(st_string)              :: string
  deallocate(string%c)
endsubroutine

!------------------------------------------------------------------------------!
! Fonction : Mise en minuscule d'un caractere
!------------------------------------------------------------------------------!
function lowercasechar(c)
  implicit none
  character, intent(in) :: c
  character             :: lowercasechar
  integer               :: i

  i = iachar(c)
  select case(i)
    case(iposamaj:iposzmaj)
      lowercasechar = achar(i-iposamaj+iposamin)
    case default
      lowercasechar = c
  endselect
endfunction lowercasechar

!------------------------------------------------------------------------------!
! Fonction : Mise en majuscule d'un caractere
!------------------------------------------------------------------------------!
function uppercasechar(c)
  implicit none
  character, intent(in) :: c
  character             :: uppercasechar
  integer               :: i

  i = iachar(c)
  select case(i)
    case(iposamin:iposzmin)
      uppercasechar = achar(i-iposamin+iposamaj)
    case default
      uppercasechar = c
  endselect
endfunction uppercasechar

!------------------------------------------------------------------------------!
! Fonction : Mise en minuscule d'une chaine de caracteres
!------------------------------------------------------------------------------!
function lowercase(str) result(strout)
  implicit none
  character(len=*), intent(in) :: str
  character(len=len(str))      :: strout
  integer                      :: i

  do i = 1, len(str)
    strout(i:i) = lowercasechar(str(i:i))
  enddo
endfunction lowercase

!------------------------------------------------------------------------------!
! Fonction : Mise en majuscule d'une chaine de caracteres
!------------------------------------------------------------------------------!
function uppercase(str) result(strout)
  implicit none
  character(len=*), intent(in) :: str
  character(len=len(str))      :: strout
  integer                      :: i

  do i = 1, len(str)
    strout(i:i) = uppercasechar(str(i:i))
  enddo
endfunction uppercase

!------------------------------------------------------------------------------!
! Fonction : Remove space characters
!------------------------------------------------------------------------------!
function delspace(str) result(strout)
  implicit none
  character(len=*), intent(in) :: str
  character(len=len(str))      :: strout
  integer                      :: length, i1, i2

  strout = ""  !!! WHY IS THIS USEFUL ? but it works: removes bug...
  i2     = 0
  length = len(str)
  do i1 = 1, length
    if (str(i1:i1) /= ' ') then
      i2 = i2 + 1
      strout(i2:i2) = str(i1:i1)
    endif
  enddo
  if (i2+1 >= length) strout(i2+1:length) = ' '    ! blank ending characters

endfunction delspace

!------------------------------------------------------------------------------!
! Fonction : Remplacement de caractere
!------------------------------------------------------------------------------!
function chg_char(str, c, r) result(strout)
  implicit none
  character(len=*), intent(in) :: str
  character                    :: c, r
  character(len=len(str))      :: strout
  integer                      :: i

  strout = str
  do i = 1, len(str)
    if (strout(i:i) == c) strout(i:i) = r
  enddo
endfunction chg_char

!------------------------------------------------------------------------------!
! Function : character transposition
!------------------------------------------------------------------------------!
function str_tr(str, c, r) result(strout)
  implicit none
  character(len=*),     intent(in) :: str  ! string to be processed
  character(len=*),     intent(in) :: c    ! character transposed
  character(len=len(c)),intent(in) :: r    ! new character
  character(len=len(str))      :: strout   ! output string
  integer                      :: i, j

  strout = str
  do i = 1, len(str)
    do j = 1, len(c)
      if (str(i:i) == c(j:j)) strout(i:i) = r(j:j)
    enddo
  enddo
endfunction str_tr

!------------------------------------------------------------------------------!
! Function : get root/base name and remove suffix
!------------------------------------------------------------------------------!
subroutine addstr(base, suff, pos)
  implicit none
  character(len=*), intent(inout) :: base
  character(len=*), intent(in)    :: suff
  integer, intent(inout)          :: pos
  integer                         :: l
  l = len(suff)
  base(pos:pos+l-1) = suff(1:l)
  pos = pos+l
endsubroutine


!------------------------------------------------------------------------------!
! Function : get root/base name and remove suffix
!------------------------------------------------------------------------------!
function basename(str, suffix) result(strout)
  implicit none
  character(len=*), intent(in) :: str, suffix
  character(len=len_trim(str)) :: strout
  integer                      :: i

  i = index(str, '.'//suffix)
  if ((i == 0).or.(i /= (len_trim(str)-len_trim(suffix)))) then
    strout = str
  else
    strout = str(1:i-1)
  endif
endfunction basename

!------------------------------------------------------------------------------!
! Function : check if "str" consists of dec/hex digits ONLY (NO ignore blank)
!------------------------------------------------------------------------------!
#define verify_decdigits(str) verify(str,"0123456789")
#define verify_hexdigits(str) verify(str,"0123456789abcdef")
#define all_decdigits(str) ( verify_decdigits(str) == 0 )
#define all_hexdigits(str) ( verify_hexdigits(str) == 0 )

!!------------------------------------------------------------------------------!
!! Function : check if "str" consists of decimal digits ONLY (NO ignore blank)
!!------------------------------------------------------------------------------!
!logical function all_decdigits(str)
!
!  implicit none
!  character(len=*), intent(in) :: str
!  integer                      :: i
!
!  all_decdigits = ( verify(str,"0123456789") == 0 )
!
!!  all_decdigits = .false.
!!  do i = 1, len(str)
!!    if (index("0123456789", str(i:i)) == 0) return
!!  enddo
!!  all_decdigits = .true.
!
!endfunction all_decdigits

!!------------------------------------------------------------------------------!
!! Function : check if "str" consists of hex digits ONLY (NO ignore blank)
!!------------------------------------------------------------------------------!
!logical function all_hexdigits(str)
!
!  implicit none
!  character(len=*), intent(in) :: str
!  integer                      :: i
!
!  all_hexdigits = ( verify(str,"0123456789abcdef") == 0 )
!
!!  all_hexdigits = .false.
!!  do i = 1, len(str)
!!    if (index("0123456789abcdef", str(i:i)) == 0) return
!!  enddo
!!  all_hexdigits = .true.
!
!endfunction all_hexdigits

!------------------------------------------------------------------------------!
! Function : check if "str" is a readable integer value
!------------------------------------------------------------------------------!
logical function is_int(str)

  implicit none
  character(len=*), intent(in) :: str
  character(len=len(str))      :: wstr
  integer                      :: is

  is_int = .false.
  wstr = adjustl(str)
  is = 1 + scan(wstr(1:1), "+-")
  is_int = all_decdigits(wstr(is:len_trim(wstr)))

endfunction is_int

!------------------------------------------------------------------------------!
! Function : check if "str" is a readable real value
!------------------------------------------------------------------------------!
logical function is_real(str)

  implicit none
  character(len=*), intent(in) :: str
  character(len=len(str))      :: wstr
  integer                      :: i, is, idot, iexp, lstr
  logical                      :: fractpart

  is_real = .false.             ! to return early if not real
  wstr = adjustl(str)
  lstr = len_trim(wstr)         ! length of useful string

  iexp = scan(wstr, "dDeE")     ! index of exponent separator (e.g. "E")
  if (iexp == 0) iexp = lstr+1  ! if no expsep then default index at (end of string)+1
  idot = index(wstr, ".")       ! index of decimal separator (".")
  if (idot == 0) idot = iexp    ! if no decsep then default index at iexp

  ! -- check string after "." and before "E" --

  if (idot+1 <= iexp-1) then
    ! -- all characters must be decimal digits
    if (.NOT. all_decdigits(wstr(idot+1:iexp-1))) return
    fractpart = .true.
  else
    fractpart = .false.
  endif

  ! -- check string after "E" --

  if (iexp == lstr) return      ! trailing "E" not allowed
  if (iexp+1 <= lstr) then
    is = iexp + 1
    ! -- if character is not the last, check optional sign
    if (is /= lstr) is = is + scan(wstr(is:is), "+-")
    ! -- all remaining characters must be decimal digits
    if (.NOT. all_decdigits(wstr(is:lstr))) return
  endif

  ! -- check integer part (string before ".") --

  ! -- check optional leading sign --
  is = 1 + scan(wstr(1:1), "+-")

  if (idot-1 >= is) then
    ! -- all characters must be decimal digits
    if (.NOT. all_decdigits(wstr(is:idot-1))) return
  else
    ! -- no integer part : there should be a valid "fractpart" string
    if (.NOT. fractpart) return
  endif
  is_real = .true.

endfunction is_real

!------------------------------------------------------------------------------!
! Fonction : tranformation entier -> chaine de caracteres (len=l)
!------------------------------------------------------------------------------!
function strofr_int2(nb, l) result(strout)
  implicit none
  integer(2), intent(in) :: nb    ! integer to convert
  integer,    intent(in) :: l     ! string length
  character(len=l)    :: strout   ! string
  character(len=3) :: sform

  write(sform,'(i3)') l
  write(strout,'(i'//trim(adjustl(sform))//')') nb
endfunction strofr_int2

!------------------------------------------------------------------------------!
! Fonction : tranformation entier -> chaine de caracteres (len=l)
!------------------------------------------------------------------------------!
function strofr_int4(nb, l) result(strout)
  implicit none
  integer(4), intent(in) :: nb    ! integer to convert
  integer,    intent(in) :: l     ! string length
  character(len=l)    :: strout   ! string
  character(len=3) :: sform

  write(sform,'(i3)') l
  write(strout,'(i'//trim(adjustl(sform))//')') nb
endfunction strofr_int4

!------------------------------------------------------------------------------!
! Fonction : tranformation entier -> chaine de caracteres (ajuste a gauche)
!------------------------------------------------------------------------------!
function strof_int2(nb) result(strout)
  implicit none
  integer(2), intent(in) :: nb      ! integer to convert
  character(len=20)      :: strout  ! string

  write(strout,'(i20)') nb
  strout = adjustl(strout)
endfunction strof_int2

!------------------------------------------------------------------------------!
! Fonction : tranformation entier -> chaine de caracteres (ajuste a gauche)
!------------------------------------------------------------------------------!
function strof_int4(nb) result(strout)
  implicit none
  integer(4), intent(in) :: nb      ! integer to convert
  character(len=20)      :: strout  ! string

  write(strout,'(i20)') nb
  strout = adjustl(strout)
endfunction strof_int4

!------------------------------------------------------------------------------!
! Fonction : tranformation entier -> chaine de caracteres (len=l)
!------------------------------------------------------------------------------!
function strof_full_int(nb, l) result(strout)
  implicit none
  integer, intent(in) :: nb, l   ! integer to write and string length
  character(len=l)    :: strout  ! output string
  character(len=20)   :: sform   ! format string

  write(sform,'(a2,i3.3,a1,i3.3,a1)') "(i",l,".",l,")"
  write(strout,trim(sform)) nb
endfunction strof_full_int

!------------------------------------------------------------------------------!
! Fonction : tranformation real -> chaine de caracteres (ajuste a gauche)
!------------------------------------------------------------------------------!
function strof_real(nb) result(strout)
  implicit none
  real(4), intent(in)  :: nb      ! nombre a transformer, et longueur
  character(len=20)    :: strout  ! longueur de la chaine

  write(strout,'(e15.8)') nb
  strout = adjustl(strout)
endfunction strof_real

!------------------------------------------------------------------------------!
! Fonction : tranformation real -> chaine de caracteres (ajuste a gauche)
!------------------------------------------------------------------------------!
function strof_reale(nb, d) result(strout)
  implicit none
  real(4), intent(in)  :: nb      ! nombre a transformer, et longueur
  integer, intent(in)  :: d
  character(len=20)    :: strout  ! longueur de la chaine

  write(strout,'(e20.'//trim(strof_int4(d))//')') nb
  strout = adjustl(strout)
endfunction strof_reale

!------------------------------------------------------------------------------!
! Fonction : tranformation real -> chaine de caracteres (ajuste a gauche)
!------------------------------------------------------------------------------!
function strof_realf(nb, d) result(strout)
  implicit none
  real(4), intent(in)  :: nb      ! nombre a transformer, et longueur
  integer, intent(in)  :: d
  character(len=20)    :: strout  ! longueur de la chaine

  write(strout,'(f15.'//trim(strof_int4(d))//')') nb
  strout = adjustl(strout)
endfunction strof_realf

!------------------------------------------------------------------------------!
! Fonction : tranformation real -> chaine de caracteres
!------------------------------------------------------------------------------!
function strofr_reale(nb, l, d) result(strout)
  implicit none
  real(4), intent(in)  :: nb      ! nombre a transformer, et longueur
  integer, intent(in)  :: l, d
  character(len=l)    :: strout  ! longueur de la chaine

  write(strout,'(e'//trim(strof_int4(l))//'.'//trim(strof_int4(d))//')') nb
endfunction strofr_reale

!------------------------------------------------------------------------------!
! Fonction : tranformation real -> chaine de caracteres
!------------------------------------------------------------------------------!
function strofr_realf(nb, l, d) result(strout)
  implicit none
  real(4), intent(in)  :: nb      ! nombre a transformer, et longueur
  integer, intent(in)  :: l, d
  character(len=l)    :: strout  ! longueur de la chaine

  write(strout,'(f'//trim(strof_int4(l))//'.'//trim(strof_int4(d))//')') nb
endfunction strofr_realf

!------------------------------------------------------------------------------!
! Fonction : tranformation real -> chaine de caracteres (ajuste a gauche)
!------------------------------------------------------------------------------!
function strof_double(nb) result(strout)
  implicit none
  real(8), intent(in)  :: nb      ! nombre a transformer, et longueur
  character(len=25)    :: strout  ! longueur de la chaine

  write(strout,'(e24.15)') nb
  strout = adjustl(strout)
endfunction strof_double

!------------------------------------------------------------------------------!
! Fonction : tranformation real -> chaine de caracteres (ajuste a gauche)
!------------------------------------------------------------------------------!
function strof_doublef(nb, d) result(strout)
  implicit none
  real(8), intent(in)  :: nb      ! nombre a transformer, et longueur
  integer, intent(in)  :: d
  character(len=20)    :: strout  ! longueur de la chaine

  write(strout,'(f15.'//trim(strof_int4(d))//')') nb
  strout = adjustl(strout)
endfunction strof_doublef

!------------------------------------------------------------------------------!
! Fonction : tranformation real -> chaine de caracteres (ajuste a gauche)
!------------------------------------------------------------------------------!
function strof_doublee(nb, d) result(strout)
  implicit none
  real(8), intent(in)  :: nb      ! nombre a transformer, et longueur
  integer, intent(in)  :: d
  character(len=20)    :: strout  ! longueur de la chaine

  write(strout,'(e20.'//trim(strof_int4(d))//')') nb
  strout = adjustl(strout)
endfunction strof_doublee

!------------------------------------------------------------------------------!
! Fonction : tranformation real -> chaine de caracteres
!------------------------------------------------------------------------------!
function strofr_doublee(nb, l, d) result(strout)
  implicit none
  real(8), intent(in)  :: nb      ! nombre a transformer, et longueur
  integer, intent(in)  :: l, d
  character(len=l)    :: strout  ! longueur de la chaine

  write(strout,'(e'//trim(strof_int4(l))//'.'//trim(strof_int4(d))//')') nb
endfunction strofr_doublee

!------------------------------------------------------------------------------!
! Fonction : tranformation real -> chaine de caracteres
!------------------------------------------------------------------------------!
function strofr_doublef(nb, l, d) result(strout)
  implicit none
  real(8), intent(in)  :: nb      ! nombre a transformer, et longueur
  integer, intent(in)  :: l, d
  character(len=l)    :: strout  ! longueur de la chaine

  write(strout,'(f'//trim(strof_int4(l))//'.'//trim(strof_int4(d))//')') nb
endfunction strofr_doublef

!------------------------------------------------------------------------------!
! Fonction : Test logique d'egalite des chaines de caracteres
!------------------------------------------------------------------------------!
function samestring(str1, str2)
  implicit none
  character(len=*), intent(in) :: str1, str2
  logical                      :: samestring

  !print*,"samestring: ",index(trim(str1),trim(str2))," ",&
  !index(trim(str2),trim(str1))
  !print*,"samestring:",trim(str1),"#",trim(str2)
  !samestring =      (index(trim(str1),trim(str2)) == 1) &
  !             .and.(index(trim(str2),trim(str1)) == 1)
  samestring = (str1==str2)
endfunction samestring

!------------------------------------------------------------------------------!
! Fonction : fill with blanks
!------------------------------------------------------------------------------!
function fill(str, l)  result(strout)
  implicit none
  character(len=l) :: strout
  character(len=*), intent(in) :: str
  integer                      :: l, lstr

  lstr = len(str)
  if (l > lstr) then
    strout = str//repeat(' ',l-lstr)
  else
    strout = str
  endif
endfunction fill

!------------------------------------------------------------------------------!
! Fonction : Donne le nombre d'occurrences d'un caractere donne dans une chaine
!------------------------------------------------------------------------------!
function numbchar(str, c)
  implicit none
  character(len=*), intent(in) :: str
  character,        intent(in) :: c
  integer                      :: numbchar

  integer ideb, ipos, nb

  nb   = 0
  ideb = 1
  ipos = index(str(ideb:),c)
  do while (ipos /= 0)
    nb   = nb + 1
    ideb = ideb + ipos
    ipos = index(str(ideb:),c)
  enddo
  numbchar = nb
endfunction numbchar

!------------------------------------------------------------------------------!
! Procedure : Renvoie un tableau des elements (chaines) separes de la chaine
!             Le separateur optionnel est l'espace par defaut
!             Les elements vides sont ignores
!------------------------------------------------------------------------------!
subroutine splitstring_string(strin, nw, strout, ierr, separator)

  implicit none
! -- entrees --
  character(len=*), intent(in)                   :: strin      ! chaine entree
  character(len=*), intent(in), optional, target :: separator  ! separateur de mot
! -- sorties --
  integer         , intent(out)               :: nw     ! nombre de mots
  character(len=*), intent(out), dimension(:) :: strout ! tableau de chaine resultat
  integer         , intent(out), optional     :: ierr   ! -1 si erreur
! -- variables internes --
  integer       :: id, is, lstr      ! entiers provisoires
  integer       :: nwmax
  character, target  :: sepstr = ':'
  character, pointer :: sep

  if (present(separator)) then
    sep => separator
  else
    sep => sepstr
  endif
  ierr = 0
  id   = 1
  nw   = 0
  nwmax = size(strout)
  lstr   = len_trim(strin)
  do while (ierr == 0 .and. id<=lstr)
    is = scan(strin(id:), separator)            ! recherche des separateurs
    if (is==0) then                             ! si pas de separateur
      is = lstr + 1                             !   le mot restant est retenu
    else                                        ! si separateur
      is = is + id - 1                          !   le mot est retenu
    endif
    if (len_trim(strin(id:is-1))/=0) then       ! si le mot est non vide
      nw = nw + 1                               !   index est incremente
      if (nw<=nwmax) then                       !   si index convient
        strout(nw) = adjustl(strin(id:is-1))    !     le mot est ajoute
      endif
    endif
    id = is + 1
  enddo
  if (nw>nwmax) nw = -nw                        ! si index superieur au max, signe -

endsubroutine splitstring_string

!------------------------------------------------------------------------------!
! Procedure : Renvoie un tableau des elements (entiers) separes de la chaine
!             Le separateur optionnel est l'espace par defaut
!             Les elements vides sont ignores
!------------------------------------------------------------------------------!
subroutine splitstring_integer(strin, nw, intout, ierr, separator)

  implicit none
! -- entrees --
  character(len=*), intent(in)                   :: strin      ! chaine entree
  character(len=*), intent(in), optional, target :: separator  ! separateur de mot
! -- sorties --
  integer, intent(out)               :: nw     ! nombre de mots
  integer, intent(out), dimension(:) :: intout ! tableau d entier resultat
  integer, intent(out), optional     :: ierr   ! -1 si erreur
! -- variables internes --
  integer       :: id, is, lstr      ! entiers provisoires
  integer       :: nwmax
  character, target  :: sepstr = ':'
  character, pointer :: sep

  if (present(separator)) then
    sep => separator
  else
    sep => sepstr
  endif
  ierr = 0
  id   = 1
  nw   = 0
  nwmax = size(intout)
  lstr   = len_trim(strin)
  do while (ierr == 0 .and. id<=lstr)
    is = scan(strin(id:), separator)            ! recherche des separateurs
    if (is==0) then                             ! si pas de separateur
      is = lstr + 1                             !   le mot restant est retenu
    else                                        ! si separateur
      is = is + id - 1                          !   le mot est retenu
    endif
    if (len_trim(strin(id:is-1))/=0) then       ! si le mot est non vide
      nw = nw + 1                               !   index est incremente
      if (nw<=nwmax) then                       !   si index inferieur au max
        if (is_int(strin(id:is-1))) then        !     si on a un entier
          read(strin(id:is-1),*,iostat=ierr) intout(nw)!le nbre entier est ajoute
        else                                    !     sinon
          ierr = 1                              !       erreur
        endif
      endif
    endif
    id = is + 1
  enddo
  if (nw>nwmax) nw = -nw                        ! si index superieur au max, signe -

endsubroutine splitstring_integer

!------------------------------------------------------------------------------!
! Procedure : Renvoie le n-ieme mot d'une chaine, separateurs optionnels
!------------------------------------------------------------------------------!
subroutine nthword(nw, strin, strout, info, separator)

  implicit none
! -- entrees --
  character(len=*), intent(in)        :: strin      ! chaine entree
  character(len=*), intent(in)        :: separator  ! separateur de mot
  integer                             :: nw         ! numero du mot recherche
! -- sorties --
  character(len=*), intent(out)       :: strout     ! chaine resultat
  integer                             :: info       ! -1 si erreur
! -- variables internes --
  integer                             :: i, n       ! entiers provisoires

  !if (present(separator)) then
  !  allocate(sep(len(separator)))
  !  sep = separator
  !else
  !  allocate(sep(1))
  !  sep = " "
  !endif

  info   = 0
  n      = 1
  strout = adjustl(strin)

  do while ((info == 0).and.(n /= nw))   ! teste le numero du mot
    i = scan(strout, separator)                  ! recherche des separateurs
    if (len_trim(strout) == 0) info = -1   ! si chaine remplie de blancs : erreur
    if (i < 0) then                        ! si pas de separateurs : erreur
      info = -1
    else                                   ! sinon
      n      = n + 1                       ! on coupe le mot courant
      strout = adjustl(strout(i+1:len(strout)))
    endif
  enddo

  if (info == 0) then                    ! on doit couper le reste de la chaine
    i = scan(strout, separator)            ! recherche de separateurs
    if (i < 0) i = len_trim(strout)        ! si il n'y en a pas : dernier mot
    strout = strout(1:i-1)
  endif
  !deallocate(sep)

endsubroutine nthword

!------------------------------------------------------------------------------!
! Procedure : Renvoie l'index de parenthese fermante associee
!------------------------------------------------------------------------------!
integer function index_rightpar (str, ip, info)

  implicit none
! -- entrees --
  character(len=*), intent(in) :: str        ! chaine entree
  integer                      :: ip         ! index de parenthese ouvrante
! -- sorties --
  integer                      :: info       ! nombre de parentheses non fermees
! -- variables internes --
  integer                      :: np           ! nombre de parentheses ouvrantes
  integer                      :: len          ! longueur totale de chaine
  integer                      :: i, ipl, ipr  ! index de chaine

  len    = len_trim(str)
  np     = 1
  i      = ip+1
  do while ((i <= len).and.(np > 0))
    select case(str(i:i))
    case('(')
      np = np + 1
    case(')')
      np = np - 1
    endselect
    i = i + 1
  enddo
  info           = np
  index_rightpar = i-1

endfunction index_rightpar


endmodule STRING
!------------------------------------------------------------------------------!
! Changes
!------------------------------------------------------------------------------!
