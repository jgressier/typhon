module STRING

implicit none

! -- Variables globales du module -------------------------------------------

integer, parameter :: iposamin = iachar('a')
integer, parameter :: iposzmin = iachar('z')
integer, parameter :: iposamaj = iachar('A')
integer, parameter :: iposzmaj = iachar('Z')

!interface uppercase
!  module procedure charuppercase, struppercase
!endinterface

interface strof
  module procedure strof_int, strof_int2
endinterface

contains 

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
endfunction

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
endfunction

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
! Function : check if "str" is a readable real value
!------------------------------------------------------------------------------!
logical function is_real(str)
  implicit none
  character(len=*), intent(in) :: str
  character(len=len(str))      :: wstr
  integer                      :: i, is, idot, iexp, nch
  logical                      :: afterdot

  is_real = .true.
  wstr = adjustl(str)
  nch  = len_trim(wstr)         ! length of useful string
  iexp = scan(wstr, "eE")       ! index of Ennn ~ 10^nnn
  if (iexp == 0) iexp = nch+1   ! if no EE then default index at the end of string (+1)
  idot = index(wstr, '.')       ! index of dot separation
  if (idot == 0) idot = iexp    ! if no dot then default index at same index as iexp

  ! -- check string after "." and before "E" --

  if (idot+1 <= iexp-1) then
    afterdot = .true.
    do i = idot+1, iexp-1
      if (index("0123456789", wstr(i:i)) == 0) is_real = .false.
    enddo
  else
    afterdot = .false.
  endif

  ! -- check string after "E" --

  if (iexp == nch) is_real = .false.       ! ending "E" not allowed
  if ((iexp+1 <= nch)) then
    is = iexp+1
    ! -- if character is not the last, then it can be a sign -> increase index
    if ((is /= nch).and.(index("+-", wstr(is:is)) /= 0)) is = is +1
    ! -- all characters must be figures
    do i = is, nch
      if (index("0123456789", wstr(i:i)) == 0) is_real = .false.
    enddo
  endif

  ! -- check "main" string (before dot) --

  is = 1
  ! -- if character is a sign -> increase starting index
  if (index("+-", wstr(is:is)) /= 0) is = is +1
  if ((idot-1 >= is)) then
    ! -- all characters must be figures
    do i = is, idot-1
      if (index("0123456789", wstr(i:i)) == 0) is_real = .false.
    enddo
  else
    ! there is no main string : there should be a valid "afterdot" string
    is_real = is_real.and.afterdot   
  endif

endfunction is_real

!------------------------------------------------------------------------------!
! Fonction : tranformation entier -> chaine de caracteres (len=l)
!------------------------------------------------------------------------------!
function strof_int(nb, l) result(strout)
  implicit none
  integer, intent(in) :: nb, l   ! nombre a transformer, et longueur
  character(len=l)    :: strout  ! longueur de la chaine
  character(len=3) :: sform

  write(sform,'(i3)') l   
  write(strout,'(i'//trim(adjustl(sform))//')') nb
endfunction strof_int

!------------------------------------------------------------------------------!
! Fonction : tranformation entier -> chaine de caracteres (ajuste a gauche)
!------------------------------------------------------------------------------!
function strof_int2(nb) result(strout)
  implicit none
  integer, intent(in) :: nb      ! nombre a transformer, et longueur
  character(len=20)   :: strout  ! longueur de la chaine

  write(strout,'(i20)') nb
  strout = adjustl(strout)

endfunction strof_int2

!------------------------------------------------------------------------------!
! Fonction : tranformation entier -> chaine de caracteres (len=l)
!------------------------------------------------------------------------------!
function strof_full_int(nb, l) result(strout)
  implicit none
  integer, intent(in) :: nb, l   ! nombre a transformer, et longueur
  character(len=l)    :: strout  ! longueur de la chaine
  character(len=20)   :: sform
  integer             :: tl      ! trimmed length

  write(sform,'(i20)') nb
  sform  = adjustl(sform)
  tl     = len_trim(sform)
  strout = repeat('0',l-tl)//trim(sform)

endfunction strof_full_int

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
  samestring =      (index(trim(str1),trim(str2)) == 1) &
               .and.(index(trim(str2),trim(str1)) == 1)
endfunction samestring

!------------------------------------------------------------------------------!
! Fonction : Donne le nombre d'un caractere donne dans un chaine
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
