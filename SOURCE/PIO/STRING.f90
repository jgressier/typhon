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

contains 

!------------------------------------------------------------------------------!
! Fonction : Mise en majuscule d'un caractère
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
! Fonction : Mise en majuscule d'une chaîne de caractères
!------------------------------------------------------------------------------!
function uppercase(str) result(strout)
  implicit none
  character(len=*), intent(in) :: str
  character(len=len(str))      :: strout
  integer                      :: i

  do i = 1, len(str)
    strout(i:i) = uppercasechar(str(i:i))
  enddo
endfunction

!------------------------------------------------------------------------------!
! Fonction : Test logique d'égalité des chaînes de caractères
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
! Fonction : Donne le nombre d'un caractère donné dans un chaîne
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
! Procédure : Renvoie le n-ième mot d'une chaîne, séparateurs optionnels
!------------------------------------------------------------------------------!
subroutine nthword(nw, strin, strout, info, separator)
  implicit none
! -- entrées --
  character(len=*), intent(in)        :: strin      ! chaîne entrée
  character(len=*), intent(in)        :: separator  ! séparateur de mot
  integer                             :: nw         ! numéro du mot recherché
! -- sorties --
  character(len=*), intent(out)       :: strout     ! chaîne résultat
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
    i = scan(strout, separator)                  ! recherche des séparateurs
    if (len_trim(strout) == 0) info = -1   ! si chaîne remplie de blancs : erreur
    if (i < 0) then                        ! si pas de séparateurs : erreur
      info = -1
    else                                   ! sinon
      n      = n + 1                       ! on coupe le mot courant
      strout = adjustl(strout(i+1:len(strout)))
    endif
  enddo  

  if (info == 0) then                    ! on doit couper le reste de la chaîne
    i = scan(strout, separator)            ! recherche de séparateurs
    if (i < 0) i = len_trim(strout)        ! si il n'y en a pas : dernier mot
    strout = strout(1:i-1)
  endif
  !deallocate(sep)

endsubroutine nthword




endmodule STRING
