!------------------------------------------------------------------------------!
! rpmgetvalreal : renvoie la valeur du second membre de la ligne (str) en real(sp)
!------------------------------------------------------------------------------!
subroutine rpmgetvalrealsp(str, res)
implicit none 
character(len=*)  :: str  ! ligne contenant la valeur    (entrée)
real(4)           :: res  ! resultat, valeur de la ligne (sortie)

! -- Declaration des variables internes --
integer           :: i, iores

! -- Debut de la procedure --

  i = index(str, '=')
  read(str(i+1:), *, iostat=iores) res
  if (iores /= 0) call rpmerr("Lecture de nombre réel impossible")

endsubroutine rpmgetvalrealsp
!------------------------------------------------------------------------------!


!------------------------------------------------------------------------------!
! rpmgetvalreal : renvoie la valeur du second membre de la ligne (str) en real
!------------------------------------------------------------------------------!
subroutine rpmgetvalrealdp(str, res)
implicit none 
character(len=*)  :: str  ! ligne contenant la valeur    (entrée)
real(8)           :: res  ! resultat, valeur de la ligne (sortie)

! -- Declaration des variables internes --
integer           :: i, iores

! -- Debut de la procedure --

  i = index(str, '=')
  read(str(i+1:), *, iostat=iores) res
  if (iores /= 0) call rpmerr("Lecture de nombre réel impossible")

endsubroutine rpmgetvalrealdp
!------------------------------------------------------------------------------!


!------------------------------------------------------------------------------!
! rpmgetvalstr : renvoie la valeur du second membre de la ligne (str) en chaine
!------------------------------------------------------------------------------!
subroutine rpmgetvalstr(str, res)
implicit none 

! -- Declaration des entrées --
character(len=*), intent(in) :: str  ! ligne contenant la valeur
! -- Declaration des sorties --
!character(len=len_trim(str)) :: res  ! resultat, valeur de la ligne
character(len=dimrpmlig) :: res  ! resultat, valeur de la ligne

! -- Declaration des variables internes --
integer i, iores

! -- Debut de la procedure --

  i   = index(str, '=')                   ! extraction de la chaine suivant =
  res = trim(adjustl(str(i+1:len(str))))

  i   = len_trim(res)                     ! test de chaine entre quotes  
  if ((res(1:1) == rpmquotechar).and.(res(i:i) == rpmquotechar)) then
    res = res(2:i-1)
  endif

endsubroutine rpmgetvalstr
!------------------------------------------------------------------------------!


!------------------------------------------------------------------------------!
! rpmgetvalint : renvoie la valeur du second membre de la ligne (str) en entier
!------------------------------------------------------------------------------!
subroutine rpmgetvalint(str, res)
implicit none 

! -- Declaration des entrées --
character(len=*)  :: str  ! ligne contenant la valeur
! -- Declaration des sorties --
integer           :: res  ! resultat, valeur de la ligne

! -- Declaration des variables internes --
integer i, iores

! -- Debut de la procedure --

  i = index(str, '=')
  read(str(i+1:), *, iostat=iores) res
  if (iores /= 0) call rpmerr("Lecture d'entier impossible")

endsubroutine rpmgetvalint
!------------------------------------------------------------------------------!



!------------------------------------------------------------------------------!
! Procedure : rpmgetkeyvalrealsp/dp       Auteur : J. Gressier
!                                         Date   : Fevrier 2002
! Fonction                                Modif  : Juillet 2003
!   Cherche dans le block (block), la clef (key) et renvoie le resultat (res)
!   attendu de type real. 
!
! Defauts/Limitations/Divers :
!   La clef est censée être unique dans le bloc donné. Il y a génération
!   d'une erreur dans le cas contraire. Si la clef n'existe pas, la valeur
!   par défaut est affectée si elle est fournie.
!   ! La clef doit être en majuscules !
!
!------------------------------------------------------------------------------!
subroutine rpmgetkeyvalrealsp(block, key, res, defval)
implicit none 

! -- Declaration des entrées --
type(rpmblock), pointer :: block  ! block censé contenir la clef
character(len=*)        :: key    ! clef à rechercher
real(4), optional       :: defval ! valeur par défaut
! -- Declaration des sorties --
real(4)                 :: res    ! resultat, valeur de la clef

! -- Declaration des variables internes --
integer ntot, ilig

! -- Debut de la procedure --

  call seekinrpmblock(block, key, 0, ilig, ntot)
  
  select case(ntot)
    case(0)
      if (present(defval)) then
        res = defval
      else
        call rpmerr("Le mot-clef "//trim(key)//" n'existe pas")
      endif
    case(1)
      call rpmgetvalreal(block%txt(ilig), res)
      block%flagtxt(ilig) = .true.
    case(2:)
      call rpmerr("Le mot-clef "//trim(key)//" n'est pas unique")
  endselect
  
endsubroutine rpmgetkeyvalrealsp
!------------------------------------------------------------------------------!


!------------------------------------------------------------------------------!
subroutine rpmgetkeyvalrealdp(block, key, res, defval)
implicit none 

! -- Declaration des entrées --
type(rpmblock), pointer :: block  ! block censé contenir la clef
character(len=*)        :: key    ! clef à rechercher
real(8), optional       :: defval ! valeur par défaut
! -- Declaration des sorties --
real(8)                 :: res    ! resultat, valeur de la clef

! -- Declaration des variables internes --
integer ntot, ilig

! -- Debut de la procedure --

  call seekinrpmblock(block, key, 0, ilig, ntot)
  
  select case(ntot)
    case(0)
      if (present(defval)) then
        res = defval
      else
        call rpmerr("Le mot-clef "//trim(key)//" n'existe pas")
      endif
    case(1)
      call rpmgetvalreal(block%txt(ilig), res)
      block%flagtxt(ilig) = .true.
    case(2:)
      call rpmerr("Le mot-clef "//trim(key)//" n'est pas unique")
  endselect
  
endsubroutine rpmgetkeyvalrealdp
!------------------------------------------------------------------------------!


!------------------------------------------------------------------------------!
! Procedure : rpmgetkeyvalstr             Auteur : J. Gressier
!                                         Date   : Fevrier 2002
! Fonction                                Modif  : Novembre 2002
!   Cherche dans le block (block), la clef (key) et renvoie le resultat (res)
!   attendu de type character(*). 
!
! Defauts/Limitations/Divers :
!   La clef est censée être unique dans le bloc donné. Il y a génération
!   d'une erreur dans le cas contraire. Si la clef n'existe pas, la valeur
!   par défaut est affectée si elle est fournie.
!   ! La clef doit être en majuscules !
!
!------------------------------------------------------------------------------!
subroutine rpmgetkeyvalstr(block, key, res, defval)
implicit none 

! -- Declaration des entrées --
type(rpmblock), pointer    :: block  ! block censé contenir la clef
character(len=*)           :: key    ! clef à rechercher
character(len=*), optional :: defval ! valeur par défaut

! -- Declaration des sorties --
character(len=*), intent(out) :: res    ! resultat, valeur de la clef

! -- Declaration des variables internes --
integer ntot, ilig

! -- Debut de la procedure --

  call seekinrpmblock(block, key, 0, ilig, ntot)
  
  select case(ntot)
    case(0)
      if (present(defval)) then
        res = defval
      else
        call rpmerr("Le mot-clef "//trim(key)//" n'existe pas")
      endif
    case(1)
      call rpmgetvalstr(block%txt(ilig), res)
      block%flagtxt(ilig) = .true.
    case(2:)
      call rpmerr("Le mot-clef "//trim(key)//" n'est pas unique")
  endselect

endsubroutine rpmgetkeyvalstr
!------------------------------------------------------------------------------!



!------------------------------------------------------------------------------!
! Procedure : rpmgetkeyvalint             Auteur : J. Gressier
!                                         Date   : Fevrier 2002
! Fonction                                Modif  : Novembre 2002
!   Cherche dans le block (block), la clef (key) et renvoie le resultat (res)
!   attendu de type real. 
!
! Defauts/Limitations/Divers :
!   La clef est censée être unique dans le bloc donné. Il y a génération
!   d'une erreur dans le cas contraire. Si la clef n'existe pas, la valeur
!   par défaut est affectée si elle est fournie.
!   ! La clef doit être en majuscules !
!
!------------------------------------------------------------------------------!
subroutine rpmgetkeyvalint(block, key, res, defval)
implicit none 

! -- Declaration des entrées --
type(rpmblock), pointer :: block  ! block censé contenir la clef
character(len=*)        :: key    ! clef à rechercher
integer, optional       :: defval ! valeur par défaut

! -- Declaration des sorties --
integer                 :: res    ! resultat, valeur de la clef

! -- Declaration des variables internes --
integer ntot, ilig

! -- Debut de la procedure --

  call seekinrpmblock(block, key, 0, ilig, ntot)
  
  select case(ntot)
    case(0)
      if (present(defval)) then
        res = defval
      else
        call rpmerr("Le mot-clef "//trim(key)//" n'existe pas")
      endif
    case(1)
      call rpmgetvalint(block%txt(ilig), res)
      block%flagtxt(ilig) = .true.
    case(2:)
      call rpmerr("Le mot-clef "//trim(key)//" n'est pas unique")
  endselect

endsubroutine rpmgetkeyvalint
!------------------------------------------------------------------------------!


