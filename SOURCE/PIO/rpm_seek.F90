!------------------------------------------------------------------------------!
! procedure : seekrpmblock                Auteur : J. Gressier
!                                         Date   : Avril 2002
! Fonction                                Modif  :
!   Recherche le pointeur d'un block donné de nom (nom), renvoie
!   - le pointeur du bloc cherché, NULL sinon
!   - le nombre d'occurences (ntot) trouvées, si (num) = 0
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine seekrpmblock(block, nom, num, blresu, ntot)
implicit none 

! -- Declaration des entrées --
type(rpmblock), pointer :: block   ! pointeur du bloc dans lequel chercher
character(len=*)        :: nom     ! mot clef à rechercher
integer                 :: num     ! numéro de l'occurence à chercher

! -- Declaration des sorties --
type(rpmblock), pointer :: blresu  ! pointeur du bloc resultat
integer                 :: ntot    ! nombre total d'occurence, calculé si num = 0

! -- Declaration des variables internes --
type(rpmblock), pointer :: pcour   ! pointeur de bloc courant
integer                 :: inum    ! numero de bloc satisfaisant courant

! -- Debut de la procedure --

  inum  = 0
  pcour => block 
  !print*,'start seek'
  do while (associated(pcour))
    !print*,'seek ', inum," :",pcour%name,":",nom
    if (samestring(nom, pcour%name)) then
      blresu => pcour
      inum   =  inum + 1
      if ((num /= 0).and.(inum == num)) exit ! si on cherche la num_ième, exit boucle
    endif
    pcour => pcour%next

  enddo
  
  if (num == 0) ntot = inum  

endsubroutine seekrpmblock
!------------------------------------------------------------------------------!




!------------------------------------------------------------------------------!
! procedure : seekinrpmblock              Auteur : J. Gressier
!                                         Date   : Fevrier 2002
! Fonction                                Modif  :
!   Recherche d'un mot clef (str) dans un bloc donné, renvoie
!   - la ligne (nlig) de la (num)-ième occurence (la première si num = 0)
!       renvoie 0 si l'occurence n'est pas trouvée
!   - le nombre d'occurences (ntot) trouvées (si num = 0)
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine seekinrpmblock(block, str, num, nlig, ntot)
implicit none 

! -- Declaration des entrées --
type(rpmblock), pointer :: block   ! pointeur du bloc dans lequel chercher
character(len=*)        :: str     ! mot clef à rechercher
integer                 :: num     ! numéro de l'occurence à chercher
! -- Declaration des sorties --
integer ntot   ! nombre total d'occurence, calculé si num = 0
integer nlig   ! numéro de ligne de la "num"_ième occurence

! -- Declaration des variables internes --
integer                  ilig, nkey
character(len=dimrpmlig) keyword

! -- Debut de la procedure --

  nlig = 0
  ilig = 1
  nkey = 0
  
  do while (ilig <= block%nblig)
    keyword = block%txt(ilig)(1:index(block%txt(ilig),'=')-1)
    
    if (samestring(str, keyword)) then
      nkey = nkey + 1
      if (num == 0) then           ! on compte toutes les occurences
        if (nkey == 1) nlig = ilig
      else
        if (nkey == num) then      ! on ne cherche que la "num"-ième
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
! function : rpm_existkey                 Auteur : J. Gressier
!                                         Date   : Novembre 2002
! Fonction                                Modif  :
!   Renvoie un booléen sur l'existence de la clef (key) dans le block (block)
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
logical function rpm_existkey(block, str)
implicit none 

! -- Declaration des entrées --
type(rpmblock), pointer :: block   ! pointeur du bloc dans lequel chercher
character(len=*)        :: str     ! mot clef à rechercher
! -- Declaration des sorties --
integer ntot   ! nombre total d'occurence, calculé si num = 0
integer nlig   ! numéro de ligne de la "num"_ième occurence

! -- Declaration des variables internes --
integer                  i, nkey

! -- Debut de la procedure --

  call seekinrpmblock(block, str, 0, i, nkey)
  rpm_existkey = (nkey >= 1)

endfunction rpm_existkey
!------------------------------------------------------------------------------!




!------------------------------------------------------------------------------!
! Fonction : numvar_inrpmdata             Auteur : J. Gressier
!                                         Date   : Fevrier 2002
! Fonction                                Modif  :
!   Recherche du numero de variable dans une structure RPMDATA
!
! Defauts/Limitations/Divers :
! - on suppose qu'il n'en existe une
! - renvoie 0 si inexistante
!
!------------------------------------------------------------------------------!
function numvar_inrpmdata(strvar, data)
implicit none 

! -- Declaration des entrées/sorties --
character(len=*)        :: strvar  ! nom de variable à chercher
type(rpmdata),  pointer :: data    ! structure de données concernée
integer                 :: numvar_inrpmdata

! -- Declaration des variables internes --
integer i

! -- Debut de la procedure --
  numvar_inrpmdata = 0
  do i = 1, data%nbvar
    if (samestring(strvar,data%name(i))) numvar_inrpmdata = i
  enddo
  
endfunction numvar_inrpmdata
!------------------------------------------------------------------------------!




!------------------------------------------------------------------------------!
! Procédure : seekrpmdata                 Auteur : J. Gressier
!                                         Date   : Fevrier 2002
! Fonction                                Modif  :
!   Recherche du (dernier) pointeur (data) contenant le nom de variable (strvar)
!   Le nombre total d'occurence de cette variable est renvoyé si num=0   
!
! Defauts/Limitations/Divers :
! - le nom de variable doit être en majuscules
!
!------------------------------------------------------------------------------!
subroutine seekrpmdata(block, strvar, num, data, ntot)
implicit none 

! -- Declaration des entrées --
type(rpmblock), pointer :: block   ! bloc auquel est limitée la recherche
character(len=*)        :: strvar  ! nom de variable à chercher
integer                 :: num     ! numero de l'occurence à chercher

! -- Declaration des sorties --
type(rpmdata),  pointer :: data    ! pointeur contenant la variable
integer                 :: ntot    ! nombre total d'occurences trouvées

! -- Declaration des variables internes --
type(rpmdata),  pointer :: pdata   ! pointeur intermédiaire
integer                 :: n       ! numero courant de l'occurence

! -- Debut de la procedure --

  n = 0
  pdata => block%data
  do while (associated(pdata))
    !print*,"seekdata"
    if (numvar_inrpmdata(strvar, pdata) /= 0) then
      n    =  n + 1
      data => pdata
      if ((num /= 0).and.(n == num)) exit ! si on cherche la num_ième, exit boucle
    endif
    pdata => pdata%next
  enddo
  ntot = n
  
endsubroutine seekrpmdata
!------------------------------------------------------------------------------!




!------------------------------------------------------------------------------!
! Procédure :                             Auteur : J. Gressier
!                                         Date   : Fevrier 2002
! Fonction                                Modif  :
!
! Parametres d'entree :
!
! Parametres de sortie :
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
!subroutine

!  use 
!  implicit none 

! -- Declaration des entrées --

! -- Declaration des sorties --

! -- Declaration des variables internes --

! -- Debut de la procedure --
  
!endsubroutine
!------------------------------------------------------------------------------!

