!------------------------------------------------------------------------------!
! Procedure : readrpmblock                Auteur : J. Gressier
!                                         Date   : Fevrier 2002
! Fonction                                Modif  :
!   Lecture et mise en buffer des blocs de lignes dans des structures
!   RPMBLOCK gérées en liste.
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine readrpmblock(nio, nerr, iaff, firstblock)
  use STRING
  implicit none 

! -- Declaration des entrées --
  integer nio    ! numero d'unite pour la lecture 
  integer nerr   ! type d'erreur en lecture de paramètres
  integer iaff   ! choix d'affichage des informations en lecture

! -- Declaration des sorties --
  type(rpmblock), pointer  :: firstblock    ! pointeur premier bloc RPM

! -- Declaration des variables internes --
  integer, parameter       :: dimbuf = 100  ! taille du buffer ligne
  integer                  :: lectstat      ! statut de la lecture
  integer                  :: ilig          ! numero de ligne
  integer                  :: nbloc         ! nombre de bloc
  integer                  :: posc          ! position de caractère
  logical                  :: inblock       ! bloc en cours de traitement
  character(len=dimrpmlig) :: str           ! chaîne courante
  character(len=dimrpmlig), dimension(:), allocatable &
                           :: buffer
  type(rpmblock), pointer  :: newblock, blockcourant
  type(rpmdata),  pointer  :: newdata,  datacourant
  
! -- Debut de la procedure --
  lectstat = 0
  ilig     = 1
  inblock  = .false.
  nullify(firstblock)
  allocate(buffer(dimbuf))
  
  do while ((lectstat == 0).and.(ilig <= dimbuf))
  
    read(unit=nio, fmt='(a)', iostat=lectstat) str
    if (lectstat == 0) then
      if (iaff >= 4) write(nerr,*) "RPM: lecture - ",ilig," : ",trim(str)
      buffer(ilig) = trait_rpmlig(str)
    else
      buffer(ilig) = ""
      if (iaff >= 2) write(nerr,*) "RPM: Fin de fichier"
    endif
    
    if (len_trim(buffer(ilig)) /= 0) then
    
      ! ----- test de début de bloc -----
      posc = index(buffer(ilig),':')
      if (samestring(buffer(ilig)(1:posc-1), 'BLOCK')) then
        if (ilig > 1) call rpmerr("Instructions inattendues&
                                  & avant définition de bloc")
        if (inblock) call rpmerr("Bloc précédent non terminé")

        ! le bloc est supposé valide
        inblock = .true.
        buffer(ilig) = adjustl(buffer(ilig)(posc+1:))
        call create_rpmblock(newblock, trim(buffer(ilig)))  ! alloc. et init.
        if (associated(firstblock)) then
          blockcourant%next => newblock  ! définition du lien de liste chaînée
        else
          firstblock => newblock         ! définition du premier bloc
        endif
        blockcourant => newblock         ! redéfinition du bloc courant
        if (iaff >= 2) write(nerr,*) "RPM: Lecture de bloc : ",trim(newblock%name)
        cycle                            ! on évite l'incrémentation de ilig
      endif
      
      ! ----- test de fin de bloc -----
      if (samestring(trim(buffer(ilig)), 'ENDBLOCK')) then
        if (.not.inblock) call rpmerr("Fin de bloc inattendue")
        call set_rpmblock(blockcourant, ilig-1, buffer(1:ilig-1))
          ! on ne compte pas la ligne ENDBLOCK
        ! ré-initialisation des données courantes
        ilig    = 1
        inblock = .false.
        cycle   ! on saute les traitements suivants
      endif
      
      ! ----- test de fin de bloc de données (incorrect) -----
      if (samestring(trim(buffer(ilig)), 'ENDDATA')) then
        call rpmerr("Fin de bloc de données inattendue")
      endif 
      
      !print*,'test test'
      ! ----- test de début de bloc de données -----
      posc = index(buffer(ilig),'=')
      if (samestring(buffer(ilig)(1:posc-1), 'DATA')) then
        ! lecture du bloc DATA
        !print*,"test entrée data"
        call readrpmdata(nio, newdata, buffer(ilig), iaff)  
        ! définition des liens de liste chaînée
        if (associated(blockcourant%data)) then
          datacourant%next => newdata
        else
          blockcourant%data => newdata
        endif
        datacourant => newdata
        if (iaff >= 2) write(nerr,*) "RPM: Lecture de bloc de données : ",&
                                     datacourant%nbvar," variables"
        cycle   ! on saute les traitements suivants
      endif ! on arrive directement à la ligne ENDDATA

      ! ----- on est alors dans une instruction interne de block -----
      ilig = ilig + 1
    endif
  enddo
  
  if (inblock) call rpmerr("Le bloc courant n'a pas été terminé")
  if (ilig > dimbuf) call rpmerr("Taille maximale du buffer de bloc dépassée")
  deallocate(buffer)
  
endsubroutine readrpmblock
!------------------------------------------------------------------------------!




!------------------------------------------------------------------------------!
! Procédure : readrpmdata                 Auteur : J. Gressier
!                                         Date   : Fevrier 2002
! Fonction                                Modif  :
!   Lecture d'un bloc de données commençant par DATA, terminé par ENDDATA,
!   renvoie le pointeur d'une structure RPMDATA
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine readrpmdata(nio, pdata, entete, iaff)
  use STRING
  implicit none 

! -- Declaration des entrées --
  integer           :: nio    ! n° d'unite pour la lecture des paramètres
  integer           :: iaff   ! choix d'affichage des actions
  character(len=*)  :: entete ! entete du bloc DATA, à traiter   

! -- Declaration des sorties --
  type(rpmdata), pointer :: pdata ! pointeur sur la structure DATA

! -- Declaration des variables internes --
  integer, parameter                :: dimbuffer = 200
  integer                           :: lectstat    ! statut de la lecture
  integer                           :: n, i, pos      
  real, dimension(:,:), allocatable :: buffer      ! buffer avant affectation
  character(len=dimrpmlig)          :: str         ! ligne intermédiaire
  logical                           :: fin         ! test

! -- Debut de la procedure --

  !print*,"cdata 1"
  call create_rpmdata(pdata)
  !print*,"cdata 2"
  
  ! Calcul du nombre de variables
  str = adjustl(entete(index(entete,'=')+1:))  ! Extraction de DATA=
  pdata%nbvar = numbchar(str,',') + 1
  if (iaff >= 4) write(6,*) "DATA variables : ",pdata%nbvar," trouvées"

  ! Extraction des noms de variables 
  allocate(pdata%name(pdata%nbvar))
  do n = 1, pdata%nbvar
    if (n == pdata%nbvar) then  ! dernière variable, à traiter sans virgule
      pdata%name(n) = trim(str)
    else
      pos = index(str,',')  
      pdata%name(n) = str(1:pos-1)  ! extraction du nom
      str = adjustl(str(pos+1:))    ! décalage
    endif
    if (len_trim(pdata%name(n)) == 0) call rpmerr("Nom de variable incorrect")
    ! vérification de non redondance des noms de variables
    do i = 1, n-1
      if (samestring(pdata%name(n),pdata%name(i))) &
        call rpmerr("Nom de variable redondant")
    enddo
  enddo
  if (iaff >= 4) write(6,*) "DATA variables : ",pdata%name

  ! Lecture des données
  allocate(buffer(pdata%nbvar, dimbuffer))
  n   = 0
  fin = .false.
  do while ((n < dimbuffer).and.(.not.fin))
   read(unit=nio, fmt='(a)', iostat=lectstat) str
   str = trait_rpmlig(str)
   if (lectstat /= 0) call rpmerr("Erreur de lecture de fichier inattendue")
   if (samestring(str,'ENDDATA')) then
     fin = .true.
   else
     n = n + 1
     read(str,*,iostat=lectstat) buffer(:,n)
     if (lectstat /= 0) call rpmerr("Données incohérentes")
   endif
  enddo
  pdata%nbpts = n
  allocate(pdata%tab(pdata%nbvar,pdata%nbpts))
  pdata%tab = buffer(:,1:pdata%nbpts)
  deallocate(buffer)
  
endsubroutine readrpmdata
!------------------------------------------------------------------------------!




!------------------------------------------------------------------------------!
! Fonction : trait_rpmlig                 Auteur : J. Gressier
!                                         Date   : Fevrier 2002
! Fonction                                Modif  :
!   Traitement d'une ligne d'un fichier RPM
!   . Suppression des espaces en début de chaine
!   . Suppression des chaines débutant par ! ou # (rpmcommentchar)
!   . Mise en caractères majuscule sauf chaîne entre "
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
character(len=len(strin)) function trait_rpmlig(strin)

  use STRING
  implicit none 

! -- Declaration des Parametres --
  character(len=dimrpmlig), intent(in)   :: strin

! -- Declaration des variables internes --
  character(len=len(strin)) :: s
  integer                   :: ipos 
  logical                   :: inquote

! -- Debut de la procedure --

  ! recherche de caractères de début de commentaire
  ipos = scan(strin, rpmcommentchar)
  if (ipos >= 1) then
    ! extraction de chaîne hors commentaires et mise en majuscules
    s = rpmuppercase(strin(1:ipos-1))
    ! suppression des espaces en début de chaîne
    s = adjustl(s)
  else
    ! mise en majuscules et suppression des espaces en début
    s = adjustl(rpmuppercase(strin))
  endif
  trait_rpmlig = s
  
contains
!------------------------------------------------------------------------------!
! mise en majuscule sauf chaîne entre "
!------------------------------------------------------------------------------!
function rpmuppercase(str)
implicit none
character(len=*)        :: str
character(len=len(str)) :: rpmuppercase
integer   :: i
logical   :: inquote
  
  rpmuppercase = str
  inquote      = .false.
  do i = 1, len(rpmuppercase)
    if (rpmuppercase(i:i) == rpmquotechar) inquote = .not.inquote
    if (.not.inquote) rpmuppercase(i:i) = uppercase(rpmuppercase(i:i))
  enddo
  ! Pas de test si il y a un (") fermant le premier (")

endfunction rpmuppercase

endfunction trait_rpmlig
!------------------------------------------------------------------------------!
