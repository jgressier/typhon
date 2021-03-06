!------------------------------------------------------------------------------!
! Procedure : create_rpmblock             Auteur : J. Gressier
!                                         Date   : Fevrier 2002
! Fonction                                Modif  :
!   Cr�ation, allocation et initialisation minimale d'un bloc RPM
!
!------------------------------------------------------------------------------!
subroutine create_rpmblock(pblock, name)
  implicit none 
  
! -- Declaration des Parametres --
  type(rpmblock), pointer :: pblock
  character(len=*)        :: name

! -- Debut de la procedure --
  allocate(pblock)
  pblock%name = name
  nullify(pblock%next)          ! IMPORTANT pour d�tecter la fin de la liste
  nullify(pblock%data)          ! IMPORTANT pour d�tecter l'existence de blocs DATA
  pblock%flagblock = .false.    ! Initialisation � "non lu"

endsubroutine create_rpmblock
!------------------------------------------------------------------------------!


!------------------------------------------------------------------------------!
! Proc�dure : set_rpmblock                Auteur : J. Gressier
!                                         Date   : Mai 2002
! Fonction                                Modif  :
!   Affectation du contenu d'un bloc RPM
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine set_rpmblock(block, n, txt)
implicit none 
! -- Declaration des entr�es --
type(rpmblock)                         :: block    ! bloc � initialiser
integer                                :: n        ! nombre de ligne du bloc texte
character(len=dimrpmlig), dimension(n) :: txt      ! contenu du texte

! -- Declaration des sorties --
! par structure de bloc

! -- Declaration des variables internes --

! -- Debut de la procedure --

block%nblig = n  
if (n == 0) call rpmerr("Bloc de taille nulle")
allocate(block%txt(n))         ! Allocation
block%txt(1:n) = txt(1:n)      ! et affectation des lignes
allocate(block%flagtxt(n))     ! Allocation
block%flagtxt(1:n) = .false.   ! et initialisation des lignes � "non lu"
  
endsubroutine set_rpmblock
!------------------------------------------------------------------------------!



!------------------------------------------------------------------------------!
! Procedure : create_rpmdata               Auteur : J. Gressier
!                                         Date   : Fevrier 2002
! Fonction                                Modif  :
!   Cr�ation, allocation et initialisation minimale d'un bloc DATA
!
!------------------------------------------------------------------------------!
subroutine create_rpmdata(pdata)
  implicit none 
  
! -- Declaration des Parametres --
  type(rpmdata), pointer :: pdata

! -- Debut de la procedure --
  allocate(pdata)
  nullify(pdata%next)    ! IMPORTANT pour d�tecter la fin de la liste

endsubroutine create_rpmdata
!------------------------------------------------------------------------------!




!------------------------------------------------------------------------------!
! Proc�dure : dealloc_rpmblock            Auteur : J. Gressier
!                                         Date   : Fevrier 2002
! Fonction                                Modif  :
!   D�salloue la liste compl�te et le contenu � partir d'un pointeur donn�
!   D�salloue les structures RPMDATA point�es
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine dealloc_rpmblock(firstblock)
  implicit none 

! -- Declaration des Parametres --
  type(rpmblock), pointer :: firstblock

! -- Declaration des variables internes --
  type(rpmblock), pointer :: block
  type(rpmdata),  pointer :: firstdata

! -- Debut de la procedure --
  do while (associated(firstblock))
    block      => firstblock
    firstblock => block%next
    deallocate(block%txt)
    firstdata => block%data
    do while (associated(firstdata))
      deallocate(firstdata%name)
      deallocate(firstdata%tab)
      firstdata => firstdata%next
    enddo
    deallocate(block)
  enddo
  
endsubroutine dealloc_rpmblock
!------------------------------------------------------------------------------!
