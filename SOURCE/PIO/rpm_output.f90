!------------------------------------------------------------------------------!
! Procedure : rpmerr                      Auteur : J. Gressier
!                                         Date   : Fevrier 2002
! Fonction                                Modif  :
!   Gestion des erreurs de la librairie RPM
!
!------------------------------------------------------------------------------!
subroutine rpmerr(message)
  implicit none 
! -- Declaration des Parametres --
  character(len=*) :: message
! -- Debut de la procedure --
  print*,'** librairie RPM - erreur : ' // message // ' **'
  stop
endsubroutine rpmerr
!------------------------------------------------------------------------------!


  

!------------------------------------------------------------------------------!
! Procedure : printrpmblock               Auteur : J. Gressier
!                                         Date   : Fevrier 2002
! Fonction                                Modif  :
!   Ecrit sur le numero d'unite specifie (iu) le contenu du bloc specifie 
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine printrpmblock(iu, block, debug)
  use RPM
  implicit none 

! -- Declaration des entrees --
  integer           :: iu      ! numero d'unite pour l'ecriture
  type(rpmblock)    :: block   ! bloc a ecrire
!  logical, optional :: debug 
  logical :: debug 

! -- Declaration des variables internes --
  integer i

! -- Debut de la procedure --

  !if (.not.present(debug)) debug = .false.

  write(iu,*) 'BLOCK: ',block%name
  do i = 1, block%nblig
    if (debug) then
      write(iu,*) i,':  ',trim(block%txt(i))
    else
      write(iu,*) '  ',trim(block%txt(i))
    endif
  enddo

  if (debug) then
    write(iu,*) 'ENDBLOCK ! fin reelle de bloc'
  else
    write(iu,*) 'ENDBLOCK'
  endif
  
endsubroutine printrpmblock
!------------------------------------------------------------------------------!



!------------------------------------------------------------------------------!
! Procedure : printrpm_unread             Auteur : J. Gressier
!                                         Date   : Fevrier 2002
! Fonction                                Modif  :
!   Ecrit sur le numero d'unite specifie (iu) les donnees non lues
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine printrpm_unread(iu, block)
  use RPM
  implicit none 

! -- Declaration des entrees --
  integer iu                ! numero d'unite pour l'ecriture
  type(rpmblock) :: block   ! bloc a tester

! -- Declaration des variables internes --
  integer i

! -- Debut de la procedure --

  if (.not.block%flagblock) then
    write(iu,*) " Attention : bloc RPM ",trim(block%name)," non traite"
    call printrpmblock(iu, block, .false.)
  else
    do i = 1, block%nblig                   ! Ecriture des lignes
      if (.not.block%flagtxt(i)) &
        write(iu,*) "Attention : ligne du bloc RPM ",trim(block%name),&
                    "non traite : ",trim(block%txt(i))
    enddo
  endif
  
endsubroutine printrpm_unread
!------------------------------------------------------------------------------!


