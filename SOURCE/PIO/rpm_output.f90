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
! Procédure : printrpmblock               Auteur : J. Gressier
!                                         Date   : Fevrier 2002
! Fonction                                Modif  :
!   Ecrit sur le numéro d'unité spécifié (iu) le contenu du bloc spécifié 
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine printrpmblock(iu, block, debug)
  use RPM
  implicit none 

! -- Declaration des entrées --
  integer        :: iu      ! numero d'unite pour l'écriture
  type(rpmblock) :: block   ! bloc à écrire
  logical        :: debug

! -- Declaration des variables internes --
  integer i

! -- Debut de la procedure --

  write(iu,*) 'BLOCK: ',block%name
  do i = 1, block%nblig
    if (debug) then
      write(iu,*) i,':  ',trim(block%txt(i))
    else
      write(iu,*) '  ',trim(block%txt(i))
    endif
  enddo

  if (debug) then
    write(iu,*) 'ENDBLOCK ! fin réelle de bloc'
  else
    write(iu,*) 'ENDBLOCK'
  endif
  
endsubroutine printrpmblock
!------------------------------------------------------------------------------!



!------------------------------------------------------------------------------!
! Procédure : printrpm_unread             Auteur : J. Gressier
!                                         Date   : Fevrier 2002
! Fonction                                Modif  :
!   Ecrit sur le numéro d'unité spécifié (iu) les données non lues
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine printrpm_unread(iu, block)
  use RPM
  implicit none 

! -- Declaration des entrées --
  integer iu                ! numero d'unite pour l'écriture
  type(rpmblock) :: block   ! bloc à tester

! -- Declaration des variables internes --
  integer i

! -- Debut de la procedure --

  if (.not.block%flagblock) then
    write(iu,*) " Attention : bloc RPM ",trim(block%name)," non traité"
    call printrpmblock(iu, block)
  else
    do i = 1, block%nblig                   ! Ecriture des lignes
      if (.not.block%flagtxt(i)) &
        write(iu,*) "Attention : ligne du bloc RPM ",trim(block%name),&
                    "non traité : ",trim(block%txt(i))
    enddo
  endif
  
endsubroutine printrpm_unread
!------------------------------------------------------------------------------!


