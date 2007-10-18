!------------------------------------------------------------------------------!
! Procedure : erreur                                   Authors : J. Gressier
!                                                      Created : July 2002
! Fonction
!   Affichage d'une erreur et arret du programme
!   Ecriture sur unite iout et fichier log 
!
!------------------------------------------------------------------------------!
subroutine erreur(str1, str2)
use OUTPUT
implicit none

! -- Declaration des entrees --
integer          iout            ! numero d'unite pour les erreurs
character(len=*) str1            ! chaine 1
character(len=*) str2            ! chaine 2

! -- Debut de la procedure --

write(uf_stdout,'(a,a,a,a,a)') "!!! ",trim(str1)," : ",trim(str2)," !!!"
write(uf_log,'(a)')    "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
write(uf_log,'(a,a,a,a)')    "[STOP] ",trim(str1)," : ",trim(str2)

stop 1

endsubroutine erreur

