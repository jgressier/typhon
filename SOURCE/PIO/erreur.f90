!------------------------------------------------------------------------------!
! Procedure : erreur                      Auteur : J. Gressier
!                                         Date   : Juillet 2002
! Fonction                                Modif  : 
!   Affichage d'une erreur et arrêt du programme
!   Ecriture sur unité iout et fichier log 
!
!------------------------------------------------------------------------------!
subroutine erreur(str1, str2)
use OUTPUT
implicit none

! -- Declaration des entrées --
integer          iout            ! numero d'unité pour les erreurs
character(len=*) str1            ! chaîne 1
character(len=*) str2            ! chaîne 2

! -- Debut de la procedure --

write(uf_stdout,'(aaaaa)') "!!! Erreur ",trim(str1)," : ",trim(str2)," !!!"
write(uf_log,'(a)')    "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
write(uf_log,'(aaaa)')    "[STOP] Erreur ",trim(str1)," : ",trim(str2)

stop

endsubroutine erreur

