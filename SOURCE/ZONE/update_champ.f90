!------------------------------------------------------------------------------!
! Procedure : update_champ                Auteur : J. Gressier
!                                         Date   : Mai 2003
! Fonction                                Modif  : cf historique
!   Mise à jour des champs conservatifs dans les équations de bilan
!   Calcul éventuel d'informations
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine update_champ(info, field, ncell)

use TYPHMAKE
use MODINFO
use VARCOM
use OUTPUT
use DEFFIELD

implicit none

! -- Declaration des entrées --
integer :: ncell           ! nombre de cellules à mettre à jour

! -- Declaration des entrées/sorties --
type(st_infozone) :: info                 ! champ d'état et de gradients
type(st_field)    :: field                ! champ d'état et de gradients

! -- Declaration des variables internes --
integer :: nc, ip, ic

! -- Debut de la procedure --

nc = ncell

do ip = 1, field%nscal
  field%etatcons%tabscal(ip)%scal(1:nc) = field%etatcons%tabscal(ip)%scal(1:nc) &
                                        + field%residu%tabscal(ip)%scal(1:nc)   
enddo

do ip = 1, field%nvect
  do ic = 1, nc
    field%etatcons%tabvect(ip)%vect(ic) = field%etatcons%tabvect(ip)%vect(ic) &
                                        + field%residu%tabvect(ip)%vect(ic)  
  enddo
enddo

! -- Calcul de résidus dans le cas pseudo-stationnaire

if (info%typ_temps == stationnaire) then

  info%cur_res = 0._krp

  do ip = 1, field%nscal
    info%cur_res = info%cur_res + sum(abs(field%residu%tabscal(ip)%scal(1:nc)))
  enddo

  do ip = 1, field%nvect
    do ic = 1, nc
      ! ATTENTION : le résidu est calculé sur la norme du vecteur, non ses composantes
      info%cur_res = info%cur_res + abs(field%residu%tabvect(ip)%vect(ic))
    enddo
  enddo
  
endif

endsubroutine update_champ

!------------------------------------------------------------------------------!
! Historique des modifications
!
! mai  2003 : création de la procédure
! sept 2003 : calcul des résidus
!------------------------------------------------------------------------------!
