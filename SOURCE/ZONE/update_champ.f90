!------------------------------------------------------------------------------!
! Procedure : update_champ                Auteur : J. Gressier
!                                         Date   : Mai 2003
! Fonction                                Modif  : Juin 2003
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine update_champ(field)

use TYPHMAKE
use VARCOM
use OUTPUT
use DEFFIELD

implicit none

! -- Declaration des entrées --

! -- Declaration des entrées/sorties --
type(st_field)   :: field                ! champ d'état et de gradients

! -- Declaration des variables internes --
integer :: nc, ip, ic

! -- Debut de la procedure --
nc = field%ncell

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

endsubroutine update_champ

!------------------------------------------------------------------------------!
! Historique des modifications
!
! mai 2003 (v0.0.1b): création de la procédure
!------------------------------------------------------------------------------!
