!------------------------------------------------------------------------------!
! Procedure : update_champ                Auteur : J. Gressier
!                                         Date   : Mai 2003
! Fonction                                Modif  : cf historique
!   Mise a jour des champs conservatifs dans les equations de bilan
!   Calcul eventuel d'informations
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

! -- Declaration des entrees --
integer :: ncell           ! nombre de cellules a mettre a jour

! -- Declaration des entrees/sorties --
type(st_infozone) :: info                 ! champ d'etat et de gradients
type(st_field)    :: field                ! champ d'etat et de gradients

! -- Declaration des variables internes --
integer :: nc, ip, ic

! -- BODY --

nc = ncell

!!$do ip = 1, field%nscal
!!$  field%etatcons%tabscal(ip)%scal(1:nc) = field%etatcons%tabscal(ip)%scal(1:nc) &
!!$                                        + field%residu%tabscal(ip)%scal(1:nc)   
!!$enddo
!!$
!!$do ip = 1, field%nvect
!!$  do ic = 1, nc
!!$    field%etatcons%tabvect(ip)%vect(ic) = field%etatcons%tabvect(ip)%vect(ic) &
!!$                                        + field%residu%tabvect(ip)%vect(ic)  
!!$  enddo
!!$enddo

call xeqxpy(field%etatcons, field%residu)

! -- Calcul de residus dans le cas pseudo-stationnaire

if (info%typ_temps == stationnaire) then

  info%cur_res = 0._krp

  do ip = 1, field%nscal
    info%cur_res = info%cur_res + sum(abs(field%residu%tabscal(ip)%scal(1:nc)))
  enddo

  do ip = 1, field%nvect
    do ic = 1, nc
      ! ATTENTION : le residu est calcule sur la norme du vecteur, non ses composantes
      info%cur_res = info%cur_res + abs(field%residu%tabvect(ip)%vect(ic))
    enddo
  enddo

  ! -- merge residual for all procs --

  call exchange_zonal_residual(info)
  
endif

endsubroutine update_champ

!------------------------------------------------------------------------------!
! Changes history
!
! may  2003 : created
! sept 2003 : Residual computation
! oct  2005 : merge residual for all procs
!------------------------------------------------------------------------------!
