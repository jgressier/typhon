!------------------------------------------------------------------------------!
! Procedure : write_monitors                    Auteur : J. Gressier
!                                         Date   : Mai 2003
! Fonction                                Modif  : (cf historique)
!   Calcul des quantites definis par les write_monitors
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine write_monitors(zone)

use TYPHMAKE
use OUTPUT
use VARCOM
use DEFZONE

implicit none

! -- Declaration des entrees --
type(st_zone) :: zone            ! zone 

! -- Declaration des sorties --

! -- Declaration des variables internes --
integer    :: ic                 ! index de capteur

! -- Debut de la procedure --

do ic = 1, zone%defsolver%nprobe

  select case(zone%defsolver%probe(ic)%type)
  case(probe)
    call erreur("Developpement","type PROBE non implemente")
  case(boco_field)
    call prb_boco_field(zone)
  case(boco_integral)
    call erreur("Developpement","type BOCO_INTEGRAL non implemente")
  case(residuals)

  endselect

enddo

write(uf_monres,*) zone%info%iter_tot, log10(zone%info%cur_res)

!-----------------------------
endsubroutine write_monitors

!------------------------------------------------------------------------------!
! Changes history
!
! mai 2003 : creation de la procedure (test pour debuggage)
! nov 2003 : redirection selon type de capteur
! Apr 2008: change name into write_monitors.f90
!------------------------------------------------------------------------------!
