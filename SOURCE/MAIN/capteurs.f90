!------------------------------------------------------------------------------!
! Procedure : capteurs                    Auteur : J. Gressier
!                                         Date   : Mai 2003
! Fonction                                Modif  : (cf historique)
!   Calcul des quantités définis par les capteurs
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine capteurs(zone)

use TYPHMAKE
use OUTPUT
use VARCOM
use DEFZONE

implicit none

! -- Declaration des entrées --
type(st_zone) :: zone            ! zone 

! -- Declaration des sorties --

! -- Declaration des variables internes --
integer    :: ic                 ! index de capteur

! -- Debut de la procedure --

do ic = 1, zone%defsolver%nprobe

  select case(zone%defsolver%probe(ic)%type)
  case(probe)
    call erreur("Développement","type PROBE non implémenté")
  case(boco_field)
    call prb_boco_field()
  case(boco_integral)
    call erreur("Développement","type BOCO_INTEGRAL non implémenté")
  case(residuals)

  endselect

enddo

write(uf_monres,*) zone%info%iter_tot, log10(zone%info%cur_res)

!-----------------------------
endsubroutine capteurs

!------------------------------------------------------------------------------!
! Historique des modifications
!
! mai 2003 : création de la procédure (test pour debuggage)
! nov 2003 : redirection selon type de capteur
!------------------------------------------------------------------------------!
