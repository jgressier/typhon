!------------------------------------------------------------------------------!
! Procedure : capteurs                    Auteur : J. Gressier
!                                         Date   : Mai 2003
! Fonction                                Modif  :
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
real(krp)     :: dt              ! pas de temps propre à la zone
type(st_zone) :: zone            ! zone à intégrer

! -- Declaration des sorties --

! -- Declaration des variables internes --

! -- Debut de la procedure --


! -- intégration des domaines --

select case(zone%typ_mesh)
 
case(mshSTR)
  call erreur("Dévéloppement (capteurs)", &
              "maillage structuré non implémenté")
  !do i = 1, zone%str_mesh%nblock
  !  call capteurs_str(dt, zone%defsolver, zone%str_mesh%block(i))
  !enddo

case(mshUST)
  !call erreur("Développement (capteurs)", &
  !            "maillage non structuré non implémenté")
  !call capteurs_ust(dt, zone%defsolver, zone%ust_mesh, zone%field)
  write(*,"(a,5f10.1)")"capteur : ", &
                       zone%field(1)%etatprim%tabscal(1)%scal(1:10:2) !! DEBUG

case default
  call erreur("incohérence interne (capteurs)", &
              "type de maillage inconnu")

endselect

!-----------------------------
endsubroutine capteurs

!------------------------------------------------------------------------------!
! Historique des modifications
!
! mai 2003 (v0.0.1b): création de la procédure
!------------------------------------------------------------------------------!
