!------------------------------------------------------------------------------!
! Procedure : echange_kdif           	  Auteur : E. Radenac
!                                         Date   : Mai 2003
! Fonction                                Modif  : Juin 2003
!   Echange de donnees entre zones de calcul, pour deux materiaux couples
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!
subroutine echange_kdif(echdata1, echdata2, normale, vecinter, d1, d2, nfacelim, &
			typecalcul, typemethode, bocokdif1, bocokdif2, connface2)

use TYPHMAKE
use OUTPUT
use GEO3D
use DEFFIELD
use MENU_KDIF

implicit none

! -- Declaration des entrees --
type(st_genericfield)      :: echdata1, echdata2
integer                    :: nfacelim   ! nombre de faces limites sur l'interface
type(v3d), dimension(nfacelim) &
                           :: vecinter                ! vecteur unitaire "intercellules"                       
type(v3d), dimension(nfacelim) &
                           :: normale ! normales a l'interface
real(krp), dimension(nfacelim) &
		           :: d1, d2  ! distance entre les centres des cellules gauche,
		      		      ! droite et l'interface
integer                    :: typecalcul, typemethode
integer, dimension(nfacelim) &
                           :: connface2

! -- Declaration des entrees/sorties --
type(st_boco_kdif)         :: bocokdif1, bocokdif2

! -- Declaration des variables internes --
integer                  :: if
real(krp)                :: temp1, temp2   ! temperatures a echanger
type(v3d)                :: gradtemp1, gradtemp2    ! gradients de temperature a echanger
real(krp)                :: conduct1, conduct2 ! conductivites a echanger
type(v3d)                :: flux1, flux2 ! densite de flux a echanger                         

real(krp)                :: temp_inter
real(krp)                :: flux_inter

! -- Debut de la procedure --

! boucle sur les faces de l'interface entre les deux zones.

do if = 1, nfacelim

  ! Donnees instationnaires
   temp1 = echdata1%tabscal(1)%scal(if)
   temp2 = echdata2%tabscal(1)%scal(if)
   gradtemp1 = echdata1%tabvect(1)%vect(if)
   gradtemp2 = echdata2%tabvect(1)%vect(if)
   conduct1 = echdata1%tabscal(2)%scal(if)
   conduct2 = echdata2%tabscal(2)%scal(if)
   flux1 = - conduct1 * gradtemp1
   flux2 = - conduct2 * gradtemp2 

  !Appel aux sous-routines de calcul de flux et temperature a l'interface.
  !flux_inter = 0
  call calc_fluxinter_kdif(temp1, temp2, gradtemp1, gradtemp2, conduct1, conduct2,& 
  			   d1(if), d2(if), vecinter(if), flux1, flux2, normale(if), &
                           flux_inter, typecalcul, typemethode)

  call calc_tempinter_kdif(temp1, temp2, conduct1, conduct2, d1(if), d2(if), &
  					temp_inter)
print*, "BOG TEMP INTER", temp_inter
  !Conditions aux limites des deux zones
  call stock_kdif_cond_coupling(bocokdif1, temp_inter, flux_inter, if, temp2)

  call stock_kdif_cond_coupling(bocokdif2, temp_inter, -flux_inter, connface2(if), temp1) 

enddo

endsubroutine echange_kdif
