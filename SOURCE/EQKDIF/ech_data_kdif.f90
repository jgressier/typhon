!------------------------------------------------------------------------------!
! Procedure : ech_data_kdif               Auteur : E. Radenac
!                                         Date   : Juin 2003
! Fonction                                Modif  :
!   Ecrire les données variables dans le temps dans une structure 
!   donnees_echange_inst, pour la diffusion de la chaleur
! Defauts/Limitations/Divers :
!   Limitation au cas un domaine unique par zone
!------------------------------------------------------------------------------!

subroutine ech_data_kdif(donnees_echange_inst, zone, nbc)

use TYPHMAKE
use OUTPUT
use GEO3D
use DEFZONE
use DEFFIELD

implicit none

! -- Declaration des entrées --
type(st_zone)           :: zone
integer                 :: nbc ! indice de la condition aux limites

! -- Declaration donnees_echange    

type(st_genericfield) :: donnees_echange_inst

! -- Declaration des variables internes --
integer   :: i, icl, if
real(krp) :: conduct

! -- Debut de la procedure --

!select case(zone%defsolver%defkdif%materiau%type)
!case(mat_LIN, mat_KNL)
!  conduct = valeur_loi(zone%defsolver%defkdif%materiau%Kd, &
!                       zone%field(1)%etatprim%tabscal(1)%scal(icl))
!case(mat_XMAT)
!  call erreur("Calcul de matériau","Materiau non linéaire interdit")
!endselect

do i=1, zone%ust_mesh%boco(nbc)%nface
  
  if = zone%ust_mesh%boco(nbc)%iface(i)
  icl = zone%ust_mesh%facecell%fils(if,1)
!--DVT-----------------------------------------------------------------
select case(zone%defsolver%defkdif%materiau%type)
case(mat_LIN, mat_KNL)
  conduct = valeur_loi(zone%defsolver%defkdif%materiau%Kd, &
                       zone%field(1)%etatprim%tabscal(1)%scal(icl))
case(mat_XMAT)
  call erreur("Calcul de matériau","Materiau non linéaire interdit")
endselect
!----------------------------------------------------------------------
  donnees_echange_inst%tabscal(1)%scal(i) = zone%field(1)%etatprim%tabscal(1)%scal(icl)
  donnees_echange_inst%tabscal(2)%scal(i) = conduct
  !donnees_echange_inst%tabvect(1)%vect(if) = zone%field(1)%gradient%tabvect(1)%vect(icl)
  
enddo

endsubroutine ech_data_kdif
