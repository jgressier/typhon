!------------------------------------------------------------------------------!
! Procedure : ech_data_kdif               Auteur : E. Radenac
!                                         Date   : Juin 2003
! Fonction                                Modif  : Juillet 2003
!   Ecrire les données variables dans le temps dans une structure 
!   donnees_echange_inst, pour la diffusion de la chaleur
! Defauts/Limitations/Divers :
!   Limitation au cas un domaine unique par zone
!------------------------------------------------------------------------------!

subroutine ech_data_kdif(donnees_echange_inst1, zone1, nbc1, &
                         donnees_echange_inst2, zone2, nbc2, ncoupl2 )

use TYPHMAKE
use OUTPUT
use GEO3D
use DEFZONE
use DEFFIELD
use MATER_LOI

implicit none

! -- Declaration des entrées --
type(st_zone)           :: zone1, zone2
integer                 :: nbc1, nbc2 ! indice de la condition aux limites
integer                 :: ncoupl2

! -- Declaration donnees_echange    

type(st_genericfield) :: donnees_echange_inst1,  donnees_echange_inst2

! -- Declaration des variables internes --
integer   :: i, icl1, icl2, if1, if2
real(krp) :: conduct

! -- Debut de la procedure --

do i=1, zone1%grid%umesh%boco(nbc1)%nface
  
  if1 = zone1%grid%umesh%boco(nbc1)%iface(i)
  if2 = zone2%grid%umesh%boco(nbc2)%iface(zone2%coupling(ncoupl2)%zcoupling%connface(i))

  icl1 = zone1%grid%umesh%facecell%fils(if1,1)
  icl2 = zone2%grid%umesh%facecell%fils(if2,1)

  ! Calcul de conductivité de la zone 1
  select case(zone1%defsolver%defkdif%materiau%type)
  case(mat_LIN, mat_KNL)
    conduct = valeur_loi(zone1%defsolver%defkdif%materiau%Kd, &
                         zone1%grid%field%etatprim%tabscal(1)%scal(icl1))
  case(mat_XMAT)
    call erreur("Calcul de matériau","Materiau non linéaire interdit")
  endselect

  ! Affectation des données d'échange de la zone 1
  donnees_echange_inst1%tabscal(1)%scal(i) = zone1%grid%field%etatprim%tabscal(1)%scal(icl1)
  donnees_echange_inst1%tabscal(2)%scal(i) = conduct
  !donnees_echange_inst1%tabvect(1)%vect(if) = zone1%grid%field%gradient%tabvect(1)%vect(icl)

  ! Calcul de conductivité de la zone 2
  select case(zone2%defsolver%defkdif%materiau%type)
  case(mat_LIN, mat_KNL)
    conduct = valeur_loi(zone2%defsolver%defkdif%materiau%Kd, &
                         zone2%grid%field%etatprim%tabscal(1)%scal(icl2))
  case(mat_XMAT)
    call erreur("Calcul de matériau","Materiau non linéaire interdit")
  endselect

  ! Affectation des données d'échange de la zone 2
  donnees_echange_inst2%tabscal(1)%scal(i) = zone2%grid%field%etatprim%tabscal(1)%scal(icl2)
  donnees_echange_inst2%tabscal(2)%scal(i) = conduct
  !donnees_echange_inst2%tabvect(1)%vect(if) = zone2%grid%field%gradient%tabvect(1)%vect(icl)  

enddo

endsubroutine ech_data_kdif

!------------------------------------------------------------------------------!
! Historique des modifications
!
! juin 2003 (v0.0.1b): création de la procédure
! juillet 2003       : conductivité non constante
!------------------------------------------------------------------------------!
