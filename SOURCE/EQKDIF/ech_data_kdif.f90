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
                         donnees_echange_inst2, zone2, nbc2, ncoupl1, &
                         ncoupl2, typcor )

use TYPHMAKE
use OUTPUT
use GEO3D
use DEFZONE
use DEFFIELD
use MATER_LOI
use VARCOM

implicit none

! -- Declaration des entrées --
type(st_zone)           :: zone1, zone2
integer                 :: nbc1, nbc2 ! indice de la condition aux limites
integer                 :: ncoupl1, ncoupl2
integer                 :: typcor

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
                         zone1%grid%field_loc%etatprim%tabscal(1)%scal(icl1))
  case(mat_XMAT)
    call erreur("Calcul de matériau","Materiau non linéaire interdit")
  endselect

  ! Affectation des données d'échange de la zone 1

  if (typcor == bocoT) then
!DEBUG
print*, "correction BOCO"
    donnees_echange_inst1%tabscal(1)%scal(i) = &
             (zone1%grid%field_loc%etatcons%tabscal(1)%scal(icl1) - &
             (zone1%coupling(ncoupl1)%zcoupling%etatcons%tabscal(2)%scal(i) / &
             zone1%grid%umesh%mesh%volume(icl1,1,1)) ) / &
             zone1%defsolver%defkdif%materiau%Cp
    ! reste de correction nul
    ! zone1%coupling(ncoupl1)%zcoupling%etatcons%tabscal(3)%scal(i) = 0
  else
    !donnees_echange_inst1%tabscal(1)%scal(i) = &
    !                              zone1%grid%field%etatprim%tabscal(1)%scal(icl1)
    donnees_echange_inst1%tabscal(1)%scal(i) = &
             zone1%grid%field_loc%etatcons%tabscal(1)%scal(icl1)/ &
             zone1%defsolver%defkdif%materiau%Cp
  endif

  donnees_echange_inst1%tabscal(2)%scal(i) = conduct
  !donnees_echange_inst1%tabvect(1)%vect(if) = zone1%grid%field%gradient%tabvect(1)%vect(icl)

  ! Calcul de conductivité de la zone 2
  select case(zone2%defsolver%defkdif%materiau%type)
  case(mat_LIN, mat_KNL)
    conduct = valeur_loi(zone2%defsolver%defkdif%materiau%Kd, &
                         zone2%grid%field_loc%etatprim%tabscal(1)%scal(icl2))
  case(mat_XMAT)
    call erreur("Calcul de matériau","Materiau non linéaire interdit")
  endselect

  ! Affectation des données d'échange de la zone 2

  if (typcor == bocoT) then
    donnees_echange_inst2%tabscal(1)%scal(i) = &
             (zone2%grid%field_loc%etatcons%tabscal(1)%scal(icl2) - &
             (zone2%coupling(ncoupl2)%zcoupling%etatcons%tabscal(2)%scal(i) / &
             zone2%grid%umesh%mesh%volume(icl2,1,1)) ) / &
             zone2%defsolver%defkdif%materiau%Cp
    ! reste de correction nul
    ! zone2%coupling(ncoupl2)%zcoupling%etatcons%tabscal(3)%scal(i) = 0
  else
    !donnees_echange_inst2%tabscal(1)%scal(i) = &
    !                              zone2%grid%field%etatprim%tabscal(1)%scal(icl2)
    donnees_echange_inst2%tabscal(1)%scal(i) = &
             zone2%grid%field_loc%etatcons%tabscal(1)%scal(icl2)/ &
             zone2%defsolver%defkdif%materiau%Cp
  endif

  donnees_echange_inst2%tabscal(2)%scal(i) = conduct
  !donnees_echange_inst2%tabvect(1)%vect(if) = zone2%grid%field%gradient%tabvect(1)%vect(icl)  

enddo

endsubroutine ech_data_kdif

!------------------------------------------------------------------------------!
! Historique des modifications
!
! juin 2003 (v0.0.1b): création de la procédure
! juillet 2003       : conductivité non constante
! oct  2004          : field chained list
!------------------------------------------------------------------------------!
