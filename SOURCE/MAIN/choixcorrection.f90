!------------------------------------------------------------------------------
! Procedure : choixcorrection             Auteur : E. Radenac
!                                         Date   : Fevrier 2004
! Fonction                                Modif  : 
!   Choix de la correction de flux
!
! Defauts/Limitations/Divers : pour l'instant, une methode de calcul commune 
!				aux deux zones
!------------------------------------------------------------------------------
 
subroutine choixcorrection(zone1, zone2, placement, corcoef, typ_cor, &
                           nfacelim, nbc1, nbc2, ncoupl2)

use OUTPUT
use VARCOM
use DEFZONE
use DEFFIELD
use GEO3D
use TYPHMAKE
use MATER_LOI

implicit none

! -- Declaration des entrees --
type(st_zone)              :: zone1, zone2
integer                    :: ncoupl2
integer                    :: nfacelim
integer                    :: typ_cor
integer                    :: nbc1, nbc2

! -- Declaration des sorties --
integer                        :: placement     ! variable pour le
                                                ! placement des corrections
real(krp), dimension(nfacelim) :: corcoef       ! coefficient de correction

! -- Declaration des variables internes --
integer                    :: i, if1, if2, icl1, icl2
real(krp)                  :: vol1, vol2, c1, c2, conduct1, conduct2, rapdifth

! -- Debut de la procedure --

  ! Calcul du nombre de Fourier de cycle (base sur la duree entre deux 
  ! echanges 
  ! et sur le pas de maillage)
  !call calc_fouriercycle(zone1, zone1%deftime%stabnb, dtexch, fcycle1)
  !call calc_fouriercycle(zone2, zone2%deftime%stabnb, dtexch, fcycle2)
  !DEBUG
  !print*, "Fourier de cycle : ", fcycle1, ", ", fcycle2

! Type de correction
select case(typ_cor)
case (sans)
  placement = sans
case (bocoT)
  placement = avant
case (bocoT2) ! DEV1603
  placement = apres
case (avant)
  placement = avant
case (apres)
  placement = apres
case (auto) ! Choix automatique selon parametres
  placement = apres
case (repart_reg)
  placement = apres
case (repart_geo)
  placement = apres
case (partiel)
  placement = avant
endselect

! Coefficient de correction
do i=1, nfacelim
  if1 = zone1%grid%umesh%boco(nbc1)%iface(i)
  if2 = zone2%grid%umesh%boco(nbc2)%iface(zone2%coupling(ncoupl2)%zcoupling%connface(i))

  icl1 = zone1%grid%umesh%facecell%fils(if1,1)
  icl2 = zone2%grid%umesh%facecell%fils(if2,1)

  ! Calcul de conductivite de la zone 1
  select case(zone1%defsolver%defkdif%materiau%type)
  case(mat_LIN, mat_KNL)
    conduct1 = valeur_loi(zone1%defsolver%defkdif%materiau%Kd, &
                          zone1%grid%field_loc%etatprim%tabscal(1)%scal(icl1))
  case(mat_XMAT)
    call erreur("Calcul de materiau","Materiau non lineaire interdit")
  endselect

  ! Calcul de conductivite de la zone 2
  select case(zone2%defsolver%defkdif%materiau%type)
  case(mat_LIN, mat_KNL)
    conduct2 = valeur_loi(zone2%defsolver%defkdif%materiau%Kd, &
                          zone2%grid%field_loc%etatprim%tabscal(1)%scal(icl2))
  case(mat_XMAT)
    call erreur("Calcul de materiau","Materiau non lineaire interdit")
  endselect

  ! Volumes des cellules limitrophes
  vol1 = zone1%grid%umesh%mesh%volume(icl1,1,1)
  vol2 = zone2%grid%umesh%mesh%volume(icl2,1,1)

  ! Capacites thermiques
  c1 = zone1%defsolver%defkdif%materiau%Cp
  c2 = zone2%defsolver%defkdif%materiau%Cp

  ! Rapport des diffusivites thermiques (avec prise en compte volumes cellules)
  rapdifth = (conduct2*c1*vol1) / (conduct1*c2*vol2)

  ! Orientation des valeurs de coefficient de correction
  if (rapdifth == 1)   corcoef(i) = 0.5
  if (rapdifth .gt. 1) corcoef(i) = 1
  if (rapdifth .lt. 1) corcoef(i) = 0
!print*, "DEBUG", rapdifth, corcoef(i)
enddo
endsubroutine choixcorrection

!------------------------------------------------------------------------------
! Historique des modifications
!
! fevrier 2003 : creation de la procedure
! oct  2004 : field chained list
!
!------------------------------------------------------------------------------
