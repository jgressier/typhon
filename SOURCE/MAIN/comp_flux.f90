!------------------------------------------------------------------------------!
! Procedure : comp_flux                   Auteur : E. Radenac
!                                         Date   : Juin 2003
! Fonction                                Modif  :
!   Comparaison des flux de part et d'autre d'une interface de deux zones
!
! Defauts/Limitations/Divers :
!
!------------------------------------------------------------------------------!

subroutine comp_flux(zone1, zone2, nbc1, nbc2, nfacelim, curtps, ncoupl)

use DEFZONE
use OUTPUT
use TYPHMAKE
use GEO3D
use VARCOM

implicit none

! -- Declaration des entrées --
type(st_zone)    :: zone1, zone2
integer          :: nbc1, nbc2
integer          :: nfacelim
real(krp)        :: curtps
integer          :: ncoupl

! -- Declaration des sorties --

! -- Declaration des variables internes --
real(krp), dimension(nfacelim)  :: d1, d2
real(krp), dimension(nfacelim)  :: conduct1, conduct2
real(krp), dimension(nfacelim)  :: flux1, flux2
real(krp), dimension(nfacelim)  :: temp1, temp2, tempinter
real(krp)                       :: dflux, dfluxcalc

integer                         :: i, if1, if2, ic1, ic2, icg1
type(v3d)                       :: cg1, cg2, cgface
type(v3d)                       :: vecinter

! -- Debut de la procedure --

do i=1, nfacelim    
  
  ! indices des faces concernées
  if1 = zone1%ust_mesh%boco(nbc1)%iface(i)
  if2 = zone2%ust_mesh%boco(nbc2)%iface(i)
   
  cgface = zone1%ust_mesh%mesh%iface(if1,1,1)%centre
  ic1 = zone1%ust_mesh%facecell%fils(if1,1)
  icg1 = zone1%ust_mesh%facecell%fils(if1,2)
  cg1 = zone1%ust_mesh%mesh%centre(ic1,1,1)
  ic2 = zone2%ust_mesh%facecell%fils(if2,1)
  cg2 = zone2%ust_mesh%mesh%centre(ic2,1,1)

  ! calcul du vecteur unitaire "inter-cellules"
  vecinter = (cg2 - cg1) / abs((cg2 - cg1))
  
  ! Calcul des distances d1 et d2 entre les cellules (des zones 1 et 2) et l'interface.
  d1(i) = (cgface-cg1).scal.vecinter
  d2(i) = (cg2-cgface).scal.vecinter

  ! Températures

  temp1(i) = zone1%field(1)%etatprim%tabscal(1)%scal(ic1)
  temp2(i) = zone2%field(1)%etatprim%tabscal(1)%scal(ic2)
  tempinter(i) = zone1%field(1)%etatprim%tabscal(1)%scal(icg1)
  
enddo 

! Conductivités :

!--DVT--------------------------------------------------------------
do i=1, nfacelim
  select case(zone1%defsolver%defkdif%materiau%type)
  case(mat_LIN, mat_KNL)
    conduct1(i) = valeur_loi(zone1%defsolver%defkdif%materiau%Kd, temp1(i))
  case(mat_XMAT)
    call erreur("Calcul de matériau","Materiau non linéaire interdit")
  endselect

  select case(zone2%defsolver%defkdif%materiau%type)
  case(mat_LIN, mat_KNL)
    conduct2(i) = valeur_loi(zone2%defsolver%defkdif%materiau%Kd, temp2(i))
  case(mat_XMAT)
    call erreur("Calcul de matériau","Materiau non linéaire interdit")
  endselect 
enddo
!-------------------------------------------------------------------

!select case(zone1%defsolver%defkdif%materiau%type)
!case(mat_LIN)
!  conduct1(:) = zone1%defsolver%defkdif%materiau%Kd%valeur
!case(mat_KNL, mat_XMAT)
!  call erreur("Calcul de matériau","Materiau non linéaire interdit")
!endselect
!
!select case(zone2%defsolver%defkdif%materiau%type)
!case(mat_LIN)
!  conduct2(:) = zone2%defsolver%defkdif%materiau%Kd%valeur
!case(mat_KNL, mat_XMAT)
!  call erreur("Calcul de matériau","Materiau non linéaire interdit")
!endselect

! Flux :
flux1(:) = - conduct1(:) * ( tempinter(:) - temp1(:) ) / d1(:)
flux2(:) = - conduct2(:) * ( temp2(:) - tempinter(:) ) / d2(:)


! Ecriture d'un fichier TECPLOT :

do i = 1, nfacelim
  if (abs(flux1(i)) == 0._krp) then
    if (abs(flux2(i)) == 0._krp) then
      dflux = 0._krp
    else
      dflux = 1._krp
    endif
  else
    dflux = abs( abs(flux2(i)) - abs(flux1(i)) ) / abs(flux1(i))
  endif

  if (abs(flux1(i)) == 0._krp) then
    if (zone1%coupling(1)%zcoupling%cond_coupling%tabscal(2)%scal(i) == 0._krp) then
      dfluxcalc = 0._krp
    else
      dfluxcalc = 1._krp
    endif
  else
    dfluxcalc = abs( zone1%coupling(1)%zcoupling%cond_coupling%tabscal(2)%scal(i) - &
                     abs(flux1(i)) )/ abs(flux1(i))
  endif

  write(uf_compflux,'(6e18.8)') curtps, abs(flux1(i)), abs(flux2(i)), dflux, &
            zone1%coupling(1)%zcoupling%cond_coupling%tabscal(2)%scal(i), &
            dfluxcalc 
                  
enddo

endsubroutine comp_flux
